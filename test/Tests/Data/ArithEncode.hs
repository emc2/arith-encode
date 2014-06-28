-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# LANGUAGE FlexibleInstances #-}

module Tests.Data.ArithEncode(tests) where

import Control.Monad
import Data.ArithEncode
import Data.Char
import Data.Hashable
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Data.Set(Set)
import Prelude hiding (either)
import Test.HUnitPlus.Base

import qualified Data.Array as Array
import qualified Data.HashMap as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set

linearDepthEncoding :: (Hashable ty, Ord ty) => [ty] -> Encoding () ty
linearDepthEncoding elems =
  let
    len = length elems
    revmap = Array.listArray (0, len) elems
    fwdmap = HashMap.fromList (zip elems [0..len])
    encodefunc = toInteger . (HashMap.!) fwdmap
    decodefunc = (Array.!) revmap . fromInteger
    sizeval = Just (toInteger len)
    maxdepthfunc () = Just (toInteger (len - 1))
    depthfunc () = toInteger . (HashMap.!) fwdmap
    highindexfunc () = Just
    indomainfunc = (flip HashMap.member) fwdmap
  in
    mkEncoding encodefunc decodefunc sizeval indomainfunc
               maxdepthfunc depthfunc highindexfunc

testIsomorphism :: (Hashable ty, Ord ty, Show ty) =>
                   Encoding dim ty -> Integer -> IO ()
testIsomorphism iso limit =
  let
    foldfun previous num =
      let
        val = decode iso num
        num' = encode iso val
      in do
        num == num' @? "encode (decode " ++ show num ++ ") != " ++ show num'
        not (HashSet.member val previous) @?
          "decode " ++ show num ++ " produced duplicate value " ++ show val
        return (HashSet.insert val previous)
  in
    foldM_ foldfun HashSet.empty [0..limit]

testEncodingVals :: (Show ty, Eq ty) => Encoding dim ty -> [ty] -> IO ()
testEncodingVals iso vals =
  let
    foldfun previous val =
      let
        num = encode iso val
        val' = decode iso num
      in do
        num >= 0 @?
          "encode " ++ show val ++ " == " ++ show num ++ " is negative"
        maybe True (> num) (size iso) @?
          "encode " ++ show val ++ " == " ++ show num ++ " is too high"
        val == val' @? "encode (decode " ++ show val ++ ") != " ++ show val'
        not (HashSet.member num previous) @?
          "decode " ++ show num ++ " produced duplicate value " ++ show val
        return (HashSet.insert num previous)
  in
    foldM_ foldfun HashSet.empty vals

testFiniteEncoding :: (Hashable ty, Ord ty, Show ty) =>
                      [String] -> Encoding dim ty -> IO ()
testFiniteEncoding tags iso =
  let
    limit = fromJust (size iso)
  in
    testIsomorphism iso limit

testInfDimlessEncodingWithLimit tags iso limit = [
    testNameTags "isomorphism" ("isomorphism" : tags)
                 (testIsomorphism iso limit),
    testNameTags "bounds_low" ("bounds" : tags)
                 (assertThrows (\(IllegalArgument _) -> assertSuccess)
                               (return $! decode iso (-1))),
    testNameTags "size" ("size" : tags) (size iso @?= Nothing),
    testNameTags "depth" ("depth" : tags)
                 (mapM_ (\n -> depth iso () (decode iso n) @?= 0) [0..limit]),
    testNameTags "maxDepth" ("maxDepth" : tags) (maxDepth iso () @?= Just 0),
    testNameTags "highestIndex" ("highestIndex" : tags)
                 (highestIndex iso () 0 @?= Nothing)
  ]

testInfDimlessEncoding tags iso = testInfDimlessEncodingWithLimit tags iso 10000

testFiniteEncodingWithVals tags iso vals =
  let
    isosize = toInteger (length vals)
  in
    [ testNameTags "isomorphism" ("isomorphism" : tags)
                   (testEncodingVals iso vals),
      testNameTags "size" ("size" : tags)
                   (size iso @?= Just isosize),
      testNameTags "bounds_low" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "bounds_high" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (fromJust (size iso)))),
      testNameTags "depth" ("depth" : tags)
                   (mapM_ (\val -> depth iso () val @?= 0) vals),
      testNameTags "maxDepth" ("maxDepth" : tags) (maxDepth iso () @?= Just 0),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (highestIndex iso () 0 @?= Just isosize) ]

testLinearDepthEncoding tags iso vals =
  let
    isosize = toInteger (length vals)
    zipped = zip [0..isosize] vals

    testDepth [] = return ()
    testDepth ((depthval, val) : rest) =
      do
        depth iso () val @?= depthval
        testDepth rest

    testHighestIndex 0 = highestIndex iso () 0 @?= Just 0
    testHighestIndex depthval =
      let
        idx = case findIndex ((> depthval) . depth iso ()) vals of
          Just 0 -> 0
          Just idx -> toInteger (idx - 1)
          Nothing -> isosize - 1
      in do
        highestIndex iso () idx @?= Just idx
        testHighestIndex (depthval - 1)
  in
    [ testNameTags "isomorphism" ("isomorphism" : tags)
                   (testEncodingVals iso vals),
      testNameTags "size" ("size" : tags)
                   (size iso @?= Just isosize),
      testNameTags "bounds_low" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "bounds_high" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (fromJust (size iso)))),
      testNameTags "maxDepth" ("maxDepth" : tags)
                   (maxDepth iso () @?= Just (isosize - 1)),
      testNameTags "depth" ("depth" : tags) (testDepth zipped),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (testHighestIndex (isosize - 1)) ]

testExclude tags iso vals excludes =
  let
    isosize = toInteger (length vals - length excludes)
    filtered = filter ((flip notElem) excludes) vals
    zipped = zip [0..isosize] vals
    filtereddepths = filter ((flip notElem) excludes . snd) zipped

    testDepth [] = return ()
    testDepth ((depthval, val) : rest) =
      do
        depth iso () val @?= depthval
        testDepth rest

    testHighestIndex 0 =
      let
        idx = case findIndex ((> 0) . depth iso ()) filtered of
          Just 0 -> 0
          Just idx -> toInteger (idx - 1)
          Nothing -> isosize - 1
      in do
        highestIndex iso () 0 @?= Just idx
    testHighestIndex depthval =
      let
        idx = case findIndex ((> depthval) . depth iso ()) filtered of
          Just 0 -> 0
          Just idx -> toInteger (idx - 1)
          Nothing -> isosize - 1
      in do
        highestIndex iso () depthval @?= Just idx
        testHighestIndex (depthval - 1)
  in
    [ testNameTags "isomorphism" ("isomorphism" : tags)
                   (testEncodingVals iso filtered),
      testNameTags "size" ("size" : tags)
                   (size iso @?= Just isosize),
      testNameTags "bounds_low" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "bounds_high" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (fromJust (size iso)))),
      testNameTags "maxDepth" ("maxDepth" : tags)
                   (maxDepth iso () @?= Just (toInteger ((length vals) - 1))),
      testNameTags "depth" ("depth" : tags) (testDepth filtereddepths),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (testHighestIndex (isosize - 1)) ]

integralInteger :: Encoding () Integer
integralInteger = integral

integralInt64 :: Encoding () Int64
integralInt64 = integral

integralWord64 :: Encoding () Int64
integralWord64 = integral

integralInt8 :: Encoding () Int8
integralInt8 = integral

integralWord8 :: Encoding () Int8
integralWord8 = integral

intervalInteger :: Integer -> Integer -> Encoding () Integer
intervalInteger = interval

intervalInt64 :: Int64 -> Int64 -> Encoding () Int64
intervalInt64 = interval

intervalInt8 :: Int8 -> Int8 -> Encoding () Int8
intervalInt8 = interval

intervalWord64 :: Word64 -> Word64 -> Encoding () Word64
intervalWord64 = interval

intervalWord8 :: Word8 -> Word8 -> Encoding () Word8
intervalWord8 = interval

optionalEncoding = optional (fromHashableList ["A", "B", "C", "D"])

integralTests = [
    "Integer" ~:
      testInfDimlessEncoding ["integral", "Integer"] integralInteger,
    "Int64" ~:
      testInfDimlessEncoding ["integral", "Int64"] integralInt64,
    "Word64" ~:
      testInfDimlessEncoding ["integral", "Word64"] integralInt64,
    "Int8" ~:
      testInfDimlessEncodingWithLimit ["integral", "Int8"]
                                       integralInt8 255,
    "Word64" ~:
      testInfDimlessEncodingWithLimit ["integral", "Word8"]
                                       integralWord8 255
  ]

intervalTests = [
    "Integer" ~: [
        "0_10000" ~:
          testFiniteEncodingWithVals ["interval", "Integer"]
                                     (intervalInteger 0 10000)
                                     [0..10000],
        "2000_10000" ~:
          testFiniteEncodingWithVals ["interval", "Integer"]
                                     (intervalInteger 2000 10000)
                                     [2000..10000],
        "neg2000_2000" ~:
          testFiniteEncodingWithVals ["interval", "Integer"]
                                     (intervalInteger (-2000) 2000)
                                     [-2000..2000],
        "neg10000_neg2000" ~:
          testFiniteEncodingWithVals ["interval", "Integer"]
                                     (intervalInteger (-10000) (-2000))
                                     [-10000..(-2000)]
      ],
    "Int64" ~: [
        "0_10000" ~:
          testFiniteEncodingWithVals ["interval", "Int64"]
                                     (intervalInt64 0 10000) [0..10000],
        "2000_10000" ~:
          testFiniteEncodingWithVals ["interval", "Int64"]
                                     (intervalInt64 2000 10000)
                                     [2000..10000],
        "neg2000_2000" ~:
          testFiniteEncodingWithVals ["interval", "Int64"]
                                     (intervalInt64 (-2000) 2000)
                                     [-2000..2000],
        "neg10000_neg2000" ~:
          testFiniteEncodingWithVals ["interval", "Int64"]
                                     (intervalInt64 (-10000) (-2000))
                                     [-10000..(-2000)]
      ],
    "Word64" ~: [
        "0_10000" ~:
          testFiniteEncodingWithVals ["interval", "Word64"]
                                     (intervalWord64 0 10000) [0..10000],
        "2000_10000" ~:
          testFiniteEncodingWithVals ["interval", "Word64"]
                                     (intervalWord64 2000 10000)
                                     [2000..10000]
      ],
    "Int8" ~: [
        "0_100" ~:
          testFiniteEncodingWithVals ["interval", "Int8"]
                                     (intervalInt8 0 100) [0..100],
        "20_100" ~:
          testFiniteEncodingWithVals ["interval", "Int8"]
                                     (intervalInt8 20 100)
                                     [20..100],
        "neg20_20" ~:
          testFiniteEncodingWithVals ["interval", "Int8"]
                                     (intervalInt8 (-20) 20)
                                     [-20..20],
        "neg100_neg20" ~:
          testFiniteEncodingWithVals ["interval", "Int8"]
                                     (intervalInt8 (-100) (-20))
                                     [-100..(-20)],
        "neg128_127" ~:
          testFiniteEncodingWithVals ["interval", "Int8"]
                                     (intervalInt8 (-128) 127)
                                     [-128..127]
      ],
    "Word8" ~: [
        "0_100" ~:
          testFiniteEncodingWithVals ["interval", "Word8"]
                                     (intervalWord8 0 100) [0..100],
        "20_100" ~:
          testFiniteEncodingWithVals ["interval", "Word8"]
                                     (intervalWord8 20 100)
                                     [20..100],
        "0_255" ~:
          testFiniteEncodingWithVals ["interval", "Word8"]
                                     (intervalWord8 0 255)
                                     [0..255]
      ]
  ]

optionalEncodingTests = [
    testNameTags "isomorphism" ["isomorphism", "optional", "fromHashableList"]
                 (testEncodingVals optionalEncoding
                                   [Nothing, Just "A", Just "B",
                                    Just "C", Just "D"]),
    testNameTags "decode_zero" ["isomorphism", "optional"]
                 (decode optionalEncoding 0 @?= Nothing),
    testNameTags "size" ["size", "optional"]
                 (size optionalEncoding @?= Just 5),
    testNameTags "bounds_low" ["bounds", "optional"]
                 (assertThrows (\(IllegalArgument _) -> assertSuccess)
                               (return $! decode optionalEncoding (-1))),
    testNameTags "bounds_high" ["bounds", "optional"]
                 (assertThrows (\(IllegalArgument _) -> assertSuccess)
                               (return $! decode optionalEncoding 5)),
    testNameTags "maxDepth" ["maxDepth", "optional"]
                 (maxDepth optionalEncoding () @?= Just 1),
    testNameTags "highestIndex_zero" ["highestIndex", "optional"]
                 (highestIndex optionalEncoding () 0 @?= Just 0),
    testNameTags "highestIndex_one" ["highestIndex", "optional"]
                 (highestIndex optionalEncoding () 1 @?= Just 4) ]

makeExcludeTest (vals, excludes) =
  let
    name = concat excludes
  in
    name ~: testExclude ["linearDepthEncoding", "exclude"]
                        (exclude excludes (linearDepthEncoding vals))
                        vals excludes

excludeTests =
  let
    testdata = [
        (["A", "B", "C", "D", "E"], ["A"]),
        (["A", "B", "C", "D", "E"], ["B"]),
        (["A", "B", "C", "D", "E"], ["E"]),
        (["A", "B", "C", "D", "E"], ["A", "B"]),
        (["A", "B", "C", "D", "E"], ["B", "A"]),
        (["A", "B", "C", "D", "E"], ["A", "C"]),
        (["A", "B", "C", "D", "E"], ["C", "A"]),
        (["A", "B", "C", "D", "E"], ["B", "C"]),
        (["A", "B", "C", "D", "E"], ["C", "B"]),
        (["A", "B", "C", "D", "E"], ["A", "E"]),
        (["A", "B", "C", "D", "E"], ["E", "A"]),
        (["A", "B", "C", "D", "E"], ["B", "E"]),
        (["A", "B", "C", "D", "E"], ["E", "B"]),
        (["A", "B", "C", "D", "E"], ["A", "B", "C"]),
        (["A", "B", "C", "D", "E"], ["A", "B", "D"]),
        (["A", "B", "C", "D", "E"], ["A", "C", "E"]),
        (["A", "B", "C", "D", "E"], ["B", "C", "D"]),
        (["A", "B", "C", "D", "E"], ["B", "D", "E"]),
        (["A", "B", "C", "D", "E"], ["C", "D", "E"]),
        (["A", "B", "C", "D", "E"], ["A", "B", "C", "D"]),
        (["A", "B", "C", "D", "E"], ["A", "B", "C", "E"]),
        (["A", "B", "C", "D", "E"], ["B", "C", "D", "E"])
      ]
  in
    ("exclude_empty" ~:
     testLinearDepthEncoding
       ["linearDepthEncoding", "exclude"]
       (exclude [] (linearDepthEncoding ["A", "B", "C", "D", "E"]))
       ["A", "B", "C", "D", "E"]) : map makeExcludeTest testdata

testInfEither tags iso limit = [
    testNameTags "isomorphism" ("isomorphism" : tags)
                 (testIsomorphism iso limit),
    testNameTags "bounds_low" ("bounds" : tags)
                 (assertThrows (\(IllegalArgument _) -> assertSuccess)
                               (return $! decode iso (-1))),
    testNameTags "size" ("size" : tags) (size iso @?= Nothing),
    testNameTags "maxDepth" ("maxDepth" : tags)
                 (maxDepth iso (Left ()) @?= Just 0),
    testNameTags "maxDepth" ("maxDepth" : tags)
                 (maxDepth iso (Right ()) @?= Just 0),
    testNameTags "highestIndex" ("highestIndex" : tags)
                 (highestIndex iso (Left ()) 0 @?= Nothing),
    testNameTags "highestIndex" ("highestIndex" : tags)
                 (highestIndex iso (Right ()) 0 @?= Nothing) ]

testInfFinEither tags iso vals limit =
  let
    lastFinite = maximum (map (encode iso) vals)
  in
    [ testNameTags "isomorphism" ("isomorphism" : tags)
                   (testIsomorphism iso limit),
      testNameTags "isomorphism_vals" ("isomorphism" : tags)
                   (testEncodingVals iso vals),
      testNameTags "bounds_low" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "size" ("size" : tags) (size iso @?= Nothing),
      testNameTags "maxDepth" ("maxDepth" : tags)
                   (maxDepth iso (Left ()) @?= Just 0),
      testNameTags "maxDepth" ("maxDepth" : tags)
                   (maxDepth iso (Right ()) @?= Just 0),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (highestIndex iso (Left ()) 0 @?= Nothing),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (highestIndex iso (Right ()) 0 @?= Just lastFinite) ]

testFinInfEither tags iso vals limit =
  let
    lastFinite = maximum (map (encode iso) vals)
  in
    [ testNameTags "isomorphism" ("isomorphism" : tags)
                   (testIsomorphism iso limit),
      testNameTags "isomorphism_vals" ("isomorphism" : tags)
                   (testEncodingVals iso vals),
      testNameTags "bounds_low" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "size" ("size" : tags) (size iso @?= Nothing),
      testNameTags "maxDepth" ("maxDepth" : tags)
                   (maxDepth iso (Left ()) @?= Just 0),
      testNameTags "maxDepth" ("maxDepth" : tags)
                   (maxDepth iso (Right ()) @?= Just 0),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (highestIndex iso (Left ()) 0 @?= Just lastFinite),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (highestIndex iso (Right ()) 0 @?= Nothing) ]

testFinEither tags iso leftvals rightvals =
  let
    vals = leftvals ++ rightvals
    isosize = toInteger (length vals)
    lastleft = maximum (map (encode iso) leftvals)
    lastright = maximum (map (encode iso) rightvals)
  in
    [ testNameTags "isomorphism" ("isomorphism" : tags)
                   (testEncodingVals iso vals),
      testNameTags "size" ("size" : tags)
                   (size iso @?= Just isosize),
      testNameTags "bounds_low" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "bounds_high" ("bounds" : tags)
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (fromJust (size iso)))),
      testNameTags "maxDepth_Left" ("maxDepth" : tags)
                   (maxDepth iso (Left ()) @?= Just 0),
      testNameTags "maxDepth_Right" ("maxDepth" : tags)
                   (maxDepth iso (Right ()) @?= Just 0),
      testNameTags "highestIndex_Left" ("highestIndex" : tags)
                   (highestIndex iso (Left ()) 0 @?= Just lastleft),
      testNameTags "highestIndex_Right" ("highestIndex" : tags)
                   (highestIndex iso (Right ()) 0 @?= Just lastright) ]

eitherTests =
  let
    infiniteEncoding = integralInteger
    bigvals = ["A", "B", "C", "D", "E", "F"]
    smallvals = ["G", "H", "I"]
    biggerEncoding = fromHashableList bigvals
    smallerEncoding = fromHashableList smallvals
    finiteEncoding = biggerEncoding
  in
    [ "infinite_infinite" ~:
        testInfEither ["integral", "Integer", "either"]
                      (either infiniteEncoding infiniteEncoding) 10000,
      "infinite_finite" ~:
         testInfFinEither ["integral", "Integer", "fromHashableList", "either"]
                          (either infiniteEncoding finiteEncoding)
                          (map Right bigvals) 100,
      "finite_infinite" ~:
         testFinInfEither ["integral", "Integer", "fromHashableList", "either"]
                          (either finiteEncoding infiniteEncoding)
                          (map Left bigvals) 100,
      "finite_finite" ~:
         testFinEither ["fromHashableList", "either"]
                       (either finiteEncoding finiteEncoding)
                       (map Left bigvals) (map Right bigvals),
      "big_small" ~:
         testFinEither ["fromHashableList", "either"]
                       (either biggerEncoding smallerEncoding)
                       (map Left bigvals) (map Right smallvals),
      "small_big" ~:
         testFinEither ["fromHashableList", "either"]
                       (either smallerEncoding biggerEncoding)
                       (map Left smallvals) (map Right bigvals) ]

instance Hashable a => Hashable (Set a) where
  hashWithSalt s = Set.foldl hashWithSalt s

instance Hashable (HashSet.Set Integer) where
  hashWithSalt s = foldl hashWithSalt s . sort . HashSet.toList

testInfSet iso limit =
  let
    checkSetSizeDepth n =
      let
        s = decode iso n
      in
        fromInteger (depth iso SetSize s) @?= Set.size s
  in
    [ testNameTags "isomorphism" ["isomorphism", "set"]
                   (testIsomorphism iso limit),
      testNameTags "bounds_low" ["bounds", "set"]
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "size" ["size", "set"] (size iso @?= Nothing),
      testNameTags "depth_SetSize" ["depth", "set", "SetSize"]
                   (mapM_ checkSetSizeDepth [0..limit]),
      testNameTags "depth_SetElem" ["depth", "set", "SetElem"]
                   (mapM_ (\n -> depth iso (SetElem ()) (decode iso n) @?= 0)
                          [0..limit]),
      testNameTags "maxDepth_SetSize" ["maxDepth", "set", "SetSize"]
                   (maxDepth iso SetSize @?= Nothing),
      testNameTags "maxDepth_SetElem" ["maxDepth", "set", "SetElem"]
                   (maxDepth iso (SetElem ()) @?= Just 0),
      testNameTags "highestIndex_SetElem" ["highestIndex", "set", "SetElem"]
                   (highestIndex iso (SetElem ()) 0 @?= Nothing),
      testNameTags "highestIndex_SetSize_0" ["highestIndex", "set", "SetSize"]
                   (highestIndex iso SetSize 0 @?= Just 0),
      testNameTags "highestIndex_SetSize_n" ["highestIndex", "set", "SetSize"]
                   (highestIndex iso SetSize 1 @?= Nothing)
    ]

testFinSet iso vals =
  let
    numvals = length vals
    setvals = map Set.fromList (subsequences vals)
    isosize = toInteger (2 ^ numvals)

    checkHighestIndex n =
      let
        filteredsetvals = filter ((==) n.  Set.size) setvals
        m = maximum (map (encode iso) filteredsetvals)
      in
        highestIndex iso SetSize (toInteger n) @?= Just m
  in
    [ testNameTags "isomorphism" ["isomorphism", "set"]
                   (testEncodingVals iso setvals),
      testNameTags "size" ["size", "set"]
                   (size iso @?= Just isosize),
      testNameTags "bounds_low" ["bounds", "set"]
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "bounds_high" ["bounds",  "set"]
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (fromJust (size iso)))),
      testNameTags "depth_SetSize" ["depth", "set", "SetSize"]
                   (mapM_ (\s -> fromInteger (depth iso SetSize s) @?=
                                 Set.size s) setvals),
      testNameTags "depth_SetElem" ["depth", "set", "SetElem"]
                   (mapM_ (\val -> depth iso (SetElem ()) val @?= 0) setvals),
      testNameTags "maxDepth_SetSize" ["maxDepth", "set", "SetSize"]
                   (maxDepth iso SetSize @?= Just (toInteger numvals)),
      testNameTags "maxDepth_SetElem" ["maxDepth", "set", "SetElem"]
                   (maxDepth iso (SetElem ()) @?= Just 0),
      testNameTags "highestIndex_SetElem" ["highestIndex", "set", "SetElem"]
                   (highestIndex iso (SetElem ()) 0 @?=
                    Just (toInteger isosize)),
      testNameTags "highestIndex_SetSize_n" ["highestIndex", "set", "SetSize"]
                   (mapM_ checkHighestIndex [0..numvals])
    ]

testInfHashSet iso limit =
  let
    checkSetSizeDepth n =
      let
        s = decode iso n
      in
        fromInteger (depth iso SetSize s) @?= HashSet.size s
  in
    [ testNameTags "isomorphism" ["isomorphism", "hashSet"]
                   (testIsomorphism iso limit),
      testNameTags "bounds_low" ["bounds", "hashSet"]
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "size" ["size", "hashSet"] (size iso @?= Nothing),
      testNameTags "depth_SetSize" ["depth", "hashSet", "SetSize"]
                   (mapM_ checkSetSizeDepth [0..limit]),
      testNameTags "depth_SetElem" ["depth", "hashSet", "SetElem"]
                   (mapM_ (\n -> depth iso (SetElem ()) (decode iso n) @?= 0)
                          [0..limit]),
      testNameTags "maxDepth_SetSize" ["maxDepth", "hashSet", "SetSize"]
                   (maxDepth iso SetSize @?= Nothing),
      testNameTags "maxDepth_SetElem" ["maxDepth", "hashSet", "SetElem"]
                   (maxDepth iso (SetElem ()) @?= Just 0),
      testNameTags "highestIndex_SetElem" ["highestIndex", "hashSet", "SetElem"]
                   (highestIndex iso (SetElem ()) 0 @?= Nothing),
      testNameTags "highestIndex_SetSize_0" ["highestIndex", "hashSet",
                                             "SetSize"]
                   (highestIndex iso SetSize 0 @?= Just 0),
      testNameTags "highestIndex_SetSize_n" ["highestIndex", "hashSet",
                                             "SetSize"]
                   (highestIndex iso SetSize 1 @?= Nothing)
    ]

testFinHashSet iso vals =
  let
    numvals = length vals
    setvals = map HashSet.fromList (subsequences vals)
    isosize = toInteger (2 ^ numvals)

    checkHighestIndex n =
      let
        filteredsetvals = filter ((==) n.  HashSet.size) setvals
        m = maximum (map (encode iso) filteredsetvals)
      in
        highestIndex iso SetSize (toInteger n) @?= Just m
  in
    [ testNameTags "isomorphism" ["isomorphism", "hashSet"]
                   (testEncodingVals iso setvals),
      testNameTags "size" ["size", "hashSet"]
                   (size iso @?= Just isosize),
      testNameTags "bounds_low" ["bounds", "hashSet"]
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (-1))),
      testNameTags "bounds_high" ["bounds",  "hashSet"]
                   (assertThrows (\(IllegalArgument _) -> assertSuccess)
                                 (return $! decode iso (fromJust (size iso)))),
      testNameTags "depth_SetSize" ["depth", "hashSet", "SetSize"]
                   (mapM_ (\s -> fromInteger (depth iso SetSize s) @?=
                                 HashSet.size s) setvals),
      testNameTags "depth_SetElem" ["depth", "hashSet", "SetElem"]
                   (mapM_ (\val -> depth iso (SetElem ()) val @?= 0) setvals),
      testNameTags "maxDepth_SetSize" ["maxDepth", "hashSet", "SetSize"]
                   (maxDepth iso SetSize @?= Just (toInteger numvals)),
      testNameTags "maxDepth_SetElem" ["maxDepth", "hashSet", "SetElem"]
                   (maxDepth iso (SetElem ()) @?= Just 0),
      testNameTags "highestIndex_SetElem" ["highestIndex", "hashSet", "SetElem"]
                   (highestIndex iso (SetElem ()) 0 @?=
                    Just (toInteger isosize)),
      testNameTags "highestIndex_SetSize_n" ["highestIndex", "hashSet",
                                             "SetSize"]
                   (mapM_ checkHighestIndex [0..numvals])
    ]

setTests = [
    "infinite" ~: testInfSet (set integralInteger) 10000,
    "finite" ~: testFinSet (set (fromHashableList ["A", "B", "C", "D", "E"]))
                           ["A", "B", "C", "D", "E"]
  ]

hashSetTests = [
    "infinite" ~: testInfHashSet (hashSet integralInteger) 10000,
    "finite" ~:
      testFinHashSet (hashSet (fromHashableList ["A", "B", "C", "D", "E"]))
                     ["A", "B", "C", "D", "E"]
  ]

testlist :: [Test]
testlist = [
    "identity" ~: testInfDimlessEncoding ["Integer"] identity,
    "singleton" ~: testFiniteEncodingWithVals ["singleton"]
                                              (singleton "A") ["A"],
    "integral" ~: integralTests,
    "interval" ~: intervalTests,
    "fromHashableList" ~:
      testFiniteEncodingWithVals ["fromHashableList"]
                                 (fromHashableList ["A", "B", "C", "D", "E"])
                                 ["A", "B", "C", "D", "E"],
    "fromOrdList" ~:
      testFiniteEncodingWithVals ["fromOrdList"]
                                 (fromOrdList ["A", "B", "C", "D", "E"])
                                 ["A", "B", "C", "D", "E"],
    "wrap" ~:
      testFiniteEncodingWithVals ["wrap", "fromOrdList"]
                                 (wrap (map toUpper) (map toLower)
                                       (fromOrdList ["A", "B", "C", "D", "E"]))
                                 ["a", "b", "c", "d", "e"],
    "optional" ~: optionalEncodingTests,
    "mandatory" ~:
      testFiniteEncodingWithVals ["mandatory"] (mandatory optionalEncoding)
                                 ["A", "B", "C", "D"],
    "nonzero" ~:
      testFiniteEncodingWithVals ["nonzero", "fromHashableList"]
                                 (nonzero (fromHashableList ["A", "B", "C",
                                                             "D", "E", "F"]))
                                 ["B", "C", "D", "E", "F"],
    "linearDepthEncoding" ~:
      testLinearDepthEncoding ["linearDepthEncoding"]
                              (linearDepthEncoding ["A", "B", "C", "D", "E"])
                              ["A", "B", "C", "D", "E"],
    "exclude" ~: excludeTests,
    "either" ~: eitherTests,
    "set" ~: setTests,
    "hashSet" ~: hashSetTests
  ]

tests :: Test
tests = "ArithEncode" ~: testlist
