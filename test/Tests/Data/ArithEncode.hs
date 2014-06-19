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
import Test.HUnitPlus.Base

import qualified Data.Array as Array
import qualified Data.HashMap as HashMap
import qualified Data.HashSet as Set

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
  in
    mkEncoding encodefunc decodefunc sizeval maxdepthfunc depthfunc highindexfunc

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
        not (Set.member val previous) @?
          "decode " ++ show num ++ " produced duplicate value " ++ show val
        return (Set.insert val previous)
  in
    foldM_ foldfun Set.empty [0..limit]

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
        not (Set.member num previous) @?
          "decode " ++ show num ++ " produced duplicate value " ++ show val
        return (Set.insert num previous)
  in
    foldM_ foldfun Set.empty vals

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
      testNameTags "testDepth" ("depth" : tags) (testDepth zipped),
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
      testNameTags "testDepth" ("depth" : tags) (testDepth filtereddepths),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (testHighestIndex (isosize - 1)) ]

integralEncodingInteger :: Encoding () Integer
integralEncodingInteger = integralEncoding

integralEncodingInt64 :: Encoding () Int64
integralEncodingInt64 = integralEncoding

integralEncodingWord64 :: Encoding () Int64
integralEncodingWord64 = integralEncoding

integralEncodingInt8 :: Encoding () Int8
integralEncodingInt8 = integralEncoding

integralEncodingWord8 :: Encoding () Int8
integralEncodingWord8 = integralEncoding

intervalEncodingInteger :: Integer -> Integer -> Encoding () Integer
intervalEncodingInteger = intervalEncoding

intervalEncodingInt64 :: Int64 -> Int64 -> Encoding () Int64
intervalEncodingInt64 = intervalEncoding

intervalEncodingInt8 :: Int8 -> Int8 -> Encoding () Int8
intervalEncodingInt8 = intervalEncoding

intervalEncodingWord64 :: Word64 -> Word64 -> Encoding () Word64
intervalEncodingWord64 = intervalEncoding

intervalEncodingWord8 :: Word8 -> Word8 -> Encoding () Word8
intervalEncodingWord8 = intervalEncoding

optionalEncoding = optional (fromHashableList ["A", "B", "C", "D"])

integralEncodingTests = [
    "Integer" ~:
      testInfDimlessEncoding ["integral", "Integer"] integralEncodingInteger,
    "Int64" ~:
      testInfDimlessEncoding ["integral", "Int64"] integralEncodingInt64,
    "Word64" ~:
      testInfDimlessEncoding ["integral", "Word64"] integralEncodingInt64,
    "Int8" ~:
      testInfDimlessEncodingWithLimit ["integral", "Int8"]
                                       integralEncodingInt8 255,
    "Word64" ~:
      testInfDimlessEncodingWithLimit ["integral", "Word8"]
                                       integralEncodingWord8 255
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
    name = concat ("exclude_" : excludes)
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
  
  

testlist :: [Test]
testlist = [
    -- Identity encoding tests
    "identityEncoding" ~:  testInfDimlessEncoding ["Integer"] identityEncoding,
    -- Integral encoding tests
    "integralEncoding" ~: integralEncodingTests,
    -- Interval encoding tests
    "intervalEncodingInteger_0_10000" ~:
      testFiniteEncodingWithVals ["interval", "Integer"]
                                 (intervalEncodingInteger 0 10000) [0..10000],
    "intervalEncodingInteger_2000_10000" ~:
      testFiniteEncodingWithVals ["interval", "Integer"]
                                 (intervalEncodingInteger 2000 10000)
                                 [2000..10000],
    "intervalEncodingInteger_neg2000_2000" ~:
      testFiniteEncodingWithVals ["interval", "Integer"]
                                 (intervalEncodingInteger (-2000) 2000)
                                 [-2000..2000],
    "intervalEncodingInteger_neg10000_neg2000" ~:
      testFiniteEncodingWithVals ["interval", "Integer"]
                                 (intervalEncodingInteger (-10000) (-2000))
                                 [-10000..(-2000)],
    "intervalEncodingInt64_0_10000" ~:
      testFiniteEncodingWithVals ["interval", "Int64"]
                                 (intervalEncodingInt64 0 10000) [0..10000],
    "intervalEncodingInt64_2000_10000" ~:
      testFiniteEncodingWithVals ["interval", "Int64"]
                                 (intervalEncodingInt64 2000 10000)
                                 [2000..10000],
    "intervalEncodingInt64_neg2000_2000" ~:
      testFiniteEncodingWithVals ["interval", "Int64"]
                                 (intervalEncodingInt64 (-2000) 2000)
                                 [-2000..2000],
    "intervalEncodingInt64_neg10000_neg2000" ~:
      testFiniteEncodingWithVals ["interval", "Int64"]
                                 (intervalEncodingInt64 (-10000) (-2000))
                                 [-10000..(-2000)],
    "intervalEncodingWord64_0_10000" ~:
      testFiniteEncodingWithVals ["interval", "Word64"]
                                 (intervalEncodingWord64 0 10000) [0..10000],
    "intervalEncodingWord64_2000_10000" ~:
      testFiniteEncodingWithVals ["interval", "Word64"]
                                 (intervalEncodingWord64 2000 10000)
                                 [2000..10000],
    "intervalEncodingInt8_0_100" ~:
      testFiniteEncodingWithVals ["interval", "Int8"]
                                 (intervalEncodingInt8 0 100) [0..100],
    "intervalEncodingInt8_20_100" ~:
      testFiniteEncodingWithVals ["interval", "Int8"]
                                 (intervalEncodingInt8 20 100)
                                 [20..100],
    "intervalEncodingInt8_neg20_20" ~:
      testFiniteEncodingWithVals ["interval", "Int8"]
                                 (intervalEncodingInt8 (-20) 20)
                                 [-20..20],
    "intervalEncodingInt8_neg100_neg20" ~:
      testFiniteEncodingWithVals ["interval", "Int8"]
                                 (intervalEncodingInt8 (-100) (-20))
                                 [-100..(-20)],
    "intervalEncodingInt8_neg128_127" ~:
      testFiniteEncodingWithVals ["interval", "Int8"]
                                 (intervalEncodingInt8 (-128) 127)
                                 [-128..127],
    "intervalEncodingWord8_0_100" ~:
      testFiniteEncodingWithVals ["interval", "Word8"]
                                 (intervalEncodingWord8 0 100) [0..100],
    "intervalEncodingWord8_20_100" ~:
      testFiniteEncodingWithVals ["interval", "Word8"]
                                 (intervalEncodingWord8 20 100)
                                 [20..100],
    "intervalEncodingWord8_0_255" ~:
      testFiniteEncodingWithVals ["interval", "Word8"]
                                 (intervalEncodingWord8 0 255)
                                 [0..255],
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
    "exclude" ~: excludeTests
  ]

tests :: Test
tests = "ArithEncode" ~: testlist
