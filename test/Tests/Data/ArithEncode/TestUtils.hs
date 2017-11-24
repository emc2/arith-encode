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

module Tests.Data.ArithEncode.TestUtils where

import Control.Monad
import Data.ArithEncode
import Data.Hashable
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Test.HUnitPlus.Base

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set

integralInteger :: Encoding Integer
integralInteger = integral

integralInt64 :: Encoding Int64
integralInt64 = integral

integralWord64 :: Encoding Int64
integralWord64 = integral

integralInt8 :: Encoding Int8
integralInt8 = integral

integralWord8 :: Encoding Int8
integralWord8 = integral

intervalInteger :: Integer -> Integer -> Encoding Integer
intervalInteger = interval

intervalInt64 :: Int64 -> Int64 -> Encoding Int64
intervalInt64 = interval

intervalInt8 :: Int8 -> Int8 -> Encoding Int8
intervalInt8 = interval

intervalWord64 :: Word64 -> Word64 -> Encoding Word64
intervalWord64 = interval

intervalWord8 :: Word8 -> Word8 -> Encoding Word8
intervalWord8 = interval

optionalEncoding = optional (fromHashableList ['A', 'B', 'C', 'D'])

testIsomorphism :: (Hashable ty, Ord ty, Show ty) =>
                   Encoding ty -> Integer -> IO ()
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

testEncodingVals :: (Show ty, Eq ty) => Encoding ty -> [ty] -> IO ()
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
                      [String] -> Encoding ty -> IO ()
testFiniteEncoding tags iso =
  let
    limit = fromJust (size iso)
  in
    testIsomorphism iso limit

testInDomain :: Show ty => Encoding ty -> [ty] -> Test
testInDomain enc vals =
  test (mapM_ (\val -> inDomain enc val @? "inDomain " ++ show val) vals)

testNotInDomain :: Show ty => Encoding ty -> [ty] -> Test
testNotInDomain enc vals =
  test (mapM_ (\val -> not (inDomain enc val) @? "not inDomain " ++ show val)
        vals)

testInfDimlessEncodingWithLimit tags iso limit = [
    testNameTags "isomorphism" ("isomorphism" : tags)
                 (testIsomorphism iso limit),
    testNameTags "bounds_low" ("bounds" : tags)
                 (assertThrows (\(IllegalArgument _) -> assertSuccess)
                               (return $! decode iso (-1))),
    testNameTags "size" ("size" : tags) (size iso @?= Nothing),
    testNameTags "inDomain" ("inDomain" : tags)
                 (testInDomain iso (map (decode iso) [0..limit]))
  ]

testInfDimlessEncoding tags iso = testInfDimlessEncodingWithLimit tags iso 10000

testFiniteEncodingWithVals tags iso vals nonvals =
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
      testNameTags "inDomain" ("inDomain" : tags) (testInDomain iso vals),
      testNameTags "not_inDomain" ("inDomain" : tags)
                   (testNotInDomain iso nonvals) ]

testExclude tags iso vals nonvals excludes =
  let
    isosize = toInteger (length vals - length excludes)
    filtered = filter ((flip notElem) excludes) vals
    badvals = nonvals ++ excludes
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
      testNameTags "inDomain" ("inDomain" : tags) (testInDomain iso filtered),
      testNameTags "not_inDomain" ("inDomain" : tags)
                   (testNotInDomain iso badvals) ]

-- instance (Ord val, Hashable val) => Hashable (HashSet.HashSet val) where
--   hashWithSalt s set = s `hashWithSalt` sort (HashSet.toList set)

instance (Ord val, Hashable val) => Ord (HashSet.HashSet val) where
  compare s1 s2 = compare (sort (HashSet.toList s1)) (sort (HashSet.toList s2))

-- instance (Ord key, Hashable key, Ord val, Hashable val) =>
--         Hashable (HashMap.HashMap key val) where
--   hashWithSalt s map = s `hashWithSalt` sort (HashMap.toList map)

instance (Ord key, Hashable key, Ord val) =>
         Ord (HashMap.HashMap key val) where
  compare m1 m2 = compare (sort (HashMap.toList m1)) (sort (HashMap.toList m2))

instance (Hashable key, Hashable val) => Hashable (Map.Map key val) where
  hashWithSalt s map = s `hashWithSalt` Map.assocs map

instance Hashable a => Hashable (Set.Set a) where
  hashWithSalt s = Set.foldl hashWithSalt s
