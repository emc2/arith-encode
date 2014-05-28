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
import Data.Hashable
import Data.Int
import Data.Maybe
import Data.Word
import Data.Set(Set)
import Test.HUnitPlus.Base

import qualified Data.HashSet as Set

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
      testNameTags "maxDepth" ("maxDepth" : tags) (maxDepth iso () @?= Just 0),
      testNameTags "highestIndex" ("highestIndex" : tags)
                   (highestIndex iso () 0 @?= Just isosize) ]

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

testlist :: [Test]
testlist = [
    -- Identity encoding tests
    "identityEncoding" ~:  testInfDimlessEncoding ["Integer"] identityEncoding,
    -- Integral encoding tests
    "integralEncodingInteger" ~:
      testInfDimlessEncoding ["integral", "Integer"] integralEncodingInteger,
    "integralEncodingInt64" ~:
      testInfDimlessEncoding ["integral", "Int64"] integralEncodingInt64,
    "integralEncodingWord64" ~:
      testInfDimlessEncoding ["integral", "Word64"] integralEncodingInt64,
    "integralEncodingInt8" ~:
      testInfDimlessEncodingWithLimit ["integral", "Int8"]
                                       integralEncodingInt8 255,
    "integralEncodingWord64" ~:
      testInfDimlessEncodingWithLimit ["integral", "Word8"]
                                       integralEncodingWord8 255,
    -- Interval encoding tests
    "intervalEncodingInteger_0_10000" ~:
      testFiniteEncodingWithVals ["interval", "Integer", "isomorhpism"]
                                 (intervalEncodingInteger 0 10000) [0..10000],
    "intervalEncodingInteger_2000_10000" ~:
      testFiniteEncodingWithVals ["interval", "Integer", "isomorhpism"]
                                 (intervalEncodingInteger 2000 10000)
                                 [2000..10000],
    "intervalEncodingInteger_neg2000_2000" ~:
      testFiniteEncodingWithVals ["interval", "Integer", "isomorhpism"]
                                 (intervalEncodingInteger (-2000) 2000)
                                 [-2000..2000],
    "intervalEncodingInteger_neg10000_neg2000" ~:
      testFiniteEncodingWithVals ["interval", "Integer", "isomorhpism"]
                                 (intervalEncodingInteger (-10000) (-2000))
                                 [-10000..(-2000)],
    "intervalEncodingInt64_0_10000" ~:
      testFiniteEncodingWithVals ["interval", "Int64", "isomorhpism"]
                                 (intervalEncodingInt64 0 10000) [0..10000],
    "intervalEncodingInt64_2000_10000" ~:
      testFiniteEncodingWithVals ["interval", "Int64", "isomorhpism"]
                                 (intervalEncodingInt64 2000 10000)
                                 [2000..10000],
    "intervalEncodingInt64_neg2000_2000" ~:
      testFiniteEncodingWithVals ["interval", "Int64", "isomorhpism"]
                                 (intervalEncodingInt64 (-2000) 2000)
                                 [-2000..2000],
    "intervalEncodingInt64_neg10000_neg2000" ~:
      testFiniteEncodingWithVals ["interval", "Int64", "isomorhpism"]
                                 (intervalEncodingInt64 (-10000) (-2000))
                                 [-10000..(-2000)],
    "intervalEncodingWord64_0_10000" ~:
      testFiniteEncodingWithVals ["interval", "Word64", "isomorhpism"]
                                 (intervalEncodingWord64 0 10000) [0..10000],
    "intervalEncodingWord64_2000_10000" ~:
      testFiniteEncodingWithVals ["interval", "Word64", "isomorhpism"]
                                 (intervalEncodingWord64 2000 10000)
                                 [2000..10000],
    "intervalEncodingInt8_0_100" ~:
      testFiniteEncodingWithVals ["interval", "Int8", "isomorhpism"]
                                 (intervalEncodingInt8 0 100) [0..100],
    "intervalEncodingInt8_20_100" ~:
      testFiniteEncodingWithVals ["interval", "Int8", "isomorhpism"]
                                 (intervalEncodingInt8 20 100)
                                 [20..100],
    "intervalEncodingInt8_neg20_20" ~:
      testFiniteEncodingWithVals ["interval", "Int8", "isomorhpism"]
                                 (intervalEncodingInt8 (-20) 20)
                                 [-20..20],
    "intervalEncodingInt8_neg100_neg20" ~:
      testFiniteEncodingWithVals ["interval", "Int8", "isomorhpism"]
                                 (intervalEncodingInt8 (-100) (-20))
                                 [-100..(-20)],
    "intervalEncodingInt8_neg128_127" ~:
      testFiniteEncodingWithVals ["interval", "Int8", "isomorhpism"]
                                 (intervalEncodingInt8 (-128) 127)
                                 [-128..127],
    "intervalEncodingWord8_0_100" ~:
      testFiniteEncodingWithVals ["interval", "Word8", "isomorhpism"]
                                 (intervalEncodingWord8 0 100) [0..100],
    "intervalEncodingWord8_20_100" ~:
      testFiniteEncodingWithVals ["interval", "Word8", "isomorhpism"]
                                 (intervalEncodingWord8 20 100)
                                 [20..100],
    "intervalEncodingWord8_0_255" ~:
      testFiniteEncodingWithVals ["interval", "Word8", "isomorhpism"]
                                 (intervalEncodingWord8 0 255)
                                 [0..255],
    "fromHashableList" ~:
      testFiniteEncodingWithVals ["fromHashableList", "isomorhpism"]
                                 (fromHashableList ["A", "B", "C", "D", "E"])
                                 ["A", "B", "C", "D", "E"],
    "fromOrdList" ~:
      testFiniteEncodingWithVals ["fromOrdList", "isomorhpism"]
                                 (fromOrdList ["A", "B", "C", "D", "E"])
                                 ["A", "B", "C", "D", "E"]
  ]

tests :: Test
tests = "ArithEncode" ~: testlist
