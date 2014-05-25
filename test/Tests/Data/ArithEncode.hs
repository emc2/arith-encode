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

testFiniteEncoding :: (Hashable ty, Ord ty, Show ty) => Encoding dim ty -> IO ()
testFiniteEncoding iso =
  let
    limit = fromJust (size iso)
  in
    testIsomorphism iso limit

testInfDimlessEncodingWithLimit iso limit = [
    testNameTags "isomorphism" ["isomorphism"] (testIsomorphism iso limit),
    testNameTags "size" ["size"] (size iso @?= Nothing)
  ]

testInfDimlessEncoding iso = testInfDimlessEncodingWithLimit iso 10000

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
    testNameTags "identityEncoding_isomorphism" ["identity", "isomorphism"]
                 (testInfDimlessEncoding identityEncoding),
    testNameTags "identityEncoding_size" ["identity", "size"]
                 (size identityEncoding @?= Nothing),
    -- Integral encoding tests
    testNameTags "integralEncodingInteger_isomorphism"
                 ["integral", "Integer", "isomorphism"]
                 (testInfDimlessEncoding integralEncodingInteger),
    testNameTags "integralEncodingInteger_size"
                 ["integral", "Integer", "size"]
                 (size integralEncodingInteger @?= Nothing),
    testNameTags "integralEncodingInt64_isomorphism"
                 ["integral", "Int64", "isomorphism"]
                 (testInfDimlessEncoding integralEncodingInt64),
    testNameTags "integralEncodingInt64_size"
                 ["integral", "Int64", "size"]
                 (size integralEncodingInt64 @?= Nothing),
    testNameTags "integralEncodingWord64_isomorphism"
                 ["integral", "Word64", "isomorphism"]
                 (testInfDimlessEncoding integralEncodingWord64),
    testNameTags "integralEncodingWord64_size"
                 ["integral", "Word64", "size"]
                 (size integralEncodingWord64 @?= Nothing),
    testNameTags "integralEncodingInt8_isomorphism"
                 ["integral", "Int8", "isomorphism"]
                 (testEncodingVals integralEncodingInt8 [-128..127]),
    testNameTags "integralEncodingInt8_size"
                 ["integral", "Int8", "size"]
                 (size integralEncodingInt8 @?= Nothing),
    testNameTags "integralEncodingWord8_isomorphism"
                 ["integral", "Word8", "isomorphism"]
                 (testEncodingVals integralEncodingWord8 [0..255]),
    testNameTags "integralEncodingWord8_size"
                 ["integral", "Word8", "size"]
                 (size integralEncodingWord8 @?= Nothing),
    -- Interval encoding tests
    testNameTags "intervalEncodingInteger_0_10000_isomorphism"
                 ["interval", "Integer", "isomorhpism"]
                 (testEncodingVals (intervalEncodingInteger 0 10000) [0..10000]),
    testNameTags "intervalEncodingInteger_0_10000_size"
                 ["interval", "Integer", "size"]
                 (size (intervalEncodingInteger 0 10000) @?= Just 10001),
    testNameTags "intervalEncodingInteger_2000_10000_isomorphism"
                 ["interval", "Integer", "isomorhpism"]
                 (testEncodingVals (intervalEncodingInteger 2000 10000)
                                   [2000..10000]),
    testNameTags "intervalEncodingInteger_2000_10000_size"
                 ["interval", "Integer", "size"]
                 (size (intervalEncodingInteger 2000 10000) @?= Just 8001),
    testNameTags "intervalEncodingInteger_neg2000_2000_isomorphism"
                 ["interval", "Integer", "isomorphism"]
                 (testEncodingVals (intervalEncodingInteger (-2000) 2000)
                                   [-2000..2000]),
    testNameTags "intervalEncodingInteger_neg2000_2000_size"
                 ["interval", "Integer", "size"]
                 (size (intervalEncodingInteger (-2000) 2000) @?= Just 4001),
    testNameTags "intervalEncodingInteger_neg10000_neg2000_isomorphism"
                 ["interval", "Integer", "isomorphism"]
                 (testEncodingVals (intervalEncodingInteger (-10000) (-2000))
                                   [-10000..(-2000)]),
    testNameTags "intervalEncodingInteger_neg10000_neg2000_size"
                 ["interval", "Integer", "size"]
                 (size (intervalEncodingInteger (-10000) (-2000)) @?= Just 8001),
    testNameTags "intervalEncodingInt64_0_10000_isomorphism"
                 ["interval", "Int64", "isomorhpism"]
                 (testEncodingVals (intervalEncodingInt64 0 10000) [0..10000]),
    testNameTags "intervalEncodingInt64_0_10000_size"
                 ["interval", "Int64", "size"]
                 (size (intervalEncodingInt64 0 10000) @?= Just 10001),
    testNameTags "intervalEncodingInt64_2000_10000_isomorphism"
                 ["interval", "Int64", "isomorhpism"]
                 (testEncodingVals (intervalEncodingInt64 2000 10000)
                                   [2000..10000]),
    testNameTags "intervalEncodingInt64_2000_10000_size"
                 ["interval", "Int64", "size"]
                 (size (intervalEncodingInt64 2000 10000) @?= Just 8001),
    testNameTags "intervalEncodingInt64_neg2000_2000_isomorphism"
                 ["interval", "Int64", "isomorphism"]
                 (testEncodingVals (intervalEncodingInt64 (-2000) 2000)
                                   [-2000..2000]),
    testNameTags "intervalEncodingInt64_neg2000_2000_size"
                 ["interval", "Int64", "size"]
                 (size (intervalEncodingInt64 (-2000) 2000) @?= Just 4001),
    testNameTags "intervalEncodingInt64_neg10000_neg2000_isomorphism"
                 ["interval", "Int64", "isomorphism"]
                 (testEncodingVals (intervalEncodingInt64 (-10000) (-2000))
                                   [-10000..(-2000)]),
    testNameTags "intervalEncodingInt64_neg10000_neg2000_size"
                 ["interval", "Int64", "size"]
                 (size (intervalEncodingInt64 (-10000) (-2000)) @?= Just 8001),
    testNameTags "intervalEncodingWord64_0_10000_isomorphism"
                 ["interval", "Word64", "isomorhpism"]
                 (testEncodingVals (intervalEncodingWord64 0 10000) [0..10000]),
    testNameTags "intervalEncodingWord64_0_10000_size"
                 ["interval", "Word64", "size"]
                 (size (intervalEncodingWord64 0 10000) @?= Just 10001),
    testNameTags "intervalEncodingWord64_2000_10000_isomorphism"
                 ["interval", "Word64", "isomorhpism"]
                 (testEncodingVals (intervalEncodingWord64 2000 10000)
                                   [2000..10000]),
    testNameTags "intervalEncodingWord64_2000_10000_size"
                 ["interval", "Word64", "size"]
                 (size (intervalEncodingWord64 2000 10000) @?= Just 8001),
    testNameTags "intervalEncodingInt8_0_100_isomorphism"
                 ["interval", "Int8", "isomorhpism"]
                 (testEncodingVals (intervalEncodingInt8 0 100) [0..100]),
    testNameTags "intervalEncodingInt8_0_100_size"
                 ["interval", "Int8", "size"]
                 (size (intervalEncodingInt8 0 100) @?= Just 101),
    testNameTags "intervalEncodingInt8_20_100_isomorphism"
                 ["interval", "Int8", "isomorhpism"]
                 (testEncodingVals (intervalEncodingInt8 20 100)
                                   [20..100]),
    testNameTags "intervalEncodingInt8_20_100_size"
                 ["interval", "Int8", "size"]
                 (size (intervalEncodingInt8 20 100) @?= Just 81),
    testNameTags "intervalEncodingInt8_neg20_20_isomorphism"
                 ["interval", "Int8", "isomorphism"]
                 (testEncodingVals (intervalEncodingInt8 (-20) 20)
                                   [-20..20]),
    testNameTags "intervalEncodingInt8_neg20_20_size"
                 ["interval", "Int8", "size"]
                 (size (intervalEncodingInt8 (-20) 20) @?= Just 41),
    testNameTags "intervalEncodingInt8_neg100_neg20_isomorphism"
                 ["interval", "Int8", "isomorphism"]
                 (testEncodingVals (intervalEncodingInt8 (-100) (-20))
                                   [-100..(-20)]),
    testNameTags "intervalEncodingInt8_neg100_neg20_size"
                 ["interval", "Int8", "size"]
                 (size (intervalEncodingInt8 (-100) (-20)) @?= Just 81),
    testNameTags "intervalEncodingInt8_neg128_127_isomorphism"
                 ["interval", "Int8", "isomorhpism"]
                 (testEncodingVals (intervalEncodingInt8 (-128) 127)
                                   [-128..127]),
    testNameTags "intervalEncodingInt8_neg128_127_size"
                 ["interval", "Int8", "size"]
                 (size (intervalEncodingInt8 (-128) 127) @?= Just 256),
    testNameTags "intervalEncodingWord8_0_100_isomorphism"
                 ["interval", "Word8", "isomorhpism"]
                 (testEncodingVals (intervalEncodingWord8 0 100) [0..100]),
    testNameTags "intervalEncodingWord8_0_100_size"
                 ["interval", "Word8", "size"]
                 (size (intervalEncodingWord8 0 100) @?= Just 101),
    testNameTags "intervalEncodingWord8_20_100_isomorphism"
                 ["interval", "Word8", "isomorhpism"]
                 (testEncodingVals (intervalEncodingWord8 20 100)
                                   [20..100]),
    testNameTags "intervalEncodingWord8_20_100_size"
                 ["interval", "Word8", "size"]
                 (size (intervalEncodingWord8 20 100) @?= Just 81),
    testNameTags "intervalEncodingWord8_0_255_isomorphism"
                 ["interval", "Word8", "isomorhpism"]
                 (testEncodingVals (intervalEncodingWord8 0 255) [0..255]),
    testNameTags "intervalEncodingWord8_0_100_size"
                 ["interval", "Word8", "size"]
                 (size (intervalEncodingWord8 0 255) @?= Just 256)
  ]

tests :: Test
tests = "ArithEncode" ~: testlist
