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

module Tests.Data.ArithEncode.Binary(tests) where

import Data.ArithEncode
import Data.ArithEncode.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Test.HUnitPlus.Base
import Tests.Data.ArithEncode.TestUtils(integralInteger, intervalInteger)

testPutGet :: (Eq ty, Show ty) => Encoding ty -> ty -> Assertion
testPutGet enc val =
  let
    bs = runPut (putWithEncoding enc val)
    val' = runGet (getWithEncoding enc) bs
  in do
    val @=? val'

testFiniteEncoding :: (Eq ty, Show ty) => String -> Encoding ty -> [ty] -> Test
testFiniteEncoding name enc vals =
  name ~: mapM_ (testPutGet enc) vals

infiniteVals = [0..20000] ++
  [((1 `shiftL` 64) - 0x100)..((1 `shiftL` 64) + 0x100)] ++
  (map (\n -> product (map (\m -> m * 2 + 1) [1..n])) [1..6000])

testlist :: [Test]
testlist = [
    testFiniteEncoding "singleton" (singleton 1) [1],
    testFiniteEncoding "finite_2" (intervalInteger 1 2) [1, 2],
    testFiniteEncoding "finite_10" (intervalInteger 1 10) [1..10],
    testFiniteEncoding "finite_100" (intervalInteger 1 100) [1..100],
    testFiniteEncoding "finite_10000" (intervalInteger 1 10000) [1..10000],
    testFiniteEncoding "finite_64bit" (intervalInteger 0 0x100000000000000000000)
                                      [1..10000],
    testFiniteEncoding "infinite" integralInteger infiniteVals
  ]

tests :: Test
tests = "Binary" ~: testlist
