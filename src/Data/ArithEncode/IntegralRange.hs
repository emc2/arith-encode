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
{-# OPTIONS_GHC -funbox-strict-fields -Werror -Wall #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Defines an 'ArithEncode' that maps an arbitrary finite range of
-- any 'Integral' on to a same-sized range of natural numbers starting
-- at zero.
--
-- \"Range\" is used in lieu of the arguably more appropriate term
-- \"interval\" to avoid alliteration.
module Data.ArithEncode.IntegralRange(
       IntegralRangeArithEncode,
       integralRange
       ) where

import Data.ArithEncode.Class

-- | Datatype for an isomorphism to and from ranges of Integral types
data IntegralRangeArithEncode num =
  IntegralRangeArithEncode {
    -- | Lower bound.
    irLower :: !num,
    -- | Upper bound.
    irUpper :: !num
  }

-- | Create an object representing a direct mapping between elements
-- of an Integral type that fall between an upper and a lower bound.
-- Note: that Integers or elements fall between the bounds will not be
-- checked dynamically.
integralRange :: Integral num
                 => num
                 -- ^ The lower bound.
                 -> num
                 -- ^ The upper bound.
                 -> IntegralRangeArithEncode num
                 -- ^ The resulting isomorphism object.
integralRange lower upper =
  IntegralRangeArithEncode { irLower = lower, irUpper = upper }

instance Integral num => ArithEncode (IntegralRangeArithEncode num) num where
  size IntegralRangeArithEncode { irLower = lower, irUpper = upper } =
    Just (toInteger (upper - lower))

  encode IntegralRangeArithEncode { irLower = lower } num =
    toInteger (num - lower)

  decode IntegralRangeArithEncode { irLower = lower } =
    (+ lower) . fromInteger
