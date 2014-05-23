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

-- | Defines a construction on 'ArithEncoding's that wraps another
-- 'ArithEncoding' up in a 'Maybe'.  The resulting isomorphism maps
-- @0@ to 'Nothing', and @n + 1@ to the value mapped to @n@ by the
-- original encoding.
module Data.ArithEncode.Option(
       OptionArithEncode,
       optionEncoding
       ) where

import Data.ArithEncode.Class

-- | Datatype for an isomorphism that wraps an inner isomorphism up in
-- a 'Maybe'.
data OptionArithEncode iso =
  OptionArithEncode {
    -- | The inner isomorphism.
    optInner :: !iso
  }

-- | Construct an option isomorphism.
optionEncoding :: iso
               -- ^ The isomorphism from which to build the option.
               -> OptionArithEncode iso
optionEncoding inner = OptionArithEncode { optInner = inner }

instance ArithEncodeBound iso => ArithEncodeBound (OptionArithEncode iso) where
  size OptionArithEncode { optInner = inner } =
    do
      count <- size inner
      return (count + 1)

instance (ArithEncode iso ty) =>
         ArithEncode (OptionArithEncode iso) (Maybe ty) where
  encode _ Nothing = 0
  encode OptionArithEncode { optInner = inner } (Just item) =
    (encode inner item) + 1

  decode _ 0 = Nothing
  decode OptionArithEncode { optInner = inner } num =
    Just (decode inner (num - 1))
