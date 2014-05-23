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

-- | An 'ArithEncode' instance that encodes based on a scheme supplied
-- directly to the constructor.  The most common use of this is to
-- supply a list which represents possible values.  Another
-- constructor allows a function to be directly supplied.
module Data.ArithEncode.Literal(
       LiteralArithEncode,
       literalEncodingFromList,
       literalEncoding
       ) where

import Data.Hashable
import Data.ArithEncode.Class

import qualified Data.Array as Array
import qualified Data.HashMap.Strict as Map

-- | Datatype representing a literal encoding.  This directly
-- specifies the 'encode'/'decode' pair, as well as the 'size'.
data LiteralArithEncode ty =
  LiteralArithEncode {
    -- | Size of the finite set.
    lSize :: Integer,
    -- | The function used to encode. 
    lEncode :: ty -> Integer,
    -- | The function used to decode.
    lDecode :: Integer -> ty
  }

-- | Construct an 'ArithEncode' by directly supplying the necessary
-- components: the 'encode' and 'decode' functions and the 'size'.
literalEncoding :: (ty -> Integer)
                -- ^ The 'encode' function.
                -> (Integer -> ty)
                -- ^ The 'decode' function.
                -> Integer
                -- ^ The 'size'
                -> LiteralArithEncode ty
literalEncoding encodefun decodefun count =
  LiteralArithEncode { lEncode = encodefun, lDecode = decodefun, lSize = count }

-- | Construct an 'ArithEncode' by supplying a list of possible
-- values.  An encoding will be constructed automatically.
literalEncodingFromList :: (Eq ty, Hashable ty)
                        => [ty]
                        -- ^ A list of distinct values from which to
                        -- build an encoding.
                        -> LiteralArithEncode ty
literalEncodingFromList vals =
  let
    len = length vals
    valarr = Array.listArray (0, len - 1) vals
    valmap = Map.fromList (map (\(a, b) -> (b, toInteger a))
                          (Array.assocs valarr))
  in
    LiteralArithEncode { lSize = toInteger len, lEncode = (Map.!) valmap,
                         lDecode = (Array.!) valarr . fromInteger }

instance ArithEncodeBound (LiteralArithEncode ty) where
  size = Just . lSize  

instance ArithEncode (LiteralArithEncode ty) ty where
  encode LiteralArithEncode { lEncode = func } = func
  decode LiteralArithEncode { lDecode = func } = func
