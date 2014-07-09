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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Derived encodings for standard datatypes.
module Data.ArithEncode.Util(
       unit,
       nonEmptySeq,
       nonEmptySet,
       nonEmptyHashSet
       {-
       map,
       hashMap,
       func,
       hashFunc-}
       ) where

import Data.ArithEncode
import Data.Hashable
import Data.Set(Set)
import Prelude hiding (seq)

--import qualified Data.HashMap as HashMap
import qualified Data.HashSet as HashSet
--import qualified Data.Map as Map

-- | An encoding that produces @()@.
unit :: Encoding () ()
unit = singleton ()

-- | Build an encoding that produces non-empty sequences from an
-- encoding for the elements of the sequence.
nonEmptySeq :: Encoding dim ty -> Encoding (SeqDim dim) [ty]
nonEmptySeq = nonzero . seq

-- | Build an encoding that produces non-empty sets from an encoding
-- for the elements of the set.
nonEmptySet :: Ord ty => Encoding dim ty -> Encoding (SetDim dim) (Set ty)
nonEmptySet = nonzero . set

-- | Build an encoding that produces non-empty hash sets from an encoding
-- for the elements of the set.
nonEmptyHashSet :: (Hashable ty, Ord ty) =>
                   Encoding dim ty -> Encoding (SetDim dim) (HashSet.Set ty)
nonEmptyHashSet = nonzero . hashSet
