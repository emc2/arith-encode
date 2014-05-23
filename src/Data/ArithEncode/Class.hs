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
{-# OPTIONS_GHC -Werror -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Classes for practical arithmetic encoding (Gödel numbering) and
-- associated functionality.  These interfaces are designed with the
-- following considerations:
--
-- * There is more than one way to encode a given datatype
-- * We may actually want to encode only a subset of a given datatype
-- * Encodings must be efficient (therefore, we assume that there is
--   some amount of precomputing that goes on)
-- * Encodings may be finite /or/ infinite.
-- * We may have a notion of "depth", which may be multidimensional.
--
-- This module provides the 'ArithEncode' class, which represents an
-- isomorphism between an arbitrary datatype and the natural numbers
-- (positive integers).  The 'encode' and 'decode' functions represent
-- the mapping to and from natural numbers respectively.
-- 'ArithEncode' is designed to allow for multiple mappings for a
-- given datatype, thus it takes an additional argument which
-- describes the encoding.
module Data.ArithEncode.Class(
       ArithEncode(..)
       ) where

-- | A class for isomorphisms between some type @ty@ and a range of
-- the natural numbers.  Instances must define 'encode' and 'decode'
-- with the following properties:
--
-- * @encode . decode == id@
-- * @encode a == encode b@ only if @a == b@
-- * @encode@ is defined for all valid inputs
-- * @encode@ only maps to positive 'Integer's, or @0@
--
-- The default implementation of 'size' assumes an infinite input
-- domain.  If the input domain is finite, then the instance should
-- redefine 'size' such that it returns @Just n@ for an input domain
-- of size @n@.  Additionally, the instance should ensure that
-- 'encode' always maps to 'Integer's less than @n@.
class ArithEncode iso ty where
  -- | Get the number of mappings in an isomorphism, or 'Nothing' if
  -- that number is infinite.
  size :: iso
       -- ^ The isomorphism in question.
       -> Maybe Integer
       -- ^ The number of mappings (also the size of the integer range
       -- mapped to), or 'Nothing' if it is infinite.

  -- | Convert an element of @ty@ into a natural number.  If the
  -- encoding is finite, the number will always be @>= 0@, and it will
  -- be < n if @size == Just n@.
  encode :: iso
         -- ^ Isomorphism-specific data.
         -> ty
         -- ^ The element to convert to a natural number
         -> Integer
         -- ^ The corresponding natural number.

  -- | Convert a natural number (represented as an @Integer@ to an
  -- element of @ty@.  The input must always be @>= 0@, and it must also
  -- be @< n@, if @size == Just n@.
  decode :: iso
         -- ^ Isomorphism-specific data.
         -> Integer
         -- ^ The integer to convert to an element of the type.
         -> ty
         -- ^ The corresponding element.
