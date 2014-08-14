--- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
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

-- | Derived encodings for standard datatypes.
--
-- This module contains a number of useful constructions which can be
-- defined using the constructions from "Basic".
module Data.ArithEncode.Util(
       -- * Simple Encodings
       unit,

       -- * Non-Empty Containers
       nonEmptySeq,
       nonEmptySet,
       nonEmptyHashSet,

       -- * Functions and Relations
       function,
       functionHashable,
       relation,
       relationHashable,
{-
       hashMap,
       hashFunc,
       -}
       -- * Trees
       tree
       ) where

import Data.ArithEncode.Basic
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Set(Set)
import Data.Tree
import Prelude hiding (seq)

import qualified Data.HashMap as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | An encoding that produces @()@.
unit :: Encoding ()
unit = singleton ()

-- | Build an encoding that produces non-empty sequences from an
-- encoding for the elements of the sequence.
nonEmptySeq :: Encoding ty
            -- ^ The encoding for the element type
            -> Encoding [ty]
nonEmptySeq = nonzero . seq


-- | Build an encoding that produces non-empty sets from an encoding
-- for the elements of the set.
nonEmptySet :: Ord ty =>
               Encoding ty
            -- ^ The encoding for the element type
            -> Encoding (Set ty)
nonEmptySet = nonzero . set

-- | Build an encoding that produces non-empty hash sets from an encoding
-- for the elements of the set.
nonEmptyHashSet :: (Hashable ty, Ord ty) =>
                   Encoding ty
                -- ^ The encoding for the element type
                -> Encoding (HashSet.Set ty)
nonEmptyHashSet = nonzero . hashSet

-- | Build an encoding that produces a (finite partial) function from
-- one type to another.  This function is represented using a @Map@.
function :: (Ord keyty) =>
            Encoding keyty
         -- ^ The encoding for the domain type (ie. key type)
         -> Encoding valty
         -- ^ The encoding for the range type (ie. value type)
         -> Encoding (Map.Map keyty valty)
function keyenc valenc =
  let
    seqToMap val =
      let
        convertEnt (_, Nothing) = Nothing
        convertEnt (key', Just val') = Just (decode keyenc key', val')
      in
        Just (Map.fromList (catMaybes (map convertEnt (zip (iterate (+ 1) 0) val))))

    mapToSeq val
      | all (inDomain keyenc) (Map.keys val) =
        let
          foldfun (count, accum) (idx, val') =
            (idx + 1,
             Just val' : replicate (fromInteger (idx - count)) Nothing ++ accum)

          sorted = sortBy (\(a, _) (b, _) -> compare a b)
                          (map (\(key, val') -> (encode keyenc key, val'))
                               (Map.assocs val))

          (_, out) = foldl foldfun (0, []) sorted
        in
          Just (reverse out)
      | otherwise = Nothing
  in
    case size keyenc of
      Just finitesize ->
        wrap mapToSeq seqToMap (boundedSeq finitesize (optional valenc))
      Nothing -> 
        wrap mapToSeq seqToMap (seq (optional valenc))

-- | Build an encoding that produces a (finite partial) function from
-- one type to another.  This function is represented using a @HashMap@.
functionHashable :: (Ord keyty, Hashable keyty) =>
                    Encoding keyty
                 -- ^ The encoding for the domain type (ie. key type)
                 -> Encoding valty
                 -- ^ The encoding for the range type (ie. value type)
                 -> Encoding (HashMap.Map keyty valty)
functionHashable keyenc valenc =
  let
    seqToMap val =
      let
        convertEnt (_, Nothing) = Nothing
        convertEnt (key', Just val') = Just (decode keyenc key', val')
      in
        Just (HashMap.fromList (catMaybes (map convertEnt
                                               (zip (iterate (+ 1) 0) val))))

    mapToSeq val
      | all (inDomain keyenc) (HashMap.keys val) =
        let
          foldfun (count, accum) (idx, val') =
            (idx + 1,
             Just val' : replicate (fromInteger (idx - count)) Nothing ++ accum)

          sorted = sortBy (\(a, _) (b, _) -> compare a b)
                          (map (\(key, val') -> (encode keyenc key, val'))
                               (HashMap.assocs val))

          (_, out) = foldl foldfun (0, []) sorted
        in
          Just (reverse out)
      | otherwise = Nothing
  in
    wrap mapToSeq seqToMap (seq (optional valenc))

-- | Build an encoding that produces relations between two types.
-- These relations are represented as @Map@s from the first type to
-- @Set@s of the second.
relation :: (Ord keyty, Ord valty) =>
            Encoding keyty
         -- ^ The encoding for the left-hand type (ie. key type)
         -> Encoding valty
         -- ^ The encoding for the right-hand type (ie. value type)
         -> Encoding (Map.Map keyty (Set.Set valty))
relation keyenc = function keyenc . set

-- | Build an encoding that produces relations between two types.
-- These relations are represented as @HashMap@s from the first type to
-- @HashSet@s of the second.
relationHashable :: (Hashable keyty, Ord keyty, Hashable valty, Ord valty) =>
                    Encoding keyty
                 -- ^ The encoding for the left-hand type (ie. key type)
                 -> Encoding valty
                 -- ^ The encoding for the right-hand type (ie. value type)
                 -> Encoding (HashMap.Map keyty (HashSet.Set valty))
relationHashable keyenc = functionHashable keyenc . hashSet

-- | Build an encoding that produces trees from an encoding for the
-- node labels.
tree :: Encoding ty
     -- ^ The encoding for the node data type
     -> Encoding (Tree ty)
tree enc =
  let
    makeNode (label, children) =
      Just Node { rootLabel = label, subForest = children }

    unmakeNode Node { rootLabel = label, subForest = children } =
      Just (label, children)

    nodeEncoding nodeenc =
      wrap unmakeNode makeNode (pair enc (seq nodeenc))
  in
    recursive nodeEncoding
