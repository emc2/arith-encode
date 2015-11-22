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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

-- | Definition of 'Encoding', and a set of fundamental 'Encoding's
-- and constructions.
--
-- This module contains the basic definitions for 'Encoding's.  It
-- defines the 'Encoding' type, the functions for creating an
-- 'Encoding', and a set of stock constructions.
--
-- The 'Encoding' type is encapsulated; the functions 'mkEncoding'
-- (and the variants thereof) are used to synthetically construct an
-- encoding from the fundamental operations.
--
-- The 'IllegalArgument' exception datatype, as well as the
-- fundamental operations are also defined here.
--
-- In addition to this, a set of basic definitions and constructions
-- are provided.  These definitions should be suitable for defining
-- 'Encoding's for most algebraic datatypes without having to manually
-- write encode/decode implementations.
module Data.ArithEncode.Basic(
       -- * Basic Definitions

       -- ** Constructors
       Encoding,
       mkEncoding,
       mkInfEncoding,

       -- ** Using Encodings
       IllegalArgument(..),
       encode,
       decode,
       size,
       inDomain,

       -- * Building Encodings

       -- ** Basic Encodings
       identity,
       singleton,
       integral,
       interval,
       fromHashableList,
       fromOrdList,

       -- ** Constructions

       -- *** Wrapping
       wrap,

       -- *** Optional
       optional,
       mandatory,

       -- *** Exclusion
       nonzero,
       exclude,

       -- *** Unions
       either,
       union,

       -- *** Products and Powers
       pair,
       triple,
       quad,
       quint,
       sextet,
       septet,
       octet,
       nonet,
       dectet,
       power,

       -- *** Sets
       set,
       hashSet,
--       exactSet,
--       boundedSet,

       -- *** Sequences
       seq,
       boundedSeq,

       -- *** Recursive
       recursive,
       recursive2,
       recursive3,
       recursive4,
       recursive5,
       recursive6,
       recursive7,
       recursive8,
       recursive9,
       recursive10
       ) where

import Control.Exception
import Control.Monad
import Data.Array.IArray(Array)
import Data.Bits
import Data.Hashable
import Data.List hiding (elem, union)
import Data.Maybe
import Data.Set(Set)
import Data.HashSet(HashSet)
import Data.Typeable
import Prelude hiding (elem, either, seq)
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Logarithms
import Data.Word
--import Debug.Trace

import qualified Data.Array.IArray as Array
import qualified Data.Either as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | An exception to be thrown if an illegal argument is given to
-- 'encode', 'decode'.
data IllegalArgument = IllegalArgument !String
  deriving Typeable

instance Show IllegalArgument where
  show (IllegalArgument "") = "Illegal argument"
  show (IllegalArgument s) = "Illegal argument: " ++ s

instance Exception IllegalArgument

-- | Type for an encoding.  The structure of this type is deliberately
-- hidden from users.  Use the 'mkEncoding' functions to construct
-- 'Encoding's, and the seven functions to use them.
data Encoding ty =
  Encoding {
    -- | Encode a @ty@ as a positive integer.
    encEncode :: ty -> Integer,
    -- | Decode a positive integer into a @ty@.
    encDecode :: Integer -> ty,
    -- | The size of an encoding, or 'Nothing' if it is infinite.
    encSize :: !(Maybe Integer),
    -- | Indicate whether or not a value is in the domain of the encoding.
    encInDomain :: ty -> Bool
  }

-- | Create an encoding from all the necessary components.
mkEncoding :: (ty -> Integer)
           -- ^ The encoding function.
           -> (Integer -> ty)
           -- ^ The decoding function.  Can assume all inputs are positive.
           -> Maybe Integer
           -- ^ The number of mappings, or 'Nothing' if it is infinite.
           -> (ty -> Bool)
           -- ^ A function indicating whether or not a given value is
           -- in the domain of values.
           -> Encoding ty
mkEncoding encodefunc decodefunc sizeval indomain =
  Encoding { encEncode = encodefunc, encDecode = decodefunc,
             encSize = sizeval, encInDomain = indomain }

-- | Create an infinite-sized encoding.  This variant does not need a
-- size.
mkInfEncoding :: (ty -> Integer)
              -- ^ The encoding function.
              -> (Integer -> ty)
              -- ^ The decoding function.  Can assume all inputs are positive.
              -> (ty -> Bool)
              -- ^ A function indicating whether or not a given value is
              -- in the domain of values.
              -> Encoding ty
mkInfEncoding encodefunc decodefunc indomain =
  mkEncoding encodefunc decodefunc Nothing indomain

-- | Encode a @ty@ as a positive 'Integer' (ie. a natural number).
--
-- If the given @ty@ is not in the domain of the 'Encoding' (meaning,
-- 'inDomain' returns 'False'), the underlying implementation /may/
-- throw 'IllegalArgument'.  However, this is not /strictly/ required;
-- therefore, do not rely on 'IllegalArgument' being thrown.
encode :: Encoding ty
       -- ^ Encoding to use.
       -> ty
       -- ^ Value to encode.
       -> Integer
       -- ^ Encoded value.
encode encoding = encEncode encoding

-- | Decode a @ty@ from a positive 'Integer' (ie. a natural number).
--
-- If the given 'Integer' is out of bounds (ie. it is bigger than
-- 'size'), the underlying implementation /may/ throw
-- 'IllegalArgument'.  However, this not /strictly/ required;
-- therefore, do not rely on 'IllegalArgument' being thrown.
decode :: Encoding ty
       -- ^ Encoding to use.
       -> Integer
       -- ^ Number to decode.
       -> ty
       -- ^ Decoded value.
decode encoding num
  | num < 0 =
    throw (IllegalArgument ("decode argument " ++ show num ++ " is negative"))
  | maybe False (<= num) (size encoding) =
    throw (IllegalArgument ("decode argument " ++ show num ++
                            " is out of bounds"))
  | otherwise = (encDecode encoding) num

-- | Get the size of an 'Encoding', or 'Nothing' if it is infinite.
size :: Encoding ty
     -- ^ Encoding to use.
     -> Maybe Integer
     -- ^ Number of values mapped, or 'Nothing' for infinity.
size = encSize

-- | Indicate whether or not a value is in the domain of the encoding.
inDomain :: Encoding ty
         -- ^ Encoding to use.
         -> ty
         -- ^ Value to query.
         -> Bool
         -- ^ Whether or not the value is in the domain of the encoding.
inDomain encoding = encInDomain encoding

-- | The identity encoding.  Maps every positive 'Integer' to itself.
--
-- Note: only positive integers are in the domain of this encoding.
-- For all an encoding whose domain is all integers, use 'integral'.
identity :: Encoding Integer
identity = mkInfEncoding id id (>= 0)

-- | A singleton encoding.  Maps a singular value to 0.
singleton :: Eq ty => ty -> Encoding ty
singleton val = mkEncoding (const 0) (const val) (Just 1) (val ==)

-- | An encoding of /all/ integers.
--
-- Note: this is /not/ an identity mapping.
integral :: Integral n => Encoding n
integral =
  let
    encodefunc num
      | num < 0 = ((abs (toInteger num) - 1) `shiftL` 1) `setBit` 0
      | otherwise = (toInteger num) `shiftL` 1

    decodefunc num
      | num `testBit` 0 = fromInteger (-((num `shiftR` 1) + 1))
      | otherwise = fromInteger (num `shiftR` 1)
  in
    mkInfEncoding encodefunc decodefunc (const True)

-- | Build an encoding from a finite range of 'Integral's.
--
-- Both the upper and lower bounds are inclusive.  This allows an
-- 'Encoding' to be created for bounded integer datatypes, such as
-- 'Int8'.
interval :: Integral n
         => n
         -- ^ The (inclusive) lower bound on the range.
         -> n
         -- ^ The (inclusive) upper bound on the range.
         -> Encoding n
interval lower upper
  | lower <= upper =
    let
      biglower = toInteger lower
      encodefunc num = (toInteger num) - biglower
      decodefunc num = fromInteger (num + biglower)
      sizeval = Just ((toInteger upper) - (toInteger lower) + 1)
      indomainfunc val = lower <= val && val <= upper
    in
       mkEncoding encodefunc decodefunc sizeval indomainfunc
  | otherwise = error "Lower bound is not less than upper bound"

-- | Build an encoding from a list of items with a 'Hashable' instance.
fromHashableList :: forall ty. (Hashable ty, Ord ty)
                 => [ty]
                 -- ^ A list of items to encode.
                 -> Encoding ty
                 -- ^ An encoding mapping the items in the list to
                 -- natural numbers.
fromHashableList elems =
  let
    len = fromIntegral (length elems)

    revmap :: Array Data.Word.Word ty
    revmap = Array.listArray (0, len) elems

    fwdmap = HashMap.fromList (zip elems [0..len])
    encodefunc = toInteger . (HashMap.!) fwdmap
    decodefunc = (Array.!) revmap . fromInteger
    sizeval = Just (toInteger len)
    indomainfunc = (flip HashMap.member) fwdmap
  in
    mkEncoding encodefunc decodefunc sizeval indomainfunc

-- | Build an encoding from a list of items with an 'Ord' instance.
fromOrdList :: forall ty . Ord ty
            => [ty]
            -- ^ A list of items to encode.
            -> Encoding ty
            -- ^ An encoding mapping the items in the list to natural
            -- numbers.
fromOrdList elems =
  let
    len = fromIntegral (length elems)

    revmap :: Array Word ty
    revmap = Array.listArray (0, len) elems

    fwdmap = Map.fromList (zip elems [0..len])
    encodefunc = toInteger . (Map.!) fwdmap
    decodefunc = (Array.!) revmap . fromInteger
    sizeval = Just (toInteger len)
    indomainfunc = (flip Map.member) fwdmap
  in
    mkEncoding encodefunc decodefunc sizeval indomainfunc

-- | Wrap an encoding using a pair of functions.  These functions must
-- also define an isomorphism.
wrap :: (a -> Maybe b)
     -- ^ The forward encoding function.
     -> (b -> Maybe a)
     -- ^ The reverse encoding function.
     -> Encoding b
     -- ^ The inner encoding.
     -> Encoding a
wrap fwd rev enc @ Encoding { encEncode = encodefunc, encDecode = decodefunc,
                              encInDomain = indomainfunc } =
  let
    safefwd val =
      case fwd val of
        Just val' -> val'
        Nothing -> throw (IllegalArgument "No mapping into underlying domain")

    saferev val =
      case rev val of
        Just val' -> val'
        Nothing -> throw (IllegalArgument "No mapping into external domain")
  in
    enc { encEncode = encodefunc . safefwd,
          encDecode = saferev . decodefunc,
          encInDomain = maybe False indomainfunc . fwd }

-- | Generate an encoding for @Maybe ty@ from an inner encoding for
-- @ty@.
optional :: Encoding ty -> Encoding (Maybe ty)
optional Encoding { encEncode = encodefunc, encDecode = decodefunc,
                    encSize = sizeval, encInDomain = indomainfunc } =
  let
    newsize = sizeval >>= return . (+ 1)
    newindomain = maybe True indomainfunc

    newencode Nothing = 0
    newencode (Just val) = 1 + encodefunc val

    newdecode 0 = Nothing
    newdecode num = Just (decodefunc (num - 1))
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = newsize, encInDomain = newindomain }

-- | The dual of @optional@.  This construction assumes that @Nothing@
-- maps to @0@, and removes it from the input domain.
--
-- Using this construction on encodings for @Maybe ty@ which are not
-- produced by @optional@ may have unexpected results.
mandatory :: Encoding (Maybe ty) -> Encoding ty
mandatory Encoding { encEncode = encodefunc, encDecode = decodefunc,
                     encSize = sizeval, encInDomain = indomainfunc } =
  let
    dec n = n - 1
    newencode = dec . encodefunc . Just
    newdecode = fromJust . decodefunc . (+ 1)
    newsize = sizeval >>= return . dec
    newindomain = indomainfunc . Just
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = newsize, encInDomain = newindomain }

-- | Removes the mapping to @0@ (ie. the first mapping).  This has the
-- same effect as @exclude [x]@, where @x@ is the value that maps to
-- @0@.  It is also similar to @mandatory@, except that it does not
-- change the base type.
nonzero :: Encoding ty -> Encoding ty
nonzero enc @ Encoding { encEncode = encodefunc, encDecode = decodefunc,
                         encSize = sizeval, encInDomain = indomainfunc } =
  let
    dec n = n - 1
    newencode = dec . encodefunc
    newdecode = decodefunc . (+ 1)
    newsize = sizeval >>= return . dec
    newindomain val = indomainfunc val && 0 /= encodefunc val
  in
    enc { encEncode = newencode, encDecode = newdecode,
          encSize = newsize, encInDomain = newindomain }

-- | A simple binary tree structure, for use with exclude.
data BinTree key val =
    Branch key val (BinTree key val) (BinTree key val)
  | Nil
    deriving Show

-- | Find the tree node with the highest index less than the given key
-- and return its data.
closestBelow :: Ord key => key -> BinTree key val -> Maybe (key, val)
closestBelow target =
  let
    closestBelow' out Nil = out
    closestBelow' out (Branch k v left right) =
      case compare k target of
        LT -> closestBelow' (Just (k, v)) right
        _ -> closestBelow' out left
  in
    closestBelow' Nothing

-- | Simple binary tree lookup, for use with exclude.
closestWithin :: Ord key => key -> BinTree key val -> Maybe (key, val)
closestWithin target =
  let
    closestWithin' out Nil = out
    closestWithin' out (Branch k v left right) =
      case compare k target of
        GT -> closestWithin' out left
        _ -> closestWithin' (Just (k, v)) right
  in
    closestWithin' Nothing

-- | Convert a list to a binary tree, for use with excludes.
toBinTree :: [(key, val)] -> BinTree key val
toBinTree vals =
  let
    toBinTree' 0 [] = Nil
    toBinTree' 0 _ = error "Zero size with non-empty list"
    toBinTree' _ [] = error "Empty list with non-zero size"
    toBinTree' len vals' =
      let
        halflo = len `shiftR` 1
        halfhi = len - halflo
        (lows, (k, v) : highs) = splitAt halflo vals'
        left = toBinTree' halflo lows
        right = toBinTree' (halfhi - 1) highs
      in
        Branch k v left right
  in
    toBinTree' (length vals) vals

-- | Removes the mapping to the items in the list.  The resulting
-- @encode@, @decode@, and @highestIndex@ are O(@length excludes@), so
-- this should only be used with a very short excludes list.
exclude :: [ty]
        -- ^ The list of items to exclude.
        -> Encoding ty
        -- ^ The base @Encoding@.
        -> Encoding ty
exclude [] enc = enc
exclude excludes enc @ Encoding { encEncode = encodefunc, encDecode = decodefunc,
                                  encSize = sizeval, encInDomain = indomainfunc } =
  let
    forbidden = HashSet.fromList (map encodefunc excludes)
    sortedlist = sort (map encodefunc excludes)

    fwdoffsets :: [(Integer, Integer)]
    (_, fwdoffsets) = mapAccumL (\offset v -> (offset + 1, (v, offset)))
                                1 sortedlist
    fwdtree = toBinTree fwdoffsets

    revoffsets :: [(Integer, Integer)]
    revoffsets =
      let
        foldfun :: [(Integer, Integer)] -> (Integer, Integer) ->
                   [(Integer, Integer)]
        foldfun accum @ ((v', _) : rest) elem @ (v, _)
          | v == v' = elem : rest
          | otherwise = elem : accum
        foldfun _ _ = error "Should not fold over an empty list"

        (first : adjusted) =
          map (\(v, offset) -> (v - (offset - 1), offset)) fwdoffsets
      in
        reverse (foldl foldfun [first] adjusted)

    revtree = toBinTree revoffsets

    toExcluded n =
      case closestBelow n fwdtree of
        Just (_, offset) -> n - offset
        Nothing -> n

    fromExcluded n =
      case closestWithin n revtree of
        Just (_, offset) -> n + offset
        Nothing -> n

    newEncode = toExcluded . encodefunc
    newDecode = decodefunc . fromExcluded

    newSize =
      do
        n <- sizeval
        return $! (n - (toInteger (length excludes)))

    newInDomain val =
      indomainfunc val && not (HashSet.member (encodefunc val) forbidden)
  in
    enc { encEncode = newEncode, encDecode = newDecode,
          encSize = newSize, encInDomain = newInDomain }

-- | Combine two encodings into a single encoding that returns an
-- @Either@ of the two types.
either :: Encoding ty1
       -- ^ The @Encoding@ that will be represented by @Left@.
       -> Encoding ty2
       -- ^ The @Encoding@ that will be represented by @Right@.
       -> Encoding (Either ty1 ty2)
either Encoding { encEncode = encode1, encDecode = decode1,
                  encInDomain = indomain1, encSize = sizeval1 }
       Encoding { encEncode = encode2, encDecode = decode2,
                  encInDomain = indomain2, encSize = sizeval2 } =
  let
    -- There are three cases here, depending on the size of the two
    -- mappings.  This does replicate code, but it also does a lot of
    -- figuring things when the encoding is created as opposed to
    -- later.
    (isLeft, leftIdxFwd, rightIdxFwd, leftIdxRev, rightIdxRev) =
      case (sizeval1, sizeval2) of
        -- Simplest case: both mappings are infinite.  Map all the
        -- evens to the left, and all the odds to the right.
        (Nothing, Nothing) ->
          (\num -> not (testBit num 0),
           \idx -> idx `shiftL` 1,
           \idx -> setBit (idx `shiftL` 1) 0,
           \idx -> idx `shiftR` 1,
           \idx -> idx `shiftR` 1)
        -- Left is smaller: do the even/odd mapping until we exhaust
        -- the left, then just map directly to the right.
        (Just size1, _) | maybe True (size1 <) sizeval2 ->
          let
            size1shifted = (size1 `shiftL` 1)
            isLeft' num = num < size1shifted && not (testBit num 0)
            leftIdxFwd' idx = idx `shiftL` 1

            rightIdxFwd' idx
              | size1 <= idx = size1shifted + (idx - size1)
              | otherwise = setBit (idx `shiftL` 1) 0

            leftIdxRev' idx = idx `shiftR` 1

            rightIdxRev' idx
              | size1shifted <= idx = size1 + (idx - size1shifted)
              | otherwise = idx `shiftR` 1
          in
            (isLeft', leftIdxFwd', rightIdxFwd', leftIdxRev', rightIdxRev')
        -- Right is smaller: do the even/odd mapping until we exhaust
        -- the right, then just map directly to the left.
        (_, Just size2) ->
          let
            size2shifted = (size2 `shiftL` 1)
            isLeft' num = num > size2shifted || not (testBit num 0)

            leftIdxFwd' idx
              | size2 <= idx = size2shifted + (idx - size2)
              | otherwise = idx `shiftL` 1

            rightIdxFwd' idx = setBit (idx `shiftL` 1) 0

            leftIdxRev' idx
              | size2shifted <= idx = size2 + (idx - size2shifted)
              | otherwise = idx `shiftR` 1

            rightIdxRev' idx = idx `shiftR` 1
          in
            (isLeft', leftIdxFwd', rightIdxFwd', leftIdxRev', rightIdxRev')
        _ -> error "This case should never happen"

    newSize =
      do
        size1 <- sizeval1
        size2 <- sizeval2
        return (size1 + size2)

    eitherIndex lfunc rfunc idx
      | isLeft idx = lfunc (leftIdxRev idx)
      | otherwise = rfunc (rightIdxRev idx)

    newEncode = Either.either (leftIdxFwd . encode1) (rightIdxFwd . encode2)
    newDecode = eitherIndex (Left . decode1) (Right . decode2)

    newInDomain = Either.either indomain1 indomain2
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain }

sortfunc :: Maybe Integer -> Maybe Integer -> Ordering
sortfunc Nothing Nothing = EQ
sortfunc Nothing _ = GT
sortfunc _ Nothing = LT
sortfunc (Just a) (Just b) = compare a b

-- | Combine a set of encodings with the result type into a single
-- encoding which represents the disjoint union of the components.
union :: forall ty.
         [Encoding ty]
      -- ^ The components of the union.
      -> Encoding ty
union [] = error "union encoding with no arguments"
union encodings =
  let
    numelems :: Int
    numelems = length encodings

    sortpair (a, _) (b, _) = sortfunc a b

    (sizes, sortedencodings) =
      unzip (sortBy sortpair (map (\enc -> (size enc, enc)) encodings))
    -- Turn the sorted element encodings into an array for fast access
    encodingarr :: Array.Array Int (Encoding ty)
    encodingarr = Array.listArray (0, numelems - 1) sortedencodings

    (fwdmapnum, revmapnum) =
      let
        -- An ordered list of the sizes of isomorphisms and how far into
        -- the array to start.
        sizeclasses =
          let
            foldfun (ind, accum) elemsize =
              case accum of
                (elemsize', _) : _ | elemsize == elemsize' ->
                  (ind + 1, accum)
                _ -> (ind + 1, (elemsize, ind) : accum)

            (_, out) = foldl foldfun (0, []) sizes
          in
            reverse out

        -- The mapping functions used to encode within a single size
        -- class.
        fwdmapbasic base width num enc =
          let
            adjustedenc = enc - (numelems - width)
          in
            ((num * toInteger width) + (toInteger adjustedenc) + base)
        revmapbasic base width num
          | (fromInteger num) < width =
            let
              adjustedenc = fromInteger num + (numelems - width)
            in
              (base, adjustedenc)
          | otherwise = ((num `quot` toInteger width) + base,
                         fromInteger (num `mod` toInteger width) +
                         (numelems - width))
      in case sizeclasses of
        -- If there is only one size class, then 
        [ _ ] -> (fwdmapbasic 0 numelems, revmapbasic 0 numelems)
        (Just firstsize, _) : rest  ->
          let
            (fwdtree, revtree) =
              let
                foldfun (lastsize, offset, fwds, revs) (Nothing, idx) =
                  let
                    thisnumencs = numelems - idx
                  in
                    (undefined, undefined,
                     (lastsize, (offset, thisnumencs)) : fwds,
                     (offset, (lastsize, thisnumencs)) : revs)
                foldfun (lastsize, offset, fwds, revs) (Just thissize, idx) =
                  let
                    thisnumencs = numelems - idx
                    sizediff = thissize - lastsize
                  in
                    (thissize, offset + (sizediff * toInteger thisnumencs),
                     (lastsize, (offset, thisnumencs)) : fwds,
                     (offset, (lastsize, thisnumencs)) : revs)

                (_, _, fwdvals, revvals) =
                  foldl foldfun
                        (firstsize, (firstsize * toInteger numelems), [], [])
                        rest
              in
                (toBinTree (reverse fwdvals), toBinTree (reverse revvals))

            fwdmap num enc =
              case closestWithin num fwdtree of
                Nothing -> fwdmapbasic 0 numelems num enc
                Just (sizeclass, (base, numencs)) ->
                  fwdmapbasic base numencs (num - sizeclass) enc

            revmap num =
              case closestWithin num revtree of
                Nothing -> revmapbasic 0 numelems num
                Just (offset, (base, numencs)) ->
                  revmapbasic base numencs (num - offset)
          in
            (fwdmap, revmap)
        _ -> error "Internal error"

    encodefunc val =
      case findIndex ((flip inDomain) val) sortedencodings of
        Just encidx ->
          let
            enc = (Array.!) encodingarr encidx
            num = encode enc val
          in
           fwdmapnum num encidx
        Nothing -> throw (IllegalArgument "Value not in domain of any component")

    decodefunc num =
      let
        (encnum, encidx) = revmapnum num
        encoding = (Array.!) encodingarr encidx
      in
        decode encoding encnum

    -- Sum up all the sizes, going to infinity if one of them in
    -- infinite
    sizeval =
      let
        foldfun accum n =
          do
            accumval <- accum
            nval <- n
            return (nval + accumval)
      in
        foldl foldfun (Just 0) sizes

    indomainfunc val = any ((flip inDomain) val) sortedencodings
  in
    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc }

isqrt :: Integer -> Integer
isqrt = integerSquareRoot'

mkPairCore :: Encoding ty1 -> Encoding ty2 ->
              ((ty1, ty2) -> Integer, Integer -> (ty1, ty2), Maybe Integer)
mkPairCore Encoding { encEncode = encode1, encDecode = decode1,
                      encSize = sizeval1 }
           Encoding { encEncode = encode2, encDecode = decode2,
                      encSize = sizeval2 } =
  let
    (convertidx, decodefunc) = case (sizeval1, sizeval2) of
      (Just maxval, _) ->
        let
          convertidx' idx1 idx2 = (idx2 * maxval) + idx1
          newdecode num = (decode1 (num `mod` maxval), decode2 (num `quot` maxval))
        in
          (convertidx', newdecode)
      (_, Just maxval) ->
        let
          convertidx' idx1 idx2 = (idx1 * maxval) + idx2
          newdecode num = (decode1 (num `quot` maxval), decode2 (num `mod` maxval))
        in
          (convertidx', newdecode)
      (Nothing, Nothing) ->
        let
          convertidx' idx1 idx2 =
            let
              sumval = idx1 + idx2
              base = (((sumval + 1) * sumval)) `quot` 2
            in
              base + idx2

          newdecode num =
            let
              sumval = (isqrt ((8 * num) + 1) - 1) `quot` 2
              base = (((sumval + 1) * sumval)) `quot` 2
              num2 = num - base
              num1 = sumval - num2
            in
              (decode1 num1, decode2 num2)
        in
          (convertidx', newdecode)

    encodefunc (val1, val2) = convertidx (encode1 val1) (encode2 val2)

    sizeval =
      do
        size1 <- sizeval1
        size2 <- sizeval2
        return (size1 * size2)
  in
    (encodefunc, decodefunc, sizeval)

-- | Take encodings for two datatypes A and B, and build an encoding
-- for a pair (A, B).
pair :: Encoding ty1 -> Encoding ty2 -> Encoding (ty1, ty2)
pair enc1 @ Encoding { encInDomain = indomain1 }
     enc2 @ Encoding { encInDomain = indomain2 } =
  let
    (encodefunc, decodefunc, sizeval) = mkPairCore enc1 enc2

    indomainfunc (val1, val2) = indomain1 val1 && indomain2 val2
  in
    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc }


-- | Construct an encoding for a 3-tuple from the encodings for the
-- three components.  This is actually just a wrapper around @pair@.
triple :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 ->
          Encoding (ty1, ty2, ty3)
triple enc1 enc2 enc3 =
  let
    fwdshuffle (val1, val2, val3) = ((val1, val2), val3)
    revshuffle ((val1, val2), val3) = (val1, val2, val3)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair enc1 enc2) enc3

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }

-- | Construct an encoding for a 4-tuple from the encodings for the
-- four components.  This is actually just a wrapper around @pair@.
quad :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 -> Encoding ty4 ->
        Encoding (ty1, ty2, ty3, ty4)
quad enc1 enc2 enc3 enc4 =
  let
    fwdshuffle (val1, val2, val3, val4) = ((val1, val2), (val3, val4))
    revshuffle ((val1, val2), (val3, val4)) = (val1, val2, val3, val4)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair enc1 enc2) (pair enc3 enc4)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }


-- | Construct an encoding for a 5-tuple from the encodings for the
-- five components.  This is actually just a wrapper around @pair@.
quint :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 ->
         Encoding ty4 -> Encoding ty5 ->
         Encoding (ty1, ty2, ty3, ty4, ty5)
quint enc1 enc2 enc3 enc4 enc5 =
  let
    fwdshuffle (val1, val2, val3, val4, val5) = (((val1, val2), val3), (val4, val5))
    revshuffle (((val1, val2), val3), (val4, val5)) = (val1, val2, val3, val4, val5)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair (pair enc1 enc2) enc3) (pair enc4 enc5)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }

-- | Construct an encoding for a 6-tuple from the encodings for the
-- six components.  This is actually just a wrapper around @pair@.
sextet :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 ->
          Encoding ty4 -> Encoding ty5 -> Encoding ty6 ->
          Encoding (ty1, ty2, ty3, ty4, ty5, ty6)
sextet enc1 enc2 enc3 enc4 enc5 enc6 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6) =
      (((val1, val2), val3), ((val4, val5), val6))
    revshuffle (((val1, val2), val3), ((val4, val5), val6)) =
      (val1, val2, val3, val4, val5, val6)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair (pair enc1 enc2) enc3) (pair (pair enc4 enc5) enc6)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }

-- | Construct an encoding for a 7-tuple from the encodings for the
-- seven components.  This is actually just a wrapper around @pair@.
septet :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 -> Encoding ty4 ->
          Encoding ty5 -> Encoding ty6 -> Encoding ty7 ->
          Encoding (ty1, ty2, ty3, ty4, ty5, ty6, ty7)
septet enc1 enc2 enc3 enc4 enc5 enc6 enc7 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7) =
      (((val1, val2), (val3, val4)), ((val5, val6), val7))
    revshuffle (((val1, val2), (val3, val4)), ((val5, val6), val7)) =
      (val1, val2, val3, val4, val5, val6, val7)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair (pair enc1 enc2) (pair enc3 enc4)) (pair (pair enc5 enc6) enc7)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }

-- | Construct an encoding for an 8-tuple from the encodings for the
-- eight components.  This is actually just a wrapper around @pair@.
octet :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 ->
         Encoding ty4 -> Encoding ty5 -> Encoding ty6 ->
         Encoding ty7 -> Encoding ty8 ->
         Encoding (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8)
octet enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8) =
      (((val1, val2), (val3, val4)), ((val5, val6), (val7, val8)))
    revshuffle (((val1, val2), (val3, val4)), ((val5, val6), (val7, val8))) =
      (val1, val2, val3, val4, val5, val6, val7, val8)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair (pair enc1 enc2) (pair enc3 enc4))
           (pair (pair enc5 enc6) (pair enc7 enc8))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }

-- | Construct an encoding for a 9-tuple from the encodings for the
-- nine components.  This is actually just a wrapper around @pair@.
nonet :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 -> Encoding ty4 ->
         Encoding ty5 -> Encoding ty6 -> Encoding ty7 ->
         Encoding ty8 -> Encoding ty9 ->
         Encoding (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8, ty9)
nonet enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8, val9) =
      ((((val1, val2), val3), (val4, val5)), ((val6, val7), (val8, val9)))
    revshuffle ((((val1, val2), val3), (val4, val5)), ((val6, val7), (val8, val9))) =
      (val1, val2, val3, val4, val5, val6, val7, val8, val9)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair (pair (pair enc1 enc2) enc3) (pair enc4 enc5))
           (pair (pair enc6 enc7) (pair enc8 enc9))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }

-- | Construct an encoding for a 10-tuple from the encodings for the
-- ten components.  This is actually just a wrapper around @pair@.
dectet :: Encoding ty1 -> Encoding ty2 -> Encoding ty3 -> Encoding ty4 ->
          Encoding ty5 -> Encoding ty6 -> Encoding ty7 ->
          Encoding ty8 -> Encoding ty9 -> Encoding ty10 ->
          Encoding (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8, ty9, ty10)
dectet enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10) =
      ((((val1, val2), val3), (val4, val5)), (((val6, val7), val8), (val9, val10)))
    revshuffle ((((val1, val2), val3), (val4, val5)),
                (((val6, val7), val8), (val9, val10))) =
      (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
      pair (pair (pair (pair enc1 enc2) enc3) (pair enc4 enc5))
           (pair (pair (pair enc6 enc7) enc8) (pair enc9 enc10))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain }

-- | Common idiom in bounded sets and sequences: take an entropy value
-- and generate a list of entropy values of a particular length.
toProdList :: Integer -> Integer -> [Integer]
toProdList =
  let
    productList' accum 1 entropy = reverse (entropy : accum)
    productList' _ 0 _ = []
    productList' accum count entropy =
      let
        sumval = (isqrt ((8 * entropy) + 1) - 1) `quot` 2
        base = (((sumval + 1) * sumval)) `quot` 2
        num2 = entropy - base
        num1 = sumval - num2
      in
        productList' (num1 : accum) (count - 1) num2
  in
    productList' []

fromProdList :: [Integer] -> Integer
fromProdList [] = 0
fromProdList vals =
  let
    (first : rest) = reverse vals
    fromProdList' accum [] = accum
    fromProdList' accum (first' : rest') =
      let
        sumval = accum + first'
        base = (((sumval + 1) * sumval)) `quot` 2
      in
        fromProdList' (base + accum) rest'
  in
    fromProdList' first rest

-- | Take an @Encoding@ for elements and a length and produce an
-- @Encoding@ for lists of exactly that length.
--
-- This differs from 'boundedSeq' in that the resulting list is
-- /exactly/ the given length, as opposed to upper-bounded by it.
power :: Integer
      -- ^ Number of elements in the resulting lists
      -> Encoding ty
      -- ^ @Encoding@ for the elements
      -> Encoding [ty]
power len Encoding { encEncode = encodefunc, encDecode = decodefunc,
                     encSize = sizeval, encInDomain = indomainfunc } =
  let
    (newencode, newdecode, newsize) =
      case sizeval of
        Just finitesize ->
          let
            newencode' accum [] = accum
            newencode' accum (first : rest) =
              newencode' ((accum * finitesize) + encodefunc first) rest

            newdecode' accum 1 entropy = (decodefunc entropy : accum)
            newdecode' _ 0 _ = []
            newdecode' accum count entropy =
              let
                thisentropy = entropy `mod` finitesize
                restentropy = entropy `quot` finitesize
                this = decodefunc thisentropy
              in
                newdecode' (this : accum) (count - 1) restentropy
          in
            (newencode' 0, newdecode' [] len, Just (finitesize ^ len))
        Nothing ->
          let
            newencode' = fromProdList . map encodefunc
            newdecode' = map decodefunc . toProdList len
          in
            (newencode', newdecode', Nothing)

    newindomain vals = length vals == fromInteger len && all indomainfunc vals
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = newsize, encInDomain = newindomain }

-- | Build an encoding for /finite/ sets of values of a given datatype
-- from an encoding for that datatype.
--
-- Note: this encoding and its variants can produce very large numbers
-- for a very small set.
set :: Ord ty => Encoding ty -> Encoding (Set ty)
set Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc } =
  let
    newEncode = Set.foldl (\n -> setBit n . fromInteger . encodefunc) 0

    newDecode =
      let
        decode' out _ 0 = out
        decode' out idx n
          | testBit n 0 =
            decode' (Set.insert (decodefunc idx) out) (idx + 1) (n `shiftR` 1)
          | otherwise = decode' out (idx + 1) (n `shiftR` 1)
      in
        decode' Set.empty 0

    newSize =
      do
        elems <- sizeval
        return (2 ^ elems)

    newInDomain = all indomainfunc . Set.toList
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain }

-- | Build an encoding for /finite/ sets of values of a given datatype
-- from an encoding for that datatype.  Similar to @set@, but uses
-- @HashSet@ instead
hashSet :: (Hashable ty, Ord ty) =>
           Encoding ty -> Encoding (HashSet ty)
hashSet Encoding { encEncode = encodefunc, encDecode = decodefunc,
                   encSize = sizeval, encInDomain = indomainfunc } =
  let
    newEncode =
      HashSet.foldr (\elem n -> setBit n (fromInteger (encodefunc elem))) 0

    newDecode =
      let
        decode' out _ 0 = out
        decode' out idx n
          | testBit n 0 =
            decode' (HashSet.insert (decodefunc idx) out)
                    (idx + 1) (n `shiftR` 1)
          | otherwise = decode' out (idx + 1) (n `shiftR` 1)
      in
        decode' HashSet.empty 0

    newSize =
      do
        elems <- sizeval
        return (2 ^ elems)

    newInDomain = all indomainfunc . HashSet.toList
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain }

seqCore :: Encoding ty -> ([ty] -> Integer, Integer -> [ty])
seqCore Encoding { encEncode = encodefunc, encDecode = decodefunc,
                   encSize = sizeval } =
  case sizeval of
    -- For encodings with a maximum size s, a list with n elements
    -- e_i is encoded as e_n + s e_(n-1) + ... s^n e_1
    Just finitesize ->
      let
        newencodefunc =
          let
            foldfun accum = (((accum * finitesize) + 1) +) . encodefunc
          in
           foldl foldfun 0

        newdecodefunc =
          let
            newdecodefunc' accum 0 = accum
            newdecodefunc' accum num =
              let
                decoded = decodefunc ((num - 1) `mod` finitesize)
              in
               newdecodefunc' (decoded : accum) ((num - 1) `quot` finitesize)
          in
           newdecodefunc' []
      in
        (newencodefunc, newdecodefunc)
    -- For encodings with no maximum size, we use a dovetailing approach.
    Nothing ->
      let
        newencodefunc [] = 0
        newencodefunc (first : rest) =
          let
            insertUnary bin val =
              let
                encoded = encodefunc val
                shifted = bin `shiftL` (fromInteger encoded)
              in
               shifted .|. ((2 ^ encoded) - 1)

            foldfun accum val =
              let
                shifted = accum `shiftL` 1
              in
               insertUnary shifted val

            initial = insertUnary 1 first
          in
           foldl foldfun initial rest

        newdecodefunc 0 = []
        newdecodefunc num =
          let
            -- Count leading ones
            leadingOnes :: Integer -> Integer
            leadingOnes =
              let
                leadingOnes' count n
                  | testBit n 0 = leadingOnes' (count + 1) (n `shiftR` 1)
                  | otherwise = count
              in
               leadingOnes' 0

            extractUnary bin =
              let
                unaryLen = leadingOnes bin
                shifted = bin `shiftR` (fromInteger (unaryLen + 1))
                decoded
                  | shifted /= 0 = decodefunc unaryLen
                  | otherwise = decodefunc (unaryLen - 1)
              in
               (decoded, shifted)

            doDecode accum 0 = accum
            doDecode accum bin =
              let
                (val, newbin) = extractUnary bin
              in
               doDecode (val : accum) newbin
          in
           doDecode [] num
      in
        (newencodefunc, newdecodefunc)

-- | Construct an encoding for /finite/ sequences of a type from an
-- encoding for values of that type.
--
-- Note: This encoding can produce very large numbers for short
-- sequences.
seq :: Encoding ty -> Encoding [ty]
seq enc @ Encoding { encInDomain = indomainfunc } =
  let
    (newEncode, newDecode) = seqCore enc
    newInDomain = all indomainfunc
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = Nothing, encInDomain = newInDomain }

-- | Sum of finite geometric series
geometricSum :: Integer -> Integer -> Integer
-- list of unit needs a special case to avoid 0/0
geometricSum len 1 = len + 1
geometricSum len base = (1 - base ^ (len + 1)) `quot` (1 - base)

-- | Integer logarithm (for base b and n, find largest i such that b^i
-- <= n)
ilog :: Integer -> Integer -> Integer
ilog n = toInteger . integerLogBase' n

boundedSeqCore :: Integer -> Encoding ty -> ([ty] -> Integer, Integer -> [ty])
boundedSeqCore len Encoding { encEncode = encodefunc, encDecode = decodefunc,
                              encSize = sizeval } =
  case sizeval of
    Nothing ->
      let
        newencode [] = 0
        newencode vals =
          let
            thislen = toInteger (length vals)
            contentnum = fromProdList (map encodefunc vals)
          in
            (contentnum * len) + thislen

        newdecode 0 = []
        newdecode num =
          let
            adjusted = num - 1
            (remainingEntropy, lengthEntropy) = adjusted `quotRem` len
            thislen = lengthEntropy + 1
          in
            map decodefunc (toProdList thislen remainingEntropy)
      in
        (newencode, newdecode)
    Just 0 -> (\[] -> 0, \0 -> [])
    Just 1 -> (genericLength, flip genericReplicate (decodefunc 0))
    Just finitesize ->
      let
        newencode [] = 0
        newencode vals =
          let
            thislen = toInteger (length vals)
            base = geometricSum (thislen - 1) finitesize

            newencode' accum [] = accum
            newencode' accum (first : rest) =
              newencode' ((accum * finitesize) + encodefunc first) rest
          in
            base + (newencode' 0 (reverse vals))

        newdecode 0 = []
        newdecode num =
          let
            lowlen = ilog finitesize ((num * (finitesize - 1)) + 1) - 1
            thislen = lowlen + 1
            contentnum = num - (geometricSum lowlen finitesize)

            newdecode' accum 1 entropy = (decodefunc entropy : accum)
            newdecode' _ 0 _ = []
            newdecode' accum count entropy =
              let
                thisentropy = entropy `mod` finitesize
                restentropy = entropy `quot` finitesize
                this = decodefunc thisentropy
              in
               newdecode' (this : accum) (count - 1) restentropy
          in
            reverse (newdecode' [] thislen contentnum)
      in
        (newencode, newdecode)

-- | Construct an encoding for sequences whose length is bounded by a
-- given value from an encoding for elements of the sequence.
boundedSeq :: Integer
           -- ^ The maximum length of the sequence
           -> Encoding ty
           -- ^ The @Encoding@ for the sequence elements
           -> Encoding [ty]
boundedSeq len enc @ Encoding { encSize = sizeval, encInDomain = indomainfunc } =
  let
    (newencode, newdecode) = boundedSeqCore len enc
    newsize = case len of
      0 -> Just 1 --even if the list members are infinite
      _ -> fmap (geometricSum len) sizeval
    newindomain vals = length vals <= fromInteger len && all indomainfunc vals
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = newsize, encInDomain = newindomain }

-- | Take a function which takes a self-reference and produces a
-- recursive encoding, and produce the fixed-point encoding.
recursive :: (Encoding ty -> Encoding ty)
          -- ^ A function that, given a self-reference,
          -- constructs an encoding.
          -> Encoding ty
recursive genfunc =
  let
    enc = Encoding { encEncode = encode (genfunc enc),
                     encDecode = decode (genfunc enc),
                     encInDomain = inDomain (genfunc enc),
                     encSize = Nothing }
  in
    enc

-- | A recursive construction for two mutually-recursive constructions.
recursive2 :: ((Encoding ty1, Encoding ty2) -> Encoding ty1)
           -- ^ A function that, given self-references to both encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2) -> Encoding ty2)
           -- ^ A function that, given self-references to both encodings,
           -- constructs the second encoding.
           -> (Encoding ty1, Encoding ty2)
recursive2 genfunc1 genfunc2 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for three mutually-recursive constructions.
recursive3 :: ((Encoding ty1, Encoding ty2, Encoding ty3) -> Encoding ty1)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3) -> Encoding ty2)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the second encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3) -> Encoding ty3)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the third encoding.
           -> (Encoding ty1, Encoding ty2, Encoding ty3)
recursive3 genfunc1 genfunc2 genfunc3 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for four mutually-recursive constructions.
recursive4 :: ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4) ->
               Encoding ty1)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4) ->
               Encoding ty2)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the second encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4) ->
               Encoding ty3)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the third encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4) ->
               Encoding ty4)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fourth encoding.
           -> (Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4)
recursive4 genfunc1 genfunc2 genfunc3 genfunc4 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc4 encs),
                  encDecode = decode (genfunc4 encs),
                  encInDomain = inDomain (genfunc4 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for five mutually-recursive constructions.
recursive5 :: ((Encoding ty1, Encoding ty2, Encoding ty3,
                Encoding ty4, Encoding ty5) -> Encoding ty1)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3,
                Encoding ty4, Encoding ty5) -> Encoding ty2)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the second encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3,
                Encoding ty4, Encoding ty5) -> Encoding ty3)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the third encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3,
                Encoding ty4, Encoding ty5) -> Encoding ty4)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fourth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3,
                Encoding ty4, Encoding ty5) -> Encoding ty5)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fifth encoding.
           -> (Encoding ty1, Encoding ty2, Encoding ty3,
               Encoding ty4, Encoding ty5)
recursive5 genfunc1 genfunc2 genfunc3 genfunc4 genfunc5 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc4 encs),
                  encDecode = decode (genfunc4 encs),
                  encInDomain = inDomain (genfunc4 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc5 encs),
                  encDecode = decode (genfunc5 encs),
                  encInDomain = inDomain (genfunc5 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for six mutually-recursive constructions.
recursive6 :: ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6) -> Encoding ty1)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6) -> Encoding ty2)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the second encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6) -> Encoding ty3)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the third encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6) -> Encoding ty4)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fourth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6) -> Encoding ty5)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fifth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6) -> Encoding ty6)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the sixth encoding.
           -> (Encoding ty1, Encoding ty2, Encoding ty3,
               Encoding ty4, Encoding ty5, Encoding ty6)
recursive6 genfunc1 genfunc2 genfunc3 genfunc4 genfunc5 genfunc6 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc4 encs),
                  encDecode = decode (genfunc4 encs),
                  encInDomain = inDomain (genfunc4 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc5 encs),
                  encDecode = decode (genfunc5 encs),
                  encInDomain = inDomain (genfunc5 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc6 encs),
                  encDecode = decode (genfunc6 encs),
                  encInDomain = inDomain (genfunc6 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for seven mutually-recursive constructions.
recursive7 :: ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7) -> Encoding ty1)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7) -> Encoding ty2)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the second encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7) -> Encoding ty3)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the third encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7) -> Encoding ty4)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fourth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7) -> Encoding ty5)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fifth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7) -> Encoding ty6)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the sixth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7) -> Encoding ty7)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the seventh encoding.
           -> (Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
               Encoding ty5, Encoding ty6, Encoding ty7)
recursive7 genfunc1 genfunc2 genfunc3 genfunc4 genfunc5 genfunc6 genfunc7 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc4 encs),
                  encDecode = decode (genfunc4 encs),
                  encInDomain = inDomain (genfunc4 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc5 encs),
                  encDecode = decode (genfunc5 encs),
                  encInDomain = inDomain (genfunc5 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc6 encs),
                  encDecode = decode (genfunc6 encs),
                  encInDomain = inDomain (genfunc6 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc7 encs),
                  encDecode = decode (genfunc7 encs),
                  encInDomain = inDomain (genfunc7 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for eight mutually-recursive constructions.
recursive8 :: ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty1)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty2)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the second encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty3)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the third encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty4)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fourth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty5)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fifth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty6)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the sixth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty7)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the seventh encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8) ->
               Encoding ty8)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the eighth encoding.
           -> (Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
               Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8)
recursive8 genfunc1 genfunc2 genfunc3 genfunc4 genfunc5 genfunc6 genfunc7 genfunc8 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc4 encs),
                  encDecode = decode (genfunc4 encs),
                  encInDomain = inDomain (genfunc4 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc5 encs),
                  encDecode = decode (genfunc5 encs),
                  encInDomain = inDomain (genfunc5 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc6 encs),
                  encDecode = decode (genfunc6 encs),
                  encInDomain = inDomain (genfunc6 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc7 encs),
                  encDecode = decode (genfunc7 encs),
                  encInDomain = inDomain (genfunc7 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc8 encs),
                  encDecode = decode (genfunc8 encs),
                  encInDomain = inDomain (genfunc8 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for nine mutually-recursive constructions.
recursive9 :: ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty1)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the first encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty2)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the second encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty3)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the third encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty4)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fourth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty5)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the fifth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty6)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the sixth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty7)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the seventh encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty8)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the eighth encoding.
           -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7,
                Encoding ty8, Encoding ty9) -> Encoding ty9)
           -- ^ A function that, given self-references to all encodings,
           -- constructs the ninth encoding.
           -> (Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4, Encoding ty5,
               Encoding ty6, Encoding ty7, Encoding ty8, Encoding ty9)
recursive9 genfunc1 genfunc2 genfunc3 genfunc4 genfunc5
           genfunc6 genfunc7 genfunc8 genfunc9 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc4 encs),
                  encDecode = decode (genfunc4 encs),
                  encInDomain = inDomain (genfunc4 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc5 encs),
                  encDecode = decode (genfunc5 encs),
                  encInDomain = inDomain (genfunc5 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc6 encs),
                  encDecode = decode (genfunc6 encs),
                  encInDomain = inDomain (genfunc6 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc7 encs),
                  encDecode = decode (genfunc7 encs),
                  encInDomain = inDomain (genfunc7 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc8 encs),
                  encDecode = decode (genfunc8 encs),
                  encInDomain = inDomain (genfunc8 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc9 encs),
                  encDecode = decode (genfunc9 encs),
                  encInDomain = inDomain (genfunc9 encs),
                  encSize = Nothing })
  in
    encs

-- | A recursive construction for ten mutually-recursive constructions.
recursive10 :: ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty1)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the first encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty2)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the second encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty3)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the third encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty4)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the fourth encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty5)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the fifth encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty6)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the sixth encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty7)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the seventh encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty8)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the eighth encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty9)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the ninth encoding.
            -> ((Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                 Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                 Encoding ty9, Encoding ty10) -> Encoding ty10)
            -- ^ A function that, given self-references to all encodings,
            -- constructs the tenth encoding.
            -> (Encoding ty1, Encoding ty2, Encoding ty3, Encoding ty4,
                Encoding ty5, Encoding ty6, Encoding ty7, Encoding ty8,
                Encoding ty9, Encoding ty10)
recursive10 genfunc1 genfunc2 genfunc3 genfunc4 genfunc5
            genfunc6 genfunc7 genfunc8 genfunc9 genfunc10 =
  let
    encs =
      (Encoding { encEncode = encode (genfunc1 encs),
                  encDecode = decode (genfunc1 encs),
                  encInDomain = inDomain (genfunc1 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc2 encs),
                  encDecode = decode (genfunc2 encs),
                  encInDomain = inDomain (genfunc2 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc3 encs),
                  encDecode = decode (genfunc3 encs),
                  encInDomain = inDomain (genfunc3 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc4 encs),
                  encDecode = decode (genfunc4 encs),
                  encInDomain = inDomain (genfunc4 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc5 encs),
                  encDecode = decode (genfunc5 encs),
                  encInDomain = inDomain (genfunc5 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc6 encs),
                  encDecode = decode (genfunc6 encs),
                  encInDomain = inDomain (genfunc6 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc7 encs),
                  encDecode = decode (genfunc7 encs),
                  encInDomain = inDomain (genfunc7 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc8 encs),
                  encDecode = decode (genfunc8 encs),
                  encInDomain = inDomain (genfunc8 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc9 encs),
                  encDecode = decode (genfunc9 encs),
                  encInDomain = inDomain (genfunc9 encs),
                  encSize = Nothing },
       Encoding { encEncode = encode (genfunc10 encs),
                  encDecode = decode (genfunc10 encs),
                  encInDomain = inDomain (genfunc10 encs),
                  encSize = Nothing })
  in
    encs
