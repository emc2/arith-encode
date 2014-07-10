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

-- | Defines a datatype representing an isomorphism between the
-- datatype ty (or some subset thereof) and the natural numbers
-- (represented as 'Integer's).
--
-- 'Encoding's consist of several components, the most important of
-- which are the 'encode' and 'decode' functions.  These functions are
-- define an isomorphism to a range of natural number starting at
-- zero, meaning they are expected to obey the following laws:
-- * @encode . decode == id@
-- * @encode a == encode b@ only if @a == b@
-- * @(encEncode e) a >= 0@
-- * If @encSize e == Just n@, then @(encEncode e) a < n@
--
-- The 'size' value gives the number of mappings in the 'Encoding'.  For
-- an infinite isomorphism, @size == Nothing@, otherwise, @size ==
-- Just n@ for an 'Encoding' of size @n@.
--
-- The dimensions of the datatype are described by the type @dim@.
-- Dimensions in this context are used to indicate a notion of
-- /depth/, which is useful in some applications for 'Encoding's.  The
-- dimension type is expected to have an 'Enum' instance. It is up to
-- the implementation to determine what constitutes a dimension;
-- however, dimensions should determine a notion of depth independent
-- of all other dimensions.  For example, a matrix has two dimensions,
-- whereas a binary tree has one.  Depending on our intended
-- application, we might define an @n@-ary tree to have two
-- dimensions: one indicating the depth of the tree, and one
-- indicating the largest node degree.
--
-- The 'maxDepth' function should give the maximum depth for a given
-- dimension, or 'Nothing' if there is no maximum.
--
-- The 'depth' function indicates the depth in a given dimension for a
-- given 'ty'.
--
-- The 'highestIndex' function gives the highest number to which any
-- 'ty' up to and including a given depth in a given dimension will be
-- mapped, or 'Nothing' if there is no such limit.
--
-- It is always valid to define a dimensionless mapping, where @dim ==
-- Unit@ and every @ty@ has depth 0.
module Data.ArithEncode(
       -- * Basic Definitions

       -- ** Constructors
       Encoding,
       mkEncoding,
       mkDimlessEncoding,
       mkInfEncoding,
       mkInfDimlessEncoding,

       -- ** Using Encodings
       IllegalArgument(..),
       encode,
       decode,
       size,
       inDomain,
       maxDepth,
       depth,
       highestIndex,

       -- * Building Encodings

       -- ** Basic Encodings
       identity,
       void,
       singleton,
       integral,
       interval,
       fromHashableList,
       fromOrdList,

       -- ** Constructions

       -- *** Wrapping Values and Dimensions
       wrap,
       wrapDim,

       -- *** Optional
       optional,
       mandatory,

       -- *** Exclusion
       nonzero,
       exclude,

       -- *** Unions
       either,
       union,

       -- *** Products
       pair,
       pair',
       triple,
       triple',
       quad,
       quad',
       quint,
       quint',
       sextet,
       sextet',
       septet,
       septet',
       octet,
       octet',
       nonet,
       nonet',
       dectet,
       dectet',

       -- *** Sets
       SetDim(..),
       set,
       set',
       hashSet,
       hashSet',

       -- *** Sequences
       SeqDim(..),
       seq,
       seq',

       -- *** Recursive
       recursive,
{-
       recursive2,
       recursive3,
       recursive4,
       recursive5,
       recursive6,
       recursive7,
       recursive8,
       recursive9,
       recursive10
       -}
       ) where

import Control.Exception
import Control.Monad hiding (void)
import Data.Array.IArray(Array)
import Data.Bits
import Data.Hashable
import Data.List hiding (elem, union)
import Data.Maybe
import Data.Set(Set)
import Data.Typeable
import Prelude hiding (elem, either, seq)

import qualified Data.Array.IArray as Array
import qualified Data.Either as Either
import qualified Data.HashMap as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | An exception to be thrown if an illegal argument is given to
-- 'encode', 'decode', 'depth', or 'highestIndex'.
data IllegalArgument = IllegalArgument !String
  deriving Typeable

instance Show IllegalArgument where
  show (IllegalArgument "") = "Illegal argument"
  show (IllegalArgument s) = "Illegal argument: " ++ s

instance Exception IllegalArgument

-- | Type for an encoding.
data Encoding dim ty =
  Encoding {
    -- | Encode a @ty@ as a positive integer.
    encEncode :: ty -> Integer,
    -- | Decode a positive integer into a @ty@.
    encDecode :: Integer -> ty,
    -- | The size of an encoding, or 'Nothing' if it is infinite.
    encSize :: !(Maybe Integer),
    -- | Indicate whether or not a value is in the domain of the encoding.
    encInDomain :: ty -> Bool,
    -- | Get the maximum depth of a given dimension.
    encMaxDepth :: dim -> Maybe Integer,
    -- | Get the depth of a given @ty@ in a given dimension.
    encDepth :: dim -> ty -> Integer,
    -- | Get the higest number to which any 'ty' of a given depth in a
    -- given dimension will map.
    encHighestIndex :: dim -> Integer -> Maybe Integer
  }

-- | Create an encoding from all the necessary components.
mkEncoding :: Enum dim
           => (ty -> Integer)
           -- ^ The encoding function.
           -> (Integer -> ty)
           -- ^ The decoding function.  Can assume all inputs are positive.
           -> Maybe Integer
           -- ^ The number of mappings, or 'Nothing' if it is infinite.
           -> (ty -> Bool)
           -- ^ A function indicating whether or not a given value is
           -- in the domain of values.
           -> (dim -> Maybe Integer)
           -- ^ A function indicating the maximum depth of any dimension.
           -> (dim -> ty -> Integer)
           -- ^ A function indicating the depth of a @ty@ in dimension @dim@.
           -> (dim -> Integer -> Maybe Integer)
           -- ^ A function indicating the last number mapping to a
           -- @ty@ less than a given depth in a given dimension.
           -> Encoding dim ty
mkEncoding encodefunc decodefunc sizeval indomain
           maxdepthfunc depthfunc highindexfunc =
  Encoding { encEncode = encodefunc, encDecode = decodefunc,
             encSize = sizeval, encInDomain = indomain,
             encMaxDepth = maxdepthfunc, encDepth = depthfunc,
             encHighestIndex = highindexfunc }

-- | Create an infinite-sized encoding.
mkInfEncoding :: Enum dim
              => (ty -> Integer)
              -- ^ The encoding function.
              -> (Integer -> ty)
              -- ^ The decoding function.  Can assume all inputs are positive.
              -> (ty -> Bool)
              -- ^ A function indicating whether or not a given value is
              -- in the domain of values.
              -> (dim -> Maybe Integer)
              -- ^ A function indicating the maximum depth of any dimension.
              -> (dim -> ty -> Integer)
              -- ^ A function indicating the depth of a @ty@ in dimension @dim@.
              -> (dim -> Integer -> Maybe Integer)
              -- ^ A function indicating the last number mapping to a
              -- @ty@ less than a given depth in a given dimension.
              -> Encoding dim ty
mkInfEncoding encodefunc decodefunc indomain =
  mkEncoding encodefunc decodefunc Nothing indomain

-- | Create a dimensionless (potentially finite) encoding.
mkDimlessEncoding :: (ty -> Integer)
                  -- ^ The encoding function.
                  -> (Integer -> ty)
                  -- ^ The decoding function.  Can assume all inputs
                  -- are positive.
                  -> Maybe Integer
                  -- ^ The number of mappings, or 'Nothing' if it is infinite.
                  -> (ty -> Bool)
                  -- ^ A function indicating whether or not a given value is
                  -- in the domain of values.
                  -> Encoding () ty
mkDimlessEncoding encodefunc decodefunc sizeval indomain =
  mkEncoding encodefunc decodefunc sizeval indomain
             (const (Just 0)) (const (const 0)) (const (const sizeval))

-- | Create an infinite, dimensionless encoding.
mkInfDimlessEncoding :: (ty -> Integer)
                     -- ^ The encoding function.
                     -> (Integer -> ty)
                     -- ^ The decoding function.  Can assume all
                     -- inputs are positive.
                     -> (ty -> Bool)
                     -- ^ A function indicating whether or not a given value is
                     -- in the domain of values.
                     -> Encoding () ty
mkInfDimlessEncoding encodefunc decodefunc indomain =
  mkDimlessEncoding encodefunc decodefunc Nothing indomain

-- | Encode a @ty@ as a positive 'Integer'.
encode :: Encoding dim ty
       -- ^ Encoding to use.
       -> ty
       -- ^ Value to encode.
       -> Integer
       -- ^ Encoded value.
encode encoding = encEncode encoding

-- | Decode a @ty@ from a positive 'Integer'.
decode :: Encoding dim ty
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
size :: Encoding dim ty
     -- ^ Encoding to use.
     -> Maybe Integer
     -- ^ Number of values mapped, or 'Nothing' for infinity.
size = encSize

-- | Indicate whether or not a value is in the domain of the encoding.
inDomain :: Encoding dim ty
         -- ^ Encoding to use.
         -> ty
         -- ^ Value to query.
         -> Bool
         -- ^ Whether or not the value is in the domain of the encoding.
inDomain encoding = encInDomain encoding

-- | Get the maximum depth of a given dimension.
maxDepth :: Encoding dim ty
         -- ^ Encoding to use.
         -> dim
         -- ^ Dimension.
         -> Maybe Integer
         -- ^ Maximum depth of @dim@.
maxDepth encoding = encMaxDepth encoding

-- | Get the depth of a value.
depth :: Encoding dim ty
      -- ^ Encoding to use.
      -> dim
      -- ^ Dimension.
      -> ty
      -- ^ Dimension.
      -> Integer
      -- ^ Maximum depth of @dim@.
depth encoding = encDepth encoding

-- | Get the higest @n@ to which any value of depth no greater than
-- the given bound will be mapped using the given encoding.
highestIndex :: Encoding dim ty
             -- ^ Encoding to use.
             -> dim
             -- ^ Dimension.
             -> Integer
             -- ^ Bound on depth.
             -> Maybe Integer
             -- ^ Maximum depth of @dim@.
highestIndex encoding = encHighestIndex encoding

-- | The identity encoding.
identity :: Encoding () Integer
identity = mkInfDimlessEncoding id id (const True)

-- | A singleton encoding.  Maps a singular value to 0.
singleton :: Eq ty => ty -> Encoding () ty
singleton val = mkDimlessEncoding (const 0) (const val) (Just 1) (val ==)

-- | An empty encoding, which contains no mappings.
void :: Encoding a b
void = Encoding { encEncode = (\_ -> throw (IllegalArgument "void encoding")),
                  encDecode = (\_ -> throw (IllegalArgument "void encoding")),
                  encSize = Just 0, encInDomain = const False,
                  encDepth = (\_ _ -> throw (IllegalArgument "void encoding")),
                  encMaxDepth = (\_ -> throw (IllegalArgument "void encoding")),
                  encHighestIndex = (\_ _ -> throw (IllegalArgument "void encoding"))
                }

-- | An encoding of /all/ integers into the positive integers.
integral :: Integral n => Encoding () n
integral =
  let
    encodefunc num
      | num < 0 = ((abs (toInteger num) - 1) `shiftL` 1) `setBit` 0
      | otherwise = (toInteger num) `shiftL` 1

    decodefunc num
      | num `testBit` 0 = fromInteger (-((num `shiftR` 1) + 1))
      | otherwise = fromInteger (num `shiftR` 1)
  in
    mkInfDimlessEncoding encodefunc decodefunc (const True)

-- | Build an encoding from a finite range of 'Integral's.
interval :: Integral n
         => n
         -- ^ The (inclusive) lower bound on the range.
         -> n
         -- ^ The (inclusive) upper bound on the range.
         -> Encoding () n
interval lower upper
  | lower <= upper =
    let
      biglower = toInteger lower
      encodefunc num = (toInteger num) - biglower
      decodefunc num = fromInteger (num + biglower)
      sizeval = Just ((toInteger upper) - (toInteger lower) + 1)
      indomainfunc val = lower <= val && val <= upper
    in
       mkDimlessEncoding encodefunc decodefunc sizeval indomainfunc
  | otherwise = error "Lower bound is not less than upper bound"

-- | Build an encoding from a list of 'Hashable' items.
fromHashableList :: forall ty. (Hashable ty, Ord ty)
                 => [ty]
                 -- ^ A list of items to encode.
                 -> Encoding () ty
                 -- ^ An encoding mapping the items in the list to
                 -- natural numbers.
fromHashableList elems =
  let
    len = length elems

    revmap :: Array Int ty
    revmap = Array.listArray (0, len) elems

    fwdmap = HashMap.fromList (zip elems [0..len])
    encodefunc = toInteger . (HashMap.!) fwdmap
    decodefunc = (Array.!) revmap . fromInteger
    sizeval = Just (toInteger len)
    indomainfunc = (flip HashMap.member) fwdmap
  in
    mkDimlessEncoding encodefunc decodefunc sizeval indomainfunc

-- | Build an encoding from a list of 'Hashable' items.
fromOrdList :: forall ty . Ord ty
            => [ty]
            -- ^ A list of items to encode.
            -> Encoding () ty
            -- ^ An encoding mapping the items in the list to natural
            -- numbers.
fromOrdList elems =
  let
    len = length elems

    revmap :: Array Int ty
    revmap = Array.listArray (0, len) elems

    fwdmap = Map.fromList (zip elems [0..len])
    encodefunc = toInteger . (Map.!) fwdmap
    decodefunc = (Array.!) revmap . fromInteger
    sizeval = Just (toInteger len)
    indomainfunc = (flip Map.member) fwdmap
  in
    mkDimlessEncoding encodefunc decodefunc sizeval indomainfunc

-- | Wrap an encoding using a pair of functions.  These functions must
-- also define an isomorphism.
--
-- The encoding resulting from @wrapEncoding fwd rev enc@ implements
-- @depth@ as @depth enc . fwd@, which only works if @fwd@ preserves
-- all depths.  For more complex cases, use @mkEncoding@ to define a
-- new encoding.
wrap :: (a -> Maybe b)
     -- ^ The forward encoding function.
     -> (b -> a)
     -- ^ The reverse encoding function.
     -> Encoding dim b
     -- ^ The inner encoding.
     -> Encoding dim a
wrap fwd rev enc @ Encoding { encEncode = encodefunc, encDecode = decodefunc,
                              encInDomain = indomainfunc, encDepth = depthfunc } =
  let
    safefwd val =
      case fwd val of
        Just val' -> val'
        Nothing -> throw (IllegalArgument "No mapping into underlying domain")
  in
    enc { encEncode = encodefunc . safefwd,
          encDecode = rev . decodefunc,
          encInDomain = maybe False indomainfunc . fwd,
          encDepth = (\dim -> depthfunc dim . safefwd) }

-- | Wrap an encoding using a function that translates the dimensions
-- from one datatype to another.
wrapDim :: (a -> b) -> Encoding b ty -> Encoding a ty
wrapDim mapdim enc @ Encoding { encDepth = depthfunc, encMaxDepth = maxdepthfunc,
                                encHighestIndex = highindexfunc } =
  enc { encDepth = depthfunc . mapdim, encMaxDepth = maxdepthfunc . mapdim,
        encHighestIndex = highindexfunc . mapdim }

-- | Generate an encoding for @Maybe ty@ from an inner encoding for
-- @ty@.  This adds one level of depth: @Nothing@ has depth @0@, and
-- the rest of the depths are determined by adding one to the depths
-- from the inner encoding.
optional :: Encoding dim ty -> Encoding dim (Maybe ty)
optional Encoding { encEncode = encodefunc, encDecode = decodefunc,
                    encSize = sizeval, encInDomain = indomainfunc,
                    encMaxDepth = maxdepthfunc, encDepth = depthfunc,
                    encHighestIndex = highestindexfunc } =
  let
    newsize = sizeval >>= return . (+ 1)
    newmaxdepth dim = maxdepthfunc dim >>= return . (+ 1)
    newindomain = maybe True indomainfunc

    newencode Nothing = 0
    newencode (Just val) = 1 + encodefunc val

    newdecode 0 = Nothing
    newdecode num = Just (decodefunc (num - 1))

    newdepth _ Nothing = 0
    newdepth dim (Just val) = (depthfunc dim val) + 1

    newhighestindex _ 0 = Just 0
    newhighestindex dim val = highestindexfunc dim (val - 1)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = newsize, encInDomain = newindomain,
               encMaxDepth = newmaxdepth, encDepth = newdepth,
               encHighestIndex = newhighestindex }

-- | The dual of @optional@.  This construction assumes that @Nothing@
-- maps to @0@, and removes it from the input domain.  It also assumes
-- that @Nothing@ has depth @0@, and everything else has a higher
-- depth.
--
-- Using this construction on encodings for @Maybe ty@ which are not
-- produced by @optional@ may have unexpected results.
mandatory :: Encoding dim (Maybe ty) -> Encoding dim ty
mandatory Encoding { encEncode = encodefunc, encDecode = decodefunc,
                     encSize = sizeval, encInDomain = indomainfunc,
                     encMaxDepth = maxdepthfunc, encDepth = depthfunc,
                     encHighestIndex = highestindexfunc } =
  let
    dec n = n - 1
    newencode = dec . encodefunc . Just
    newdecode = fromJust . decodefunc . (+ 1)
    newsize = sizeval >>= return . dec
    newmaxdepth dim = maxdepthfunc dim >>= return . dec
    newdepth dim = dec . depthfunc dim . Just
    newhighestindex dim = highestindexfunc dim . dec
    newindomain = indomainfunc . Just
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = newsize, encInDomain = newindomain,
               encMaxDepth = newmaxdepth, encDepth = newdepth,
               encHighestIndex = newhighestindex }

-- | Removes the mapping to @0@ (ie. the first mapping).  This has the
-- same effect as @exclude [x]@, where @x@ is the value that maps to
-- @0@.  It is also similar to @mandatory@, except that it does not
-- change the base type.
nonzero :: Encoding dim ty -> Encoding dim ty
nonzero enc @ Encoding { encEncode = encodefunc, encDecode = decodefunc,
                         encSize = sizeval, encInDomain = indomainfunc,
                         encHighestIndex = highindexfunc } =
  let
    dec n = n - 1
    newencode = dec . encodefunc
    newdecode = decodefunc . (+ 1)
    newsize = sizeval >>= return . dec
    newhighestindex dim num = highindexfunc dim num >>= return . dec
    newindomain val = indomainfunc val && 0 /= encodefunc val
  in
    enc { encEncode = newencode, encDecode = newdecode,
          encSize = newsize, encInDomain = newindomain,
          encHighestIndex = newhighestindex }

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
exclude :: [ty] -> Encoding dim ty -> Encoding dim ty
exclude [] enc = enc
exclude excludes enc @ Encoding { encEncode = encodefunc, encDecode = decodefunc,
                                  encSize = sizeval, encInDomain = indomainfunc,
                                  encHighestIndex = highindexfunc } =
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

    newHighestIndex dim depthval =
      do
        maxdepth <- highindexfunc dim depthval
        case closestWithin maxdepth fwdtree of
          Just (_, offset)
            | offset <= maxdepth -> return (maxdepth - offset)
            | otherwise -> return 0
          Nothing -> return maxdepth

    newSize =
      do
        n <- sizeval
        return $! (n - (toInteger (length excludes)))

    newInDomain val =
      indomainfunc val && not (HashSet.member (encodefunc val) forbidden)
  in
    enc { encEncode = newEncode, encDecode = newDecode,
          encSize = newSize, encInDomain = newInDomain,
          encHighestIndex = newHighestIndex }

-- | Combine two encodings into a single encoding that returns an
-- @Either@ of the two types.
either :: Encoding dim1 ty1
       -- ^ The @Encoding@ that will be represented by @Left@.
       -> Encoding dim2 ty2
       -- ^ The @Encoding@ that will be represented by @Right@.
       -> Encoding (Either dim1 dim2) (Either ty1 ty2)
either Encoding { encEncode = encode1, encDecode = decode1,
                  encInDomain = indomain1, encSize = sizeval1,
                  encDepth = depth1, encHighestIndex = highindex1,
                  encMaxDepth = maxDepth1 }
       Encoding { encEncode = encode2, encDecode = decode2,
                  encInDomain = indomain2, encSize = sizeval2,
                  encDepth = depth2, encHighestIndex = highindex2,
                  encMaxDepth = maxDepth2 } =
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
    newMaxDepth = Either.either maxDepth1 maxDepth2

    newHighestIndex (Left dim) =
      maybe Nothing (Just . leftIdxFwd . (\n -> n - 1)) . highindex1 dim
    newHighestIndex (Right dim) =
      maybe Nothing (Just . rightIdxFwd . (\n -> n - 1)) . highindex2 dim

    newDepth (Left dim) (Left val) = depth1 dim val
    newDepth (Right dim) (Right val) = depth2 dim val
    newDepth _ _ = 0

    newInDomain = Either.either indomain1 indomain2
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain,
               encMaxDepth = newMaxDepth, encDepth = newDepth,
               encHighestIndex = newHighestIndex }

sortfunc :: Maybe Integer -> Maybe Integer -> Ordering
sortfunc Nothing Nothing = EQ
sortfunc Nothing _ = GT
sortfunc _ Nothing = LT
sortfunc (Just a) (Just b) = compare a b

-- | Combine a set of encodings with the same dimension and result
-- type into a single encoding which represents the disjoint union of
-- the components.
union :: forall dim ty. [Encoding dim ty] -> Encoding dim ty
union [] = error "union encoding with no arguments"
union encodings =
  let
    numelems :: Int
    numelems = length encodings

    sortpair (a, _) (b, _) = sortfunc a b

    (sizes, sortedencodings) =
      unzip (sortBy sortpair (map (\enc -> (size enc, enc)) encodings))
    -- Turn the sorted element encodings into an array for fast access
    encodingarr :: Array.Array Int (Encoding dim ty)
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

    depthfunc dim val =
      case find ((flip inDomain) val) sortedencodings of
        Just enc -> depth enc dim val
        Nothing -> throw (IllegalArgument "Value not in domain of any component")

    indomainfunc val = any ((flip inDomain) val) sortedencodings
    maxdepthfunc dim = maximum (map ((flip maxDepth) dim) sortedencodings)
    highindexfunc dim depthval =
      let
        mapfun (encidx, enc) =
          do
            highidx <- highestIndex enc dim depthval
            return (fwdmapnum highidx encidx)
      in
        maximumBy sortfunc (map mapfun (Array.assocs encodingarr))
  in
    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

mkPairCore :: Encoding dim1 ty1 -> Encoding dim2 ty2 ->
                      ((ty1, ty2) -> Integer, Integer -> (ty1, ty2), Maybe Integer)
mkPairCore Encoding { encEncode = encode1, encDecode = decode1,
                      encSize = sizeval1 }
           Encoding { encEncode = encode2, encDecode = decode2,
                      encSize = sizeval2 } =
  let
    (encodefunc, decodefunc) = case (sizeval1, sizeval2) of
      (Just maxval, _) ->
        let
          newencode (val1, val2) = ((encode2 val2) * maxval) + (encode1 val1)
          newdecode num = (decode1 (num `mod` maxval), decode2 (num `quot` maxval))
        in
          (newencode, newdecode)
      (_, Just maxval) ->
        let
          newencode (val1, val2) = ((encode1 val1) * maxval) + (encode2 val2)
          newdecode num = (decode1 (num `quot` maxval), decode2 (num `mod` maxval))
        in
          (newencode, newdecode)
      (Nothing, Nothing) ->
        let
          newencode (val1, val2) =
            let
              encoded1 = encode1 val1
              encoded2 = encode2 val2
              sumval = encoded1 + encoded2
              base = (((sumval + 1) * sumval)) `quot` 2
            in
              base + encoded2

          newdecode num =
            let
              sumval = (isqrt ((8 * num) + 1) - 1) `quot` 2
              base = (((sumval + 1) * sumval)) `quot` 2
              num2 = num - base
              num1 = sumval - num2
            in
              (decode1 num1, decode2 num2)
        in
          (newencode, newdecode)

    sizeval =
      do
        size1 <- sizeval1
        size2 <- sizeval2
        return (size1 * size2)
  in
    (encodefunc, decodefunc, sizeval)

-- | Take encodings for two datatypes A and B, and build an encoding
-- for a pair (A, B).
pair :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding (dim1, dim2) (ty1, ty2)
pair enc1 @ Encoding { encInDomain = indomain1, encMaxDepth = maxDepth1,
                       encDepth = depth1, encHighestIndex = highindex1 }
     enc2 @ Encoding { encInDomain = indomain2, encMaxDepth = maxDepth2,
                       encDepth = depth2, encHighestIndex = highindex2 } =
  let
    (encodefunc, decodefunc, sizeval) = mkPairCore enc1 enc2

    indomainfunc (val1, val2) = indomain1 val1 && indomain2 val2
    depthfunc (dim1, dim2) (val1, val2) = max (depth1 dim1 val1) (depth2 dim2 val2)

    maxdepthfunc (dim1, dim2) =
      do
        maxdepthval1 <- maxDepth1 dim1
        maxdepthval2 <- maxDepth2 dim2
        return (max maxdepthval1 maxdepthval2)

    highindexfunc (dim1, dim2) num =
      do
        idx1 <- highindex1 dim1 num
        idx2 <- highindex2 dim2 num
        return (max idx1 idx2)
  in
    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Take encodings for two datatypes A and B, and build an encoding
-- for a pair (A, B).
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
pair' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim (ty1, ty2)
pair' enc1 @ Encoding { encInDomain = indomain1, encMaxDepth = maxDepth1,
                        encDepth = depth1, encHighestIndex = highindex1 }
      enc2 @ Encoding { encInDomain = indomain2, encMaxDepth = maxDepth2,
                        encDepth = depth2, encHighestIndex = highindex2 } =
  let
    (encodefunc, decodefunc, sizeval) = mkPairCore enc1 enc2

    indomainfunc (val1, val2) = indomain1 val1 && indomain2 val2
    depthfunc dim (val1, val2) = max (depth1 dim val1) (depth2 dim val2)

    maxdepthfunc dim =
      do
        maxdepthval1 <- maxDepth1 dim
        maxdepthval2 <- maxDepth2 dim
        return (max maxdepthval1 maxdepthval2)

    highindexfunc dim num =
      do
        idx1 <- highindex1 dim num
        idx2 <- highindex2 dim num
        return (max idx1 idx2)
  in
    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for a 3-tuple from the encodings for the
-- three components.  This is actually just a wrapper around @pair@.
triple :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding dim3 ty3 ->
          Encoding (dim1, dim2, dim3) (ty1, ty2, ty3)
triple enc1 enc2 enc3 =
  let
    fwdshuffle (val1, val2, val3) = ((val1, val2), val3)
    revshuffle ((val1, val2), val3) = (val1, val2, val3)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair enc1 enc2) enc3

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for a 3-tuple from the encodings for the
-- three components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
triple' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim ty3 ->
           Encoding dim (ty1, ty2, ty3)
triple' enc1 enc2 enc3 =
  let
    fwdshuffle (val1, val2, val3) = ((val1, val2), val3)
    revshuffle ((val1, val2), val3) = (val1, val2, val3)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' enc1 enc2) enc3

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for a 4-tuple from the encodings for the
-- four components.  This is actually just a wrapper around @pair@.
quad :: Encoding dim1 ty1 -> Encoding dim2 ty2 ->
        Encoding dim3 ty3 -> Encoding dim4 ty4 ->
        Encoding (dim1, dim2, dim3, dim4) (ty1, ty2, ty3, ty4)
quad enc1 enc2 enc3 enc4 =
  let
    fwdshuffle (val1, val2, val3, val4) = ((val1, val2), (val3, val4))
    revshuffle ((val1, val2), (val3, val4)) = (val1, val2, val3, val4)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair enc1 enc2) (pair enc3 enc4)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for a 4-tuple from the encodings for the
-- four components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
quad' :: Encoding dim ty1 -> Encoding dim ty2 ->
         Encoding dim ty3 -> Encoding dim ty4 ->
         Encoding dim (ty1, ty2, ty3, ty4)
quad' enc1 enc2 enc3 enc4 =
  let
    fwdshuffle (val1, val2, val3, val4) = ((val1, val2), (val3, val4))
    revshuffle ((val1, val2), (val3, val4)) = (val1, val2, val3, val4)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' enc1 enc2) (pair' enc3 enc4)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for a 5-tuple from the encodings for the
-- five components.  This is actually just a wrapper around @pair@.
quint :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding dim3 ty3 ->
         Encoding dim4 ty4 -> Encoding dim5 ty5 ->
         Encoding (dim1, dim2, dim3, dim4, dim5) (ty1, ty2, ty3, ty4, ty5)
quint enc1 enc2 enc3 enc4 enc5 =
  let
    fwdshuffle (val1, val2, val3, val4, val5) = (((val1, val2), val3), (val4, val5))
    revshuffle (((val1, val2), val3), (val4, val5)) = (val1, val2, val3, val4, val5)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair (pair enc1 enc2) enc3) (pair enc4 enc5)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for a 5-tuple from the encodings for the
-- five components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
quint' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim ty3 ->
          Encoding dim ty4 -> Encoding dim ty5 ->
          Encoding dim (ty1, ty2, ty3, ty4, ty5)
quint' enc1 enc2 enc3 enc4 enc5 =
  let
    fwdshuffle (val1, val2, val3, val4, val5) = (((val1, val2), val3), (val4, val5))
    revshuffle (((val1, val2), val3), (val4, val5)) = (val1, val2, val3, val4, val5)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' (pair' enc1 enc2) enc3) (pair' enc4 enc5)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for a 6-tuple from the encodings for the
-- six components.  This is actually just a wrapper around @pair@.
sextet :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding dim3 ty3 ->
          Encoding dim4 ty4 -> Encoding dim5 ty5 -> Encoding dim6 ty6 ->
          Encoding (dim1, dim2, dim3, dim4, dim5, dim6)
                   (ty1, ty2, ty3, ty4, ty5, ty6)
sextet enc1 enc2 enc3 enc4 enc5 enc6 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6) =
      (((val1, val2), val3), ((val4, val5), val6))
    revshuffle (((val1, val2), val3), ((val4, val5), val6)) =
      (val1, val2, val3, val4, val5, val6)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair (pair enc1 enc2) enc3) (pair (pair enc4 enc5) enc6)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for a 6-tuple from the encodings for the
-- six components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
sextet' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim ty3 ->
           Encoding dim ty4 -> Encoding dim ty5 -> Encoding dim ty6 ->
           Encoding dim (ty1, ty2, ty3, ty4, ty5, ty6)
sextet' enc1 enc2 enc3 enc4 enc5 enc6 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6) =
      (((val1, val2), val3), ((val4, val5), val6))
    revshuffle (((val1, val2), val3), ((val4, val5), val6)) =
      (val1, val2, val3, val4, val5, val6)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' (pair' enc1 enc2) enc3) (pair' (pair' enc4 enc5) enc6)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for a 7-tuple from the encodings for the
-- seven components.  This is actually just a wrapper around @pair@.
septet :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding dim3 ty3 ->
          Encoding dim4 ty4 -> Encoding dim5 ty5 ->
          Encoding dim6 ty6 -> Encoding dim7 ty7 ->
          Encoding (dim1, dim2, dim3, dim4, dim5, dim6, dim7)
                   (ty1, ty2, ty3, ty4, ty5, ty6, ty7)
septet enc1 enc2 enc3 enc4 enc5 enc6 enc7 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7) =
      (((val1, val2), (val3, val4)), ((val5, val6), val7))
    revshuffle (((val1, val2), (val3, val4)), ((val5, val6), val7)) =
      (val1, val2, val3, val4, val5, val6, val7)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair (pair enc1 enc2) (pair enc3 enc4)) (pair (pair enc5 enc6) enc7)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for a 7-tuple from the encodings for the
-- seven components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
septet' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim ty3 ->
           Encoding dim ty4 -> Encoding dim ty5 ->
           Encoding dim ty6 -> Encoding dim ty7 ->
           Encoding dim (ty1, ty2, ty3, ty4, ty5, ty6, ty7)
septet' enc1 enc2 enc3 enc4 enc5 enc6 enc7 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7) =
      (((val1, val2), (val3, val4)), ((val5, val6), val7))
    revshuffle (((val1, val2), (val3, val4)), ((val5, val6), val7)) =
      (val1, val2, val3, val4, val5, val6, val7)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' (pair' enc1 enc2) (pair' enc3 enc4))
            (pair' (pair' enc5 enc6) enc7)

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for an 8-tuple from the encodings for the
-- eight components.  This is actually just a wrapper around @pair@.
octet :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding dim3 ty3 ->
         Encoding dim4 ty4 -> Encoding dim5 ty5 -> Encoding dim6 ty6 ->
         Encoding dim7 ty7 -> Encoding dim8 ty8 ->
         Encoding (dim1, dim2, dim3, dim4, dim5, dim6, dim7, dim8)
                  (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8)
octet enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8) =
      (((val1, val2), (val3, val4)), ((val5, val6), (val7, val8)))
    revshuffle (((val1, val2), (val3, val4)), ((val5, val6), (val7, val8))) =
      (val1, val2, val3, val4, val5, val6, val7, val8)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair (pair enc1 enc2) (pair enc3 enc4))
           (pair (pair enc5 enc6) (pair enc7 enc8))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for an 8-tuple from the encodings for the
-- eight components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
octet' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim ty3 ->
          Encoding dim ty4 -> Encoding dim ty5 -> Encoding dim ty6 ->
          Encoding dim ty7 -> Encoding dim ty8 ->
          Encoding dim (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8)
octet' enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8) =
      (((val1, val2), (val3, val4)), ((val5, val6), (val7, val8)))
    revshuffle (((val1, val2), (val3, val4)), ((val5, val6), (val7, val8))) =
      (val1, val2, val3, val4, val5, val6, val7, val8)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' (pair' enc1 enc2) (pair' enc3 enc4))
            (pair' (pair' enc5 enc6) (pair' enc7 enc8))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for a 9-tuple from the encodings for the
-- nine components.  This is actually just a wrapper around @pair@.
nonet :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding dim3 ty3 ->
         Encoding dim4 ty4 -> Encoding dim5 ty5 -> Encoding dim6 ty6 ->
         Encoding dim7 ty7 -> Encoding dim8 ty8 -> Encoding dim9 ty9 ->
         Encoding (dim1, dim2, dim3, dim4, dim5, dim6, dim7, dim8, dim9)
                  (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8, ty9)
nonet enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8, val9) =
      ((((val1, val2), val3), (val4, val5)), ((val6, val7), (val8, val9)))
    revshuffle ((((val1, val2), val3), (val4, val5)), ((val6, val7), (val8, val9))) =
      (val1, val2, val3, val4, val5, val6, val7, val8, val9)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair (pair (pair enc1 enc2) enc3) (pair enc4 enc5))
           (pair (pair enc6 enc7) (pair enc8 enc9))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for a 9-tuple from the encodings for the
-- nine components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
nonet' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim ty3 ->
          Encoding dim ty4 -> Encoding dim ty5 -> Encoding dim ty6 ->
          Encoding dim ty7 -> Encoding dim ty8 -> Encoding dim ty9 ->
          Encoding dim (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8, ty9)
nonet' enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8, val9) =
      ((((val1, val2), val3), (val4, val5)), ((val6, val7), (val8, val9)))
    revshuffle ((((val1, val2), val3), (val4, val5)), ((val6, val7), (val8, val9))) =
      (val1, val2, val3, val4, val5, val6, val7, val8, val9)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' (pair' (pair' enc1 enc2) enc3) (pair' enc4 enc5))
            (pair' (pair' enc6 enc7) (pair' enc8 enc9))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | Construct an encoding for a 10-tuple from the encodings for the
-- ten components.  This is actually just a wrapper around @pair@.
dectet :: Encoding dim1 ty1 -> Encoding dim2 ty2 -> Encoding dim3 ty3 ->
          Encoding dim4 ty4 -> Encoding dim5 ty5 -> Encoding dim6 ty6 ->
          Encoding dim7 ty7 -> Encoding dim8 ty8 ->
          Encoding dim9 ty9 -> Encoding dim10 ty10 ->
          Encoding (dim1, dim2, dim3, dim4, dim5, dim6, dim7, dim8, dim9, dim10)
                   (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8, ty9, ty10)
dectet enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10) =
      ((((val1, val2), val3), (val4, val5)), (((val6, val7), val8), (val9, val10)))
    revshuffle ((((val1, val2), val3), (val4, val5)),
                (((val6, val7), val8), (val9, val10))) =
      (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair (pair (pair (pair enc1 enc2) enc3) (pair enc4 enc5))
           (pair (pair (pair enc6 enc7) enc8) (pair enc9 enc10))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc (fwdshuffle dim) . fwdshuffle
    newmaxdepth = maxdepthfunc . fwdshuffle
    newhighindex dim = highindexfunc (fwdshuffle dim)
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = newmaxdepth,
               encHighestIndex = newhighindex }

-- | Construct an encoding for a 10-tuple from the encodings for the
-- ten components.  This is actually just a wrapper around @pair@.
--
-- This variant uses the same dimension type as
-- its two components, making it suitable as a recursive construction.
dectet' :: Encoding dim ty1 -> Encoding dim ty2 -> Encoding dim ty3 ->
           Encoding dim ty4 -> Encoding dim ty5 -> Encoding dim ty6 ->
           Encoding dim ty7 -> Encoding dim ty8 ->
           Encoding dim ty9 -> Encoding dim ty10 ->
           Encoding dim (ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8, ty9, ty10)
dectet' enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 =
  let
    fwdshuffle (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10) =
      ((((val1, val2), val3), (val4, val5)), (((val6, val7), val8), (val9, val10)))
    revshuffle ((((val1, val2), val3), (val4, val5)),
                (((val6, val7), val8), (val9, val10))) =
      (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10)

    Encoding { encEncode = encodefunc, encDecode = decodefunc,
               encSize = sizeval, encInDomain = indomainfunc,
               encDepth = depthfunc, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc } =
      pair' (pair' (pair' (pair' enc1 enc2) enc3) (pair' enc4 enc5))
            (pair' (pair' (pair' enc6 enc7) enc8) (pair' enc9 enc10))

    newencode = encodefunc . fwdshuffle
    newdecode = revshuffle . decodefunc
    newindomain = indomainfunc . fwdshuffle
    newdepth dim = depthfunc dim . fwdshuffle
  in
    Encoding { encEncode = newencode, encDecode = newdecode,
               encSize = sizeval, encInDomain = newindomain,
               encDepth = newdepth, encMaxDepth = maxdepthfunc,
               encHighestIndex = highindexfunc }

-- | A datatype representing the dimensions of a set.
data SetDim dim =
    -- | A dimension representing the size of the set.
    SetSize
    -- | A dimension representing the dimensions of the elements.  The
    -- depth of a set in a given element dimension is the maximum of
    -- the depths of all its elements in that dimension.
  | SetElem dim

setCore :: Ord ty => Encoding dim ty -> (Set ty -> Integer, Integer -> Set ty,
                                         Maybe Integer, Set ty -> Bool)
setCore Encoding { encEncode = encodefunc, encDecode = decodefunc,
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
    (newEncode, newDecode, newSize, newInDomain)

-- | Build an encoding for /finite/ sets of values of a given datatype
-- from an encoding for that datatype.
set :: Ord ty => Encoding dim ty -> Encoding (SetDim dim) (Set ty)
set enc @ Encoding { encDepth = depthfunc, encHighestIndex = highindexfunc,
                     encSize = sizeval, encMaxDepth = maxdepthfunc } =
  let
    (newEncode, newDecode, newSize, newInDomain) = setCore enc

    newDepth SetSize = toInteger . Set.size
    newDepth (SetElem dim) =
      let
        foldfun m elem =
          let
            n = depthfunc dim elem
          in
            (max n m)
      in
        Set.foldl foldfun 0

    newMaxDepth SetSize = sizeval
    newMaxDepth (SetElem dim) = maxdepthfunc dim

    -- Pick the right implementation based on whether or not the
    -- underlying encoding is infinite
    newHighestIndex =
      case sizeval of
        Just setsize ->
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize n
              | n <= setsize =
                Just (((2 ^ n) - 1 :: Integer) `shiftL`
                        fromInteger (setsize - n))
              | otherwise = throw (IllegalArgument "Set size is too big")
            newHighestIndex' (SetElem dim) n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
        Nothing ->
          -- For the infinite case, there is no highest index for a
          -- set of a given size.
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize _ = Nothing
            newHighestIndex' (SetElem dim) n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain,
               encDepth = newDepth, encMaxDepth = newMaxDepth,
               encHighestIndex = newHighestIndex }

-- | Build an encoding for /finite/ sets of values of a given datatype
-- from an encoding for that datatype.
--
-- In this variant, the component type must be a @SetDim@, making it
-- suitable as a recursive construction.
set' :: Ord ty => Encoding (SetDim dim) ty -> Encoding (SetDim dim) (Set ty)
set' enc @ Encoding { encDepth = depthfunc, encHighestIndex = highindexfunc,
                      encSize = sizeval, encMaxDepth = maxdepthfunc } =
  let
    (newEncode, newDecode, newSize, newInDomain) = setCore enc

    newDepth SetSize = toInteger . Set.size
    newDepth dim =
      let
        foldfun m elem =
          let
            n = depthfunc dim elem
          in
            (max n m)
      in
        Set.foldl foldfun 0

    newMaxDepth SetSize = sizeval
    newMaxDepth dim = maxdepthfunc dim

    -- Pick the right implementation based on whether or not the
    -- underlying encoding is infinite
    newHighestIndex =
      case sizeval of
        Just setsize ->
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize n
              | n <= setsize =
                Just (((2 ^ n) - 1 :: Integer) `shiftL`
                        fromInteger (setsize - n))
              | otherwise = throw (IllegalArgument "Set size is too big")
            newHighestIndex' dim n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
        Nothing ->
          -- For the infinite case, there is no highest index for a
          -- set of a given size.
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize _ = Nothing
            newHighestIndex' dim n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain,
               encDepth = newDepth, encMaxDepth = newMaxDepth,
               encHighestIndex = newHighestIndex }

hashSetCore :: (Hashable ty, Ord ty) => Encoding dim ty ->
               (HashSet.Set ty -> Integer, Integer -> HashSet.Set ty,
                Maybe Integer, HashSet.Set ty -> Bool)
hashSetCore Encoding { encEncode = encodefunc, encDecode = decodefunc,
                       encSize = sizeval, encInDomain = indomainfunc } =
  let
    newEncode =
      HashSet.fold (\elem n -> setBit n (fromInteger (encodefunc elem))) 0

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
    (newEncode, newDecode, newSize, newInDomain)

-- | Build an encoding for /finite/ sets of values of a given datatype
-- from an encoding for that datatype.  Similar to @set@, but uses
-- @HashSet@ instead
hashSet :: (Hashable ty, Ord ty) =>
           Encoding dim ty -> Encoding (SetDim dim) (HashSet.Set ty)
hashSet enc @ Encoding { encSize = sizeval, encDepth = depthfunc,
                         encMaxDepth = maxdepthfunc,
                         encHighestIndex = highindexfunc } =
  let
    (newEncode, newDecode, newSize, newInDomain) = hashSetCore enc

    newDepth SetSize = toInteger . HashSet.size
    newDepth (SetElem dim) =
      let
        foldfun elem m =
          let
            n = depthfunc dim elem
          in
            (max n m)
      in
        HashSet.fold foldfun 0

    newMaxDepth SetSize = sizeval
    newMaxDepth (SetElem dim) = maxdepthfunc dim

    -- Pick the right implementation based on whether or not the
    -- underlying encoding is infinite
    newHighestIndex =
      case sizeval of
        Just setsize ->
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize n
              | n <= setsize =
                Just (((2 ^ n) - 1 :: Integer) `shiftL`
                        fromInteger (setsize - n))
              | otherwise = throw (IllegalArgument "Set size is too big")
            newHighestIndex' (SetElem dim) n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
        Nothing ->
          -- For the infinite case, there is no highest index for a
          -- set of a given size.
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize _ = Nothing
            newHighestIndex' (SetElem dim) n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain,
               encDepth = newDepth, encMaxDepth = newMaxDepth,
               encHighestIndex = newHighestIndex }

-- | Build an encoding for /finite/ sets of values of a given datatype
-- from an encoding for that datatype.  Similar to @set@, but uses
-- @HashSet@ instead
--
-- In this variant, the component type must be a @SetDim@, making it
-- suitable as a recursive construction.
hashSet' :: (Hashable ty, Ord ty) =>
            Encoding (SetDim dim) ty -> Encoding (SetDim dim) (HashSet.Set ty)
hashSet' enc @ Encoding { encSize = sizeval, encDepth = depthfunc,
                          encMaxDepth = maxdepthfunc,
                          encHighestIndex = highindexfunc } =
  let
    (newEncode, newDecode, newSize, newInDomain) = hashSetCore enc

    newDepth SetSize = toInteger . HashSet.size
    newDepth dim =
      let
        foldfun elem m =
          let
            n = depthfunc dim elem
          in
            (max n m)
      in
        HashSet.fold foldfun 0

    newMaxDepth SetSize = sizeval
    newMaxDepth dim = maxdepthfunc dim

    -- Pick the right implementation based on whether or not the
    -- underlying encoding is infinite
    newHighestIndex =
      case sizeval of
        Just setsize ->
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize n
              | n <= setsize =
                Just (((2 ^ n) - 1 :: Integer) `shiftL`
                        fromInteger (setsize - n))
              | otherwise = throw (IllegalArgument "Set size is too big")
            newHighestIndex' dim n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
        Nothing ->
          -- For the infinite case, there is no highest index for a
          -- set of a given size.
          let
            newHighestIndex' SetSize 0 = Just 0
            newHighestIndex' SetSize _ = Nothing
            newHighestIndex' dim n =
              do
                idx <- highindexfunc dim n
                return (2 ^ idx)
          in
            newHighestIndex'
  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = newSize, encInDomain = newInDomain,
               encDepth = newDepth, encMaxDepth = newMaxDepth,
               encHighestIndex = newHighestIndex }

-- | Dimension constructor for sequences.  Adds a dimension
-- representing length.
data SeqDim dim =
  -- | A Dimension representing the length of the sequence.
    SeqLen
  -- | A dimension representing the dimensions of the elements.  The
  -- depth of a set in a given element dimension is the maximum of
  -- the depths of all its elements in that dimension.
  | SeqElem dim

seqCore :: Encoding dim ty -> ([ty] -> Integer, Integer -> [ty])
seqCore Encoding { encEncode = encodefunc, encDecode = decodefunc,
                   encSize = sizeval } =
  case sizeval of
    -- For encodings with a maximum size s, a list with n elements
    -- e_i is encoded as e_n + s e_(n-1) + ... s^n e_1
    Just finitesize ->
      let
        withendsize = finitesize + 1

        newencodefunc =
          let
            foldfun accum = (((accum * withendsize) + 1) +) . encodefunc
          in
           foldl foldfun 0

        newdecodefunc =
          let
            newdecodefunc' accum 0 = accum
            newdecodefunc' accum num =
              let
                decoded = decodefunc ((num `mod` withendsize) - 1)
              in
               newdecodefunc' (decoded : accum) (num `quot` withendsize)
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

-- | Construct an encoding for sequences of a type from an encoding
-- for values of that type.
seq :: Encoding dim ty -> Encoding (SeqDim dim) [ty]
seq enc @ Encoding { encInDomain = indomainfunc, encSize = sizeval,
                     encDepth = depthfunc, encMaxDepth = maxdepthfunc } =
  let
    (newEncode, newDecode) = seqCore enc
    newInDomain = all indomainfunc

    newHighestIndex =
      case sizeval of
        Just finitesize ->
          let
            withendsize = finitesize + 1

            newhighindexfunc SeqLen n = Just (n * withendsize)
            newhighindexfunc (SeqElem _) _ = Nothing
          in
            newhighindexfunc
        Nothing ->
          let
            newhighindexfunc SeqLen 0 = Just 0
            newhighindexfunc _ _ = Nothing
          in
            newhighindexfunc

    newDepth SeqLen val = toInteger (length val)
    newDepth (SeqElem _) [] = 0
    newDepth (SeqElem dim) val = maximum (map (depthfunc dim) val)

    newMaxDepth SeqLen = Nothing
    newMaxDepth (SeqElem dim) = maxdepthfunc dim

  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = Nothing, encInDomain = newInDomain,
               encDepth = newDepth, encMaxDepth = newMaxDepth,
               encHighestIndex = newHighestIndex }

-- | Construct an encoding for sequences of a type from an encoding
-- for values of that type.
--
-- In this variant, the component type must be a @SeqDim@, making it
-- suitable as a recursive construction.
seq' :: Encoding (SeqDim dim) ty -> Encoding (SeqDim dim) [ty]
seq' enc @ Encoding { encInDomain = indomainfunc, encSize = sizeval,
                      encDepth = depthfunc, encMaxDepth = maxdepthfunc } =
  let
    (newEncode, newDecode) = seqCore enc
    newInDomain = all indomainfunc

    newHighestIndex =
      case sizeval of
        Just finitesize ->
          let
            withendsize = finitesize + 1

            newhighindexfunc SeqLen n = Just (n * withendsize)
            newhighindexfunc (SeqElem _) _ = Nothing
          in
            newhighindexfunc
        Nothing ->
          let
            newhighindexfunc SeqLen 0 = Just 0
            newhighindexfunc _ _ = Nothing
          in
            newhighindexfunc

    newDepth SeqLen val = toInteger (length val)
    newDepth (SeqElem _) [] = 0
    newDepth dim val = maximum (map (depthfunc dim) val)

    newMaxDepth SeqLen = Nothing
    newMaxDepth dim = maxdepthfunc dim

  in
    Encoding { encEncode = newEncode, encDecode = newDecode,
               encSize = Nothing, encInDomain = newInDomain,
               encDepth = newDepth, encMaxDepth = newMaxDepth,
               encHighestIndex = newHighestIndex }

-- | Take a function which takes a self-reference and produces a
-- recursive encoding, and produce the fixed-point encoding.
recursive :: (Encoding dim ty -> Encoding dim ty)
          -- ^ A function that, given a self-reference,
          -- constructs an encoding.
          -> Encoding dim ty
recursive genfunc =
  let
    enc = Encoding { encEncode = encode (genfunc enc),
                     encDecode = decode (genfunc enc),
                     encInDomain = inDomain (genfunc enc),
                     encDepth = depth (genfunc enc),
                     encMaxDepth = maxDepth (genfunc enc),
                     encSize = Nothing,
                     encHighestIndex = const (const Nothing) }
  in
    enc
{-
-- | 
recursive2 :: ((Encoding dim ty1, Encoding dim ty2) -> Encoding dim ty1)
           -- ^ A function that, given self-references to both encodings,
           -- constructs the first encoding.
           -> ((Encoding dim ty1, Encoding dim ty2) -> Encoding dim ty1)
           -- ^ A function that, given self-references to both encodings,
           -- constructs the second encoding.
           -> Encoding dim ty
recursive2 genfunc1 genfunc2 =
  let
    newMaxDepth1 =
      let
        Encoding { encMaxDepth = maxdepthfunc } = genfunc1 void
      in
        maxdepthfunc

    newMaxDepth2 =
      let
        Encoding { encMaxDepth = maxdepthfunc } = genfunc2 void
      in
        maxdepthfunc

    (enc1, enc2) =
      let
        recDecode1 num =
          let
            Encoding { encDecode = decodefunc } = genfunc enc1
          in
            decodefunc num

        recEncode1 val =
          let
            Encoding { encEncode = encodefunc } = genfunc enc1
          in
            encodefunc val

        recDepth1 dim val =
          let
            Encoding { encDepth = depthfunc } = genfunc enc1
          in
            depthfunc dim val

        recInDomain1 val =
          let
            Encoding { encInDomain = indomainfunc } = genfunc enc1
          in
            indomainfunc val

        recDecode2 num =
          let
            Encoding { encDecode = decodefunc } = genfunc enc2
          in
            decodefunc num
      in
        (Encoding { encEncode = recEncode1, encDecode = recDecode1,
                    encSize = Nothing, encInDomain = recInDomain1,
                    encDepth = recDepth1, encMaxDepth = newMaxDepth1,
                    encHighestIndex = const (const Nothing) },
         Encoding { encEncode = recEncode2, encDecode = recDecode2,
                    encSize = Nothing, encInDomain = recInDomain2,
                    encDepth = recDepth2, encMaxDepth = newMaxDepth2,
                    encHighestIndex = const (const Nothing) })
  in
    enc
-}
-- THIS CODE IS FROM HASKELLWIKI, AND THEREFORE NOT SUBJECT TO THE
-- COPYRIGHT CLAIM AT THE TOP OF THIS FILE
--
-- This should eventually be replaced with an implementation that
-- calls GMP.
(^!) :: Num a => a -> Int -> a
(^!) x n = x ^ n

isqrt :: Integer -> Integer
isqrt 0 = 0
isqrt 1 = 1
isqrt n =
  let
    twopows = iterate (^! 2) 2
    (lowerRoot, lowerN) =
      last $ takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
    newtonStep x = div (x + div n x) 2
    iters = iterate newtonStep (isqrt (div n lowerN) * lowerRoot)
    isRoot r = r ^! 2 <= n && n < (r + 1) ^! 2
  in
    head $ dropWhile (not . isRoot) iters

-- END CODE NOT SUBJECT TO COPYRIGHT CLAIM
