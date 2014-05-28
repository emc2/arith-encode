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
       IllegalArgument,
       encode,
       decode,
       size,
       maxDepth,
       depth,
       highestIndex,

       -- * Building Encodings

       -- ** Basic Encodings
       identityEncoding,
       integralEncoding,
       intervalEncoding,
       fromHashableList,
       fromOrdList,

       -- ** Constructions
       wrapEncoding
       ) where

import Control.Exception
import Data.Bits
import Data.Hashable
import Data.Typeable

import qualified Data.Array as Array
import qualified Data.HashMap as HashMap
import qualified Data.Map as Map

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
    encSize :: Maybe Integer,
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
           -> (dim -> Maybe Integer)
           -- ^ A function indicating the maximum depth of any dimension.
           -> (dim -> ty -> Integer)
           -- ^ A function indicating the depth of a @ty@ in dimension @dim@.
           -> (dim -> Integer -> Maybe Integer)
           -- ^ A function indicating the last number mapping to a
           -- @ty@ less than a given depth in a given dimension.
           -> Encoding dim ty
mkEncoding encodefunc decodefunc sizeval maxdepthfunc depthfunc highindexfunc =
  Encoding { encEncode = encodefunc, encDecode = decodefunc,
             encSize = sizeval, encMaxDepth = maxdepthfunc,
             encDepth = depthfunc, encHighestIndex = highindexfunc }

-- | Create an infinite-sized encoding.
mkInfEncoding :: Enum dim
              => (ty -> Integer)
              -- ^ The encoding function.
              -> (Integer -> ty)
              -- ^ The decoding function.  Can assume all inputs are positive.
              -> (dim -> Maybe Integer)
              -- ^ A function indicating the maximum depth of any dimension.
              -> (dim -> ty -> Integer)
              -- ^ A function indicating the depth of a @ty@ in dimension @dim@.
              -> (dim -> Integer -> Maybe Integer)
              -- ^ A function indicating the last number mapping to a
              -- @ty@ less than a given depth in a given dimension.
              -> Encoding dim ty
mkInfEncoding encodefunc decodefunc =
  mkEncoding encodefunc decodefunc Nothing

-- | Create a dimensionless (potentially finite) encoding.
mkDimlessEncoding :: (ty -> Integer)
                  -- ^ The encoding function.
                  -> (Integer -> ty)
                  -- ^ The decoding function.  Can assume all inputs
                  -- are positive.
                  -> Maybe Integer
                     -- ^ The number of mappings, or 'Nothing' if it is infinite.
                  -> Encoding () ty
mkDimlessEncoding encodefunc decodefunc sizeval =
  mkEncoding encodefunc decodefunc sizeval (\() -> Just 0)
             (\() _ -> 0) (\() _ -> sizeval)

-- | Create an infinite, dimensionless encoding.
mkInfDimlessEncoding :: (ty -> Integer)
                     -- ^ The encoding function.
                     -> (Integer -> ty)
                     -- ^ The decoding function.  Can assume all
                     -- inputs are positive.
                     -> Encoding () ty
mkInfDimlessEncoding encodefunc decodefunc =
  mkDimlessEncoding encodefunc decodefunc Nothing

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
identityEncoding :: Encoding () Integer
identityEncoding = mkInfDimlessEncoding id id

-- | An encoding of /all/ integers into the positive integers.
integralEncoding :: Integral n => Encoding () n
integralEncoding =
  let
    encodefunc num
      | num < 0 = ((abs (toInteger num) - 1) `shiftL` 1) `setBit` 0
      | otherwise = (toInteger num) `shiftL` 1

    decodefunc num
      | num `testBit` 0 = fromInteger (-((num `shiftR` 1) + 1))
      | otherwise = fromInteger (num `shiftR` 1)
  in
    mkInfDimlessEncoding encodefunc decodefunc

-- | Build an encoding from a finite range of 'Integral's.
intervalEncoding :: (Show n, Integral n)
                 => n
                 -- ^ The (inclusive) lower bound on the range.
                 -> n
                 -- ^ The (exclusive) upper bound on the range.
                 -> Encoding () n
intervalEncoding lower upper
  | lower <= upper =
    let
      biglower = toInteger lower

      encodefunc num
        | num < lower =
          throw (IllegalArgument ("value " ++ show num ++
                                  " is less than lower bound " ++ show lower))
        | num > upper =
          throw (IllegalArgument ("value " ++ show num ++
                                  " is greater or equal to upper bound " ++
                                  show upper))
        | otherwise = (toInteger num) - biglower

      decodefunc num = fromInteger (num + biglower)

      sizeval = Just ((toInteger upper) - (toInteger lower) + 1)
    in
       mkDimlessEncoding encodefunc decodefunc sizeval
  | otherwise = error "Lower bound is not less than upper bound"

-- | Build an encoding from a list of 'Hashable' items.
fromHashableList :: (Hashable ty, Ord ty)
                 => [ty]
                 -- ^ A list of items to encode.
                 -> Encoding () ty
                 -- ^ An encoding mapping the items in the list to
                 -- natural numbers.
fromHashableList elems =
  let
    len = length elems
    revmap = Array.listArray (0, len) elems
    fwdmap = HashMap.fromList (zip elems [0..len])
    encodefunc = toInteger . (HashMap.!) fwdmap
    decodefunc = (Array.!) revmap . fromInteger
    sizeval = Just (toInteger len)
  in
    mkDimlessEncoding encodefunc decodefunc sizeval

-- | Build an encoding from a list of 'Hashable' items.
fromOrdList :: Ord ty
            => [ty]
            -- ^ A list of items to encode.
            -> Encoding () ty
            -- ^ An encoding mapping the items in the list to natural
            -- numbers.
fromOrdList elems =
  let
    len = length elems
    revmap = Array.listArray (0, len) elems
    fwdmap = Map.fromList (zip elems [0..len])
    encodefunc = toInteger . (Map.!) fwdmap
    decodefunc = (Array.!) revmap . fromInteger
    sizeval = Just (toInteger len)
  in
    mkDimlessEncoding encodefunc decodefunc sizeval

-- | Wrap an encoding using a pair of functions.  These functions must
-- also define an isomorphism.
--
-- The resulting encoding from
-- > wrapEncoding fwd rev enc
-- implements @depth@ as @depth enc . fwd@, which only works if @fwd@
-- preserves all depths.  For more complex cases, use @mkEncoding@ to
-- define a new encoding.
wrapEncoding :: (a -> b)
             -- ^ The forward encoding function.
             -> (b -> a)
             -- ^ The reverse encoding function.
             -> Encoding dim b
             -- ^ The inner encoding.
             -> Encoding dim a
wrapEncoding fwd rev enc @ Encoding { encEncode = encodefunc,
                                      encDecode = decodefunc,
                                      encDepth = depthfunc } =
  enc { encEncode = encodefunc . fwd,
        encDecode = rev . decodefunc,
        encDepth = (\dim -> depthfunc dim . fwd) }
