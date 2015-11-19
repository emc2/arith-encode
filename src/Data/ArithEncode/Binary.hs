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
{-# OPTIONS_GHC -Wall #-}

-- | Facilities for using @Encoding@s as binary serializers.  The
-- resulting binary format is, for the most part, determined by the
-- @Encoding@, and therefore is within a constant factor of
-- succintness.
--
-- In all cases, little-endian byte ordering is used in order to allow
-- for very large data to be read in an decoded lazily.  (Note:
-- Haskell's libraries do not provide support for this functionality
-- at this time).
--
-- For finite @Encoding@s, the binary format is just the little-endian
-- encoding of the encoded value, using as few bytes as necessary to
-- represent the largest encoded value.
--
-- For infinite @Encoding@s, the binary format includes a length field
-- for most values.  The current encoding uses length fields of
-- different sizes, depending on the size of the encoded value.
module Data.ArithEncode.Binary(
       getWithEncoding,
       putWithEncoding
       ) where

import Data.ArithEncode.Basic
import Data.Binary.Put
import Data.Binary.Get hiding (remaining)
import Data.Bits
import Math.NumberTheory.Logarithms

-- Read in a natural number as a sequence of some number of bytes
getNatural :: Int -> Get Integer
getNatural bytes =
  let
    getNatural' :: Integer -> Int -> Get Integer
    getNatural' accum count
      | count + 8 < bytes =
        do
          input <- getWord64le
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum) (count + 8)
      | count + 4 < bytes =
        do
          input <- getWord32le
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum) (count + 4)
      | count + 2 < bytes =
        do
          input <- getWord16le
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum) (count + 2)
      | count < bytes =
        do
          input <- getWord8
          getNatural' ((toInteger input `shiftL` (count * 8)) .|. accum) (count + 1)
      | otherwise = return accum
  in
    getNatural' 0 0

-- | Use an @Encoding@ to extract a @ty@ from binary data.
getWithEncoding :: Encoding ty
                -- ^ The encoding to use.
                -> Get ty
getWithEncoding enc =
  case size enc of
    Just 0 -> error "Cannot decode with empty encoding"
    -- For the degenerate case of a singleton, no need to encode anything at all
    Just 1 -> return (decode enc 0)
    -- Otherwise store the natural as a sequence of bytes.  We store
    -- this in little-endian order to allow lazy handling of very large
    -- numbers.
    Just finitesize ->
      let
        bytes = ((integerLog2 (finitesize - 1)) `quot` 3) + 1
      in do
        encoded <- getNatural bytes
        return (decode enc encoded)
    -- Arbitrary-length naturals are encoded with a more complex
    -- scheme.  The first two bits are a tag, which tells how to
    -- interpret the rest.
    Nothing ->
      do
        firstbyte <- lookAhead getWord8
        encoded <-
          case firstbyte .&. 0x03 of
            -- Naturals less than 64 get packed into the same byte as the tag
            0x0 ->
              do
                datafield <- getWord8
                return (toInteger (datafield `shiftR` 2))
            -- One-byte length field, and then up to 64 bytes of data
            0x1 ->
              do
                lenfield <- getWord8
                getNatural (fromIntegral (lenfield `shiftR` 2) + 1)
            -- Two-byte length field, and then up to 16384 bytes of data
            0x2 ->
              do
                lenfield <- getWord16le
                getNatural (fromIntegral (lenfield `shiftR` 2) + 1)
            -- Eight-byte length field, and then data
            0x3 ->
              do
                lenfield <- getWord64le
                getNatural (fromIntegral (lenfield `shiftR` 2) + 1)
            _ -> error "Impossible case"
        return (decode enc encoded)

-- Emit a natural number as a sequence of some number of bytes
putNatural :: Int -> Integer -> Put
putNatural 0 0 = return ()
putNatural 0 _ = error "Data remaining at end of encoding"
putNatural remaining natural
  | remaining > 8 =
    let
      output = fromInteger (natural .&. 0xffffffffffffffff)
      rest = natural `shiftR` 64
    in do
      putWord64le output
      putNatural (remaining - 8) rest
  | remaining > 4 =
    let
      output = fromInteger (natural .&. 0xffffffff)
      rest = natural `shiftR` 32
    in do
      putWord32le output
      putNatural (remaining - 4) rest
  | remaining > 2 =
    let
      output = fromInteger (natural .&. 0xffff)
      rest = natural `shiftR` 16
    in do
      putWord16le output
      putNatural (remaining - 2) rest
  | otherwise =
    let
      output = fromInteger (natural .&. 0xff)
      rest = natural `shiftR` 8
    in do
      putWord8 output
      putNatural (remaining - 1) rest

-- | Use an @Encoding@ to write a @ty@ out as binary data.
putWithEncoding :: Encoding ty
                -- ^ The encoding to use.
                -> ty
                -- ^ The value to encode.
                -> Put
putWithEncoding enc val =
  case size enc of
    Just 0 -> error "Cannot encode with empty encoding"
    -- For the degenerate case of a singleton, no need to encode anything at all
    Just 1 -> return ()
    -- Otherwise store the natural as a sequence of bytes.  We store
    -- this in little-endian order to allow lazy handling of very large
    -- numbers.
    Just finitesize ->
      let
        bytes = ((integerLog2 (finitesize - 1)) `quot` 3) + 1
        encoded = encode enc val
      in
        putNatural bytes encoded
    Nothing ->
      let
        encoded = encode enc val
      in
        if encoded < 64
          then putWord8 (fromInteger encoded `shiftL` 2)
          else
            let
              bytes = ((integerLog2 (encoded - 1)) `quot` 3) + 1
            in do
              if bytes <= 64
                then putWord8 (fromIntegral (((bytes - 1) `shiftL` 2) .|. 0x1))
                else if bytes <= 16384
                  then putWord16le (fromIntegral (((bytes - 1) `shiftL` 2) .|. 0x2))
                  else putWord64le (fromIntegral (((bytes - 1) `shiftL` 2) .|. 0x3))
              putNatural bytes encoded
