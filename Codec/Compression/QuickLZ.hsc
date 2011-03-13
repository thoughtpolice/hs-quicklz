{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Codec.Compression.QuickLZ
-- Copyright   : (c) Austin Seipp 2011
-- License     : GPLv2
-- 
-- Maintainer  : as@hacks.yi.org
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides a high level 'ByteString' interface to
-- the QuickLZ library. More info about quicklz can be found here:
-- http://quicklz.com
-- 
-- QuickLZ is fast and compresses very well.
-- The library that is bundled with this version is QuickLZ v1.5.0,
-- with the compression level set to 1.
-- 
-- Streaming/enumerator interface coming soon.
-- 
module Codec.Compression.QuickLZ
( -- * Compressing and decompressing strict 'ByteString's
  compress      -- :: S.ByteString -> S.ByteString
, decompress    -- :: S.ByteString -> S.ByteString
) where

import Foreign
import Foreign.C

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Unsafe as U

#include <quicklz.h>

-- 
-- High-level interface
-- 

-- | Compresses the input 'ByteString'.
compress :: S.ByteString -> S.ByteString
compress xs
  | S.null xs = S.empty
  | otherwise = 
      unsafePerformIO . allocaBytes qlz_state_compress_sz $ \compress_state ->
        SI.createAndTrim (S.length xs + 400) $ \output -> do
          U.unsafeUseAsCStringLen xs $ \(cstr,len) -> 
            c_qlz_compress cstr output len compress_state


-- | Decompress the input 'ByteString'.
decompress :: S.ByteString -> S.ByteString
decompress xs
  | S.null xs = S.empty
  | otherwise = 
      unsafePerformIO . allocaBytes qlz_state_decompress_sz $ \decompress_state -> do
        sz <- U.unsafeUseAsCString xs c_qlz_size_decompressed
        SI.createAndTrim sz $ \output -> do
          U.unsafeUseAsCString xs $ \cstr -> 
            c_qlz_decompress cstr output decompress_state

-- 
-- Simple bindings to some constants
-- 
qlz_state_compress_sz :: Int
qlz_state_compress_sz = #{size qlz_state_compress}
qlz_state_decompress_sz :: Int
qlz_state_decompress_sz = #{size qlz_state_decompress}

--
-- FFI Bindings
-- 
foreign import ccall unsafe "quicklz.h qlz_compress"
  c_qlz_compress :: Ptr a -> Ptr b -> Int -> Ptr c -> IO Int

foreign import ccall unsafe "quicklz.h qlz_decompress"
  c_qlz_decompress :: Ptr a -> Ptr b -> Ptr c -> IO Int

foreign import ccall unsafe "quicklz.h qlz_size_decompressed"
  c_qlz_size_decompressed :: CString -> IO Int
