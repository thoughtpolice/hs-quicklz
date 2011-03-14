{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Criterion.Main

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Codec.Compression.QuickLZ as QLZ
import qualified Codec.Compression.Zlib as Zlib
--import qualified Codec.Compression.BZip as BZip

main :: IO ()
main = 
  defaultMain [ bgroup "compression speed" [
                  bench "quicklz compress words" quicklz_comp
                , bench "zlib compress words" zlib_comp
--              , bench "bzip compress words" bzip_comp
                ]
              , bgroup "decompress speed" [
                  bench "quicklz decompress words" quicklz_decomp
                , bench "quicklz overlap decompress words" quicklz_overlap_decomp
                , bench "zlib decompress words" zlib_decomp
--              , bench "bzip decompress words" bzip_decomp
                ]
              ]
  where quicklz_comp   = nfIO $ liftS QLZ.compress "words"
        quicklz_decomp = nfIO $ liftS QLZ.decompress "words_quicklz"
        quicklz_overlap_decomp = nfIO $ liftS QLZ.decompress' "words_quicklz"
        
        zlib_comp   = nfIO $ liftL Zlib.compress "words"
        zlib_decomp = nfIO $ liftL Zlib.decompress "words_zlib"

-- utilities
liftS c f = (S.length . c) `liftM` S.readFile f
liftL c f = (L.length . c) `liftM` L.readFile f
