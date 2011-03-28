module Main where

import Control.Monad
import Criterion.Main

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Codec.Compression.QuickLZ as QLZ
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Compression.Snappy as Snappy

main :: IO ()
main = 
  defaultMain [ bgroup "compression" [
                  bench "quicklz" quicklz_comp
                , bench "zlib" zlib_comp
                , bench "bzip" bzip_comp
                , bench "snappy" snappy_comp
                ]
              , bgroup "decompress" [
                  bench "quicklz" quicklz_decomp
                , bench "quicklz [overlap]" quicklz_overlap_decomp
                , bench "zlib" zlib_decomp
                , bench "bzip" bzip_decomp
                , bench "snappy" snappy_decomp
                ]
              ]
  where quicklz_comp   = nfIO $ liftS QLZ.compress "words"
        quicklz_decomp = nfIO $ liftS QLZ.decompress "words_quicklz"
        quicklz_overlap_decomp = nfIO $ liftS QLZ.decompress' "words_quicklz"
        
        zlib_comp   = nfIO $ liftL Zlib.compress "words"
        zlib_decomp = nfIO $ liftL Zlib.decompress "words_zlib"

        bzip_comp   = nfIO $ liftL BZip.compress "words"
        bzip_decomp = nfIO $ liftL BZip.decompress "words_bzip"

        snappy_comp   = nfIO $ liftS Snappy.compress "words"
        snappy_decomp = nfIO $ liftS Snappy.decompress "words_snappy"

-- utilities
liftS c f = (S.length . c) `liftM` S.readFile f
liftL c f = (L.length . c) `liftM` L.readFile f
