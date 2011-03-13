{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Criterion.Main

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Codec.Compression.QuickLZ as QLZ
import qualified Codec.Compression.Zlib as Zlib

main = defaultMain [
    bench "quicklz /usr/share/dict/words" quicklz
  , bench "zlib /usr/share/dict/words" zlib
  ]
  where quicklz = do
          !x <- (S.length . QLZ.compress) `liftM` S.readFile "words"
          return ()
        zlib = do
          !x <- (L.length . Zlib.compress) `liftM` L.readFile "words"
          return ()
