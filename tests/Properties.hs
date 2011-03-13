{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.ByteString.Char8
import Codec.Compression.QuickLZ

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain [ testGroup "simple" [
                       testProperty "compress identity" prop_id
                     , testProperty "decompress transparent" prop_decompress_id
                     ]
                   ]

prop_id :: String -> Bool
prop_id (pack -> xs) =
  xs == (decompress . compress) xs

prop_decompress_id :: String -> Bool
prop_decompress_id (pack -> xs) =
  let z  = compress xs
  in (decompress z) == (decompress z)

