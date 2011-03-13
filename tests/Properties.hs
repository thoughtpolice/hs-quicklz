{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.ByteString.Char8
import Codec.Compression.QuickLZ

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain [ testGroup "simple" [
                       testProperty "identity" prop_id
                     ]
                   ]

prop_id :: String -> Bool
prop_id (pack -> xs) = xs == (decompress . compress) xs
