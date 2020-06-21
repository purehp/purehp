module Main (main) where

import Prelude ()
import Prelude.Compat
import Test.Tasty
import System.IO (hSetEncoding, stdout, stderr, utf8)

import qualified TestUtils
import qualified TestGolden

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Updating support code"
  TestUtils.updateSupportCode

  goldenTests <- TestGolden.main

  defaultMain $
    testGroup
      "Tests"
      [ goldenTests ]

  where
    heading msg = do
      putStrLn ""
      putStrLn $ replicate 79 '#'
      putStrLn $ "# " ++ msg
      putStrLn $ replicate 79 '#'
      putStrLn ""
