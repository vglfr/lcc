module Main where

import Test.Hspec (hspec)

import AST (testEnrich)
import IR (testSpool)

main :: IO ()
main = hspec $ do
  testEnrich
  testSpool
