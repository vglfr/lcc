module IR where

import Test.Hspec (Spec, describe, it, shouldBe)

import Lcc.Example
  (
    e1', e2', e3', e4', e5', e6', e7', e8', e9', e10'
  , i1, i2, i3, i4, i5, i6, i7, i8, i9, i10
  )
import Lcc.IR (spool)

testSpool :: Spec
testSpool = describe "Lcc.IR" $ do
  it "spool e1'" $ do
    spool e1' `shouldBe` i1

  it "spool e2'" $ do
    spool e2' `shouldBe` i2

  it "spool e3'" $ do
    spool e3' `shouldBe` i3

  it "spool e4'" $ do
    spool e4' `shouldBe` i4

  it "spool e5'" $ do
    spool e5' `shouldBe` i5

  it "spool e6'" $ do
    spool e6' `shouldBe` i6

  it "spool e7'" $ do
    spool e7' `shouldBe` i7

  it "spool e8'" $ do
    spool e8' `shouldBe` i8

  it "spool e9'" $ do
    spool e9' `shouldBe` i9

  it "spool e10'" $ do
    spool e10' `shouldBe` i10
