module AST where

import Test.Hspec (Spec, describe, it, shouldBe)

import Lcc.AST (enrich)
import Lcc.Example
  (
    e1, e2, e3, e4, e5, e6, e7, e8, e9, e10
  , e1', e2', e3', e4', e5', e6', e7', e8', e9', e10'
  )

testEnrich :: Spec
testEnrich = describe "Lcc.AST" $ do
  it "enrich e1" $ do
    enrich e1 `shouldBe` e1'

  it "enrich e2" $ do
    enrich e2 `shouldBe` e2'

  it "enrich e3" $ do
    enrich e3 `shouldBe` e3'

  it "enrich e4" $ do
    enrich e4 `shouldBe` e4'

  it "enrich e5" $ do
    enrich e5 `shouldBe` e5'

  it "enrich e6" $ do
    enrich e6 `shouldBe` e6'

  it "enrich e7" $ do
    enrich e7 `shouldBe` e7'

  it "enrich e8" $ do
    enrich e8 `shouldBe` e8'

  it "enrich e9" $ do
    enrich e9 `shouldBe` e9'

  it "enrich e10" $ do
    enrich e10 `shouldBe` e10'
