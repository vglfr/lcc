{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lcc.Util where

import Data.Char (toLower)
import Language.Haskell.TH
  (
    Body (NormalB)
  , Clause (Clause)
  , Con (NormalC)
  , Dec (DataD, SigD, FunD)
  , Exp (ConE)
  , Info (TyConI)
  , Name
  , Pat (RecP, WildP)
  , Q
  , Type (AppT, ArrowT, ConT)
  , mkName, nameBase, reify
  )

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s

patterns :: Name -> Q [Dec]
patterns t = do
  (TyConI c) <- reify t
  (DataD _ n _ _ cs _) <- pure c
  concat <$> traverse (pattern n) cs

pattern :: Name -> Con -> Q [Dec]
pattern n (NormalC n' _) = let c = nameBase n'
                               f = mkName $ toLower (head c) : tail c
                            in pure
  [
    SigD f (AppT (AppT ArrowT (ConT n)) (ConT $ mkName "Bool"))
  , FunD f [
             Clause [RecP n' []] (NormalB (ConE $ mkName "True")) []
           , Clause [WildP] (NormalB (ConE $ mkName "False")) []
           ]
  ]