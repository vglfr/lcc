{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lcc.AST where

import Prelude hiding (abs)

import Data.Function (applyWhen)
import Data.String (IsString, fromString)
import GHC.Show (showSpace)

import Lcc.Util (patterns)

data Exp
  = Abs Exp Exp
  | App Exp Exp
  | Var String

data Exp'
  = Abs' Name Exp'
  | App' Name Name Exp' Exp'
  | Bin' Name
  | Unb' Char
  deriving Show

type Name = Int

patterns ''Exp

instance Show Exp where
  showsPrec n e = showsPrec' n (disc' e) e

-- instance Show Exp' where
--   show = undefined

instance IsString Exp where
  fromString = Var

-- convert to Exp'
-- name functions & assign arities
-- rename binded variables
enrich :: Exp -> Exp'
enrich = go 0
 where
  go :: Int -> Exp -> Exp'
  go n e = case e of
             Abs _ b -> Abs' n (go (n+1) b)
             App f x -> App' n 0 (go (n+1) f) (go (n+1) x)
             Var v -> Unb' $ head v

λ :: Exp -> Exp -> Exp
λ = Abs

(∘) :: Exp -> Exp -> Exp
(∘) = App

disc' :: Exp -> Int
disc' = \case
          Abs {} -> 0
          App {} -> 1
          Var {} -> 2

showsPrec' :: Int -> Int -> Exp -> ShowS
showsPrec' n p = \case
                   Abs h b -> showParen (n > 2) $ shows h . applyWhen (not $ abs b) (showString ".") . showsPrec' 1 0 b
                   App f x -> showParen (n > 1) $ showsPrec' 1 1 f . applyWhen (p == 1) showSpace . (showParen (abs x && p == 0) $ showsPrec' 2 1 x)
                   Var x -> showString x
