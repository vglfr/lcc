{-# LANGUAGE LambdaCase #-}

module Lcc.AST where

import Data.Function (applyWhen)
import Data.String (IsString, fromString)
import GHC.Show (showSpace)

data Exp
  = Abs Exp Exp
  | App Exp Exp
  | Var String

data Exp'
  = Abs' Name Name Exp' Exp'
  | App' Name Name Exp' Exp'
  | Bin' Name Name
  | Unb' Char

type Name = Int

instance Show Exp where
  showsPrec n e = showsPrec' n (disc' e) e

instance Show Exp' where
  show = undefined

instance IsString Exp where
  fromString = Var

-- enrich :: Exp -> Exp'
-- enrich = go 0
--  where
--   go :: Int -> Exp -> Exp'
--   go n e = case e of
--              Abs h b -> Abs' n [go n h] (go (n+1) b)
--              App f x -> App' (go n f) [go n x]
--              Var v -> Var' $ head v

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
                   Abs h b -> showParen (n > 2) $ shows h . applyWhen (disc' b /= 0) (showString ".") . showsPrec' 1 0 b
                   App f x -> showParen (n > 1) $ showsPrec' 1 1 f . applyWhen (p == 1) showSpace . (showParen (disc' x == 0 && p == 0) $ showsPrec' 2 1 x)
                   Var x -> showString x
