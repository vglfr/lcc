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
  deriving Eq

data Exp'
  = Abs' Name Exp'
  | App' Exp' Exp'
  | Bin' Name
  | Unb' Char
  deriving (Eq, Show)

type Name = Int

patterns ''Exp
patterns ''Exp'

instance Show Exp where
  showsPrec n e = showsPrec' n (disc' e) e

instance IsString Exp where
  fromString = Var

enrich :: Exp -> Exp'
enrich = third . go . (1,[],)
 where
  go (n,m,e) = case e of
                 Abs h b -> let (n',_,b') = go (n+1, (h,n) : m, b)
                            in (n', m, Abs' n b')
                 App f x -> let (n' ,_,f') = go (n,m,f)
                                (n'',_,x') = if app x
                                             then go (n',m,Abs (Var "a") (App x (Var "a")))
                                             else go (n',m,x)
                            in (n'', m, App' f' x')
                 Var v -> (n,m,) $ case lookup e m of
                                     Just n' -> Bin' n'
                                     Nothing -> Unb' (head v)
  third (_,_,x) = x

partial :: Exp -> Bool
partial = \case
            App _f _x -> undefined
            _ -> False

sfilter :: (Exp' -> Bool) -> (Exp' -> Bool) -> Exp' -> [Exp']
sfilter p q e = let e0 = if q e then [e] else []
                in e0 <> case e of
                           Abs' _ b -> sfilter' b
                           App' f x -> sfilter' f <> sfilter' x
                           _ -> []
 where
  sfilter' e' = if p e' then sfilter p q e' else []

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
