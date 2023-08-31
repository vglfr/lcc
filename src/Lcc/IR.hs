{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lcc.IR where

import Prelude hiding (abs)

import Data.List (intercalate, uncons)
import Data.Maybe (fromJust)

import Lcc.AST
  (
    Exp' (Abs', App', Var')
  )
import Debug.Trace (traceShowId)

newtype Spool a = Spool [a] deriving Eq

data IR
  = Start [Ins]
  | Proc Label [Ins]
  deriving Eq

data Ins
  = Cal Int
  | Loa Dat
  deriving (Eq, Show)

data Dat
  = Arg Int
  | Ref Int
  | Ret
  | Val Char
  deriving (Eq, Show)

type Label = Int

instance Show a => Show (Spool a) where
  show (Spool as) = intercalate "\n\n" $ fmap show as

instance Show IR where
  show (Start is) = intercalate "\n" $ "start:" : fmap (offset 2 . show) is
  show (Proc n is) = intercalate "\n" $ show n <> ":" : fmap (offset 2 . show) is

spool :: Exp' -> Spool IR
spool e = let (s, ps) = traceShowId . fromJust . uncons . flatten $ e
           in Spool $ spools s : fmap spoolp (filter abs ps)
 where
  -- flatten :: Exp' -> [Exp']
  flatten e' = case e' of
              Abs' _ _ b -> e' : flatten b
              App' f xs -> e' : flatten f <> concatMap flatten xs
              Var' _ -> pure e'

  -- spools :: Exp' -> IR
  spools = Start . spool'

  -- spoolp :: Exp' -> IR
  spoolp (Abs' n _ _) = Proc n [Loa $ Arg 0]

  -- spool' :: Exp' -> [Ins]
  spool' e' = case e' of
               Abs' _ _ b -> spool' b
               App' (Abs' n as _) xs -> concatMap spool' xs <> [Loa $ Ref n, Cal $ length as + 1, Loa Ret]
               Var' c -> [Loa . Val $ c]

  abs (Abs' {}) = True
  abs _ = False

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s