{-# LANGUAGE LambdaCase #-}

module Lcc.IR where

import Prelude hiding (abs)
import Data.List (intercalate)

-- import Lcc.AST
--   (
--     Exp' (Abs', App', Bin', Unb')
--   )

newtype Spool a = Spool [a] deriving Eq

data IR
  = Start [Ins]
  | Proc Label [Ins]

data Ins
  = Cal Dat Dat
  | End Dat
  | Sav Dat
  deriving Show

data Dat
  = Bin
  | Con Char
  | Ref Int
  | Ret
  | Unb Int
  deriving Show

-- type Arity = Int
type Label = Int

instance Show a => Show (Spool a) where
  show (Spool as) = intercalate "\n\n" $ fmap show as

instance Show IR where
  show (Start is) = intercalate "\n" $ "start:" : fmap (offset 2 . show) is
  show (Proc s is) = intercalate "\n" $ show s <> ":" : fmap (offset 2 . show) is

{-
spool :: Exp' -> Spool IR
spool e = let ps = flatten e
           in Spool $ spools e : fmap spoolp ps
 where
  flatten e' = case e' of
                 Abs' _ _ b -> e' : flatten b
                 App' _ a f x
                   | a > 0 -> e' : flatten f <> flatten x
                   | otherwise -> flatten f <> flatten x
                 _ -> mempty

  -- spools :: Exp' -> IR
  spools e' = Start $ spool' e <> end e'

  -- spoolp :: Exp' -> IR
  spoolp e'@(Abs' l a _ _) = Proc l a $ spool' e'
  spoolp _ = undefined

  -- spool' :: Exp' -> [Ins]
  spool' = \case
             Abs' _ _ b -> spool' b
             App' _ _ f x -> pure $ Cal (dat f) (dat x)
             Bin' l a
               | a == 1 -> pure $ End Bin
               | otherwise -> pure $ End $ dat (Bin' l a)
             Unb' c -> pure $ End $ Con c

  -- dat :: Exp' -> Dat
  dat = \case
          Abs' l a _ _ -> Ref l a
          App' l a _ _ -> Ref l a
          Bin' l a -> Ref l a
          Unb' c -> Con c

  -- end :: Exp' -> [Ins]
  end = \case
          Unb' c -> [End $ Con c]
          _ -> [End Ret]
-}

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s