{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lcc.IR where

import Prelude hiding (abs)
import Data.List (intercalate)

import Lcc.AST
  (
    Exp' (Abs', App', Bin', Unb')
  , abs', app', sfilter
  )
import Lcc.Util (offset)

newtype Spool a = Spool [a] deriving Eq

data IR
  = Start [Ins]
  | Proc Label [Ins]
  deriving Eq

data Ins
  = Cal Dat Dat
  | End Dat
  | Sav Dat
  deriving (Eq, Show)

data Dat
  = Arg
  | Bin Int
  | Ref Int
  | Ret
  | Unb Char
  deriving (Eq, Show)

type Label = Int

instance Show a => Show (Spool a) where
  show (Spool as) = intercalate "\n\n" $ fmap show as

instance Show IR where
  show (Start is) = intercalate "\n" $ "start:" : fmap (offset 2 . show) is
  show (Proc s is) = intercalate "\n" $ show s <> ":" : fmap (offset 2 . show) is

spool :: Exp' -> Spool IR
spool e = let ls = e : sfilter (const True) abs' e
              as = fmap (reverse . sfilter (not . abs') app') ls
          in Spool $ fmap ir (zip ls as)
 where
  ir (l,as) = case l of
                Abs' n _ -> Proc n $ spool' l as
                _ -> Start $ spool' l as

  spool' l as = save' l <> concatMap (call' l) as <> return' l

  save' = \case
            Abs' h b -> if abs' b then [Sav $ Bin h] else []
            _ -> []

  call' e' (App' _ f x) = let m = case e' of
                                    Abs' l _ -> l
                                    _ -> 0
                          in [Cal (dat m f) (dat m x)]

  return' e' = case e' of
                 Abs' l b -> case b of
                               Bin' _ -> [End $ dat l b]
                               _ -> [End $ dat 0 b]
                 _ -> [End $ dat 0 e']

  dat m = \case
            Abs' l _ -> Ref l
            App' {} -> Ret
            Bin' l -> if l == m then Arg else Bin l
            Unb' c -> Unb c
