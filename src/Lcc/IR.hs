{-# LANGUAGE LambdaCase #-}

module Lcc.IR where

import Prelude hiding (abs)
import Data.List (intercalate)

import Lcc.AST
  (
    Exp' (Abs', App', Bin', Unb')
  )

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
  = Arg
  | Bin Int
  | Ref Int
  | Ret
  | Unb Char
  deriving Show

type Label = Int

instance Show a => Show (Spool a) where
  show (Spool as) = intercalate "\n\n" $ fmap show as

instance Show IR where
  show (Start is) = intercalate "\n" $ "start:" : fmap (offset 2 . show) is
  show (Proc s is) = intercalate "\n" $ show s <> ":" : fmap (offset 2 . show) is

spool :: Exp' -> Spool IR
spool e = let ps = procedures e -- filter proc e
           in Spool $ spools e : fmap spoolp ps
 where
  procedures e' = case e' of
                    Abs' _ b -> e' : procedures b
                    App' _ _ f x -> procedures f <> procedures x
                    -- App' _ a f x
                    --   | a > 0 -> e' : flatten f <> flatten x
                    --   | otherwise -> flatten f <> flatten x
                    _ -> mempty

  -- spools :: Exp' -> IR
  spools e' = Start $ spool' e' -- <> end e'

  -- spoolp :: Exp' -> IR
  spoolp e'@(Abs' l _) = Proc l $ spool' e'
  spoolp _ = undefined

  -- spool' :: Exp' -> [Ins]
  spool' = \case
             Abs' l b -> case b of
                           Abs' l' _ -> [Sav (Bin l), End (Ref l')]
                           App' _ _ _f _x -> undefined
                           Bin' l' | l == l' -> [End Arg]
                                   | otherwise -> [End (Bin l')]
                           Unb' c -> [End (Unb c)]
             App' _ _ f x -> case f of
                               Abs' _ _ -> [Cal (dat f) (dat x), End Ret]
                               App' _ _ f' x' -> [Cal (dat f') (dat x'), Cal Ret (dat x), End Ret]
                               _ -> error "type error: value cannot be lhs"
             Unb' c -> [End (Unb c)]
             _ -> error "Bin' shouldn't be top level"

  -- spool'' :: Exp' -> [Ins]
  -- spool'' = \case
  --             Abs' _ _ -> [Cal _ _]
  --             App' _ _ _ _ -> [Cal Ret _]
  --             _ -> error "type error: value cannot be lhs"

  -- dat :: Exp' -> Dat
  dat = \case
          Abs' l _ -> Ref l
          App' l _ _ _ -> Ref l
          Bin' l -> Bin l
          Unb' c -> Unb c

  -- end :: Exp' -> [Ins]
  -- end = \case
  --         Unb' _ -> mempty
  --         _ -> pure $ End Ret

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s