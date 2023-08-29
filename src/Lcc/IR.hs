module Lcc.IR where

import Data.List (intercalate)

import Lcc.AST (Exp)

newtype Spool a = Spool [a]

data IR
  = Start [Ins]
  | Proc Label [Ins]

data Ins
  = Cal Int
  | Loa Dat

data Dat
  = Arg Int
  | Ref Int
  | Ret
  | Val Char

type Label = Int

instance Show a => Show (Spool a) where
  show (Spool as) = intercalate "\n\n" $ fmap show as

instance Show IR where
  show (Start is) = intercalate "\n" $ "start:" : fmap (offset 2 . show) is
  show (Proc n is) = intercalate "\n" $ show n <> ":" : fmap (offset 2 . show) is

instance Show Ins where
  show (Cal i) = "call " <> show i
  show (Loa v) = "push " <> show v

instance Show Dat where
  show (Arg x) = "'" <> show x
  show (Ref x) = "[" <> show x <> "]"
  show Ret     = "rax"
  show (Val x) = "'" <> pure x <> "'"

spool :: Exp -> Spool IR
spool _e = Spool mempty

{-
5 * (5 * 5) -> 5 * 5 * 5

  *                  *
 / \                / \
5   *       ->     *   5
   / \            / \
  5   5          5   5
-}
leftify :: Exp -> Exp
leftify = undefined

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s