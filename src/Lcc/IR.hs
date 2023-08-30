module Lcc.IR where

import Prelude hiding (abs)

import Data.List (intercalate, uncons)
import Data.Maybe (fromJust)

import Lcc.AST
  (
    Exp (Abs, App, Var)
  , abs
  )
import Debug.Trace (traceShowId)

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
spool = Spool . fmap spool' . traceShowId . uncurry (:) . fmap (filter abs) . fromJust . uncons . collapse
 where
  collapse :: Exp -> [Exp]
  collapse e = case e of
              Abs _ b -> e : collapse b
              App f x -> e : collapse f <> collapse x
              Var _ -> pure e
  spool' :: Exp -> IR
  spool' = undefined
  -- spool' e' = case e' of
  --               Abs _ _ -> undefined
  --               App _ _ -> undefined
  --               Var _ -> undefined

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