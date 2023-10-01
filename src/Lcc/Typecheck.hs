{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lcc.Typecheck where

-- import Lcc.AST
--   (
--     Exp
--   )
import Lcc.Util (patterns)

data TExp
  = TAbs TExp TExp
  | TApp TExp TExp
  | TVal
  deriving Show

data TIns
  = TFun TIns TIns
  | TCal TIns TIns
  | TCon
  deriving (Eq, Show)

patterns ''TIns

check :: [TIns] -> TIns
check is
  | null is = error "empty type expression list"
  | length is == 1 = head is
  | otherwise = let (f:x:xs) = is
                in case eval f x of
                     TCal f' x' -> check $ f' : x' : xs
                     x'@_ -> check $ x' : xs
 where
  -- eval :: TIns -> TIns -> TIns
  eval f x
   | tcon f = error "lhs must be function"
   | otherwise = let (TFun i o) = f
                 in if i == x
                    then o
                    else error "argument type doesn't match parameter type"

check' :: [TIns] -> Bool
check' = (== TCon) . check

{-
patterns ''TExp

type' :: Exp -> TExp
type' = undefined

check :: TExp -> TExp
check e = case e of
            TAbs h b -> TAbs h (check b)
            TApp f x
              | tabs f -> let (TAbs h b) = f
                          in if x == h then b else error "types of argument and parameter differ"
              | tapp f -> check $ TApp (check f) x
              | otherwise -> error "Val cannot be lhs"
            TVal -> e

check' :: TExp -> Bool
check' = (== TVal) . check
-}
