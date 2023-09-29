{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lcc.Typecheck where

import Lcc.AST
  (
    Exp
  )
import Lcc.Util (patterns)

data TExp
  = TAbs TExp TExp
  | TApp TExp TExp
  | TVal
  deriving (Eq, Show)

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
