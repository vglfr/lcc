{-# LANGUAGE OverloadedStrings #-}

module Lir.Example where

import Lir.AST (λ, (∘), Exp)
import Lir.IR (Ins (Cal), Dat (Pro, Val))

{- λx.x y -> y -}
e1 :: Exp
e1 = λ "x" "x" ∘ "y"

i1 :: [Ins]
i1 = [
       Cal (Pro 0) (Val 'y')
     ]