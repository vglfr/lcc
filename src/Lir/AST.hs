module Lir.AST where

import Data.String (IsString, fromString)
import GHC.Show (showSpace)

data Exp
  = Abs Exp Exp
  | App Exp Exp
  | Var String

instance Show Exp where
  showsPrec _ (Var x)   = showString x
  showsPrec _ (Abs h b) = showString "λ" . shows h . showAbs b
   where
    showAbs (Abs h' b') = shows h' . showAbs b'
    showAbs e           = showString "." . showInner e
    showInner e@(Abs _ _) = showParen True $ shows e
    showInner (App f x)   = showInner f . showInner x
    showInner e           = shows e
  showsPrec _ (App f x) = shows f . showSpace . shows x

instance IsString Exp where
  fromString = Var

λ :: Exp -> Exp -> Exp
λ = Abs

(∘) :: Exp -> Exp -> Exp
(∘) = App