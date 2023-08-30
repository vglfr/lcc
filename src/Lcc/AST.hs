module Lcc.AST where

import Data.String (IsString, fromString)
import GHC.Show (showSpace)

data Exp
  = Abs Exp Exp
  | App Exp Exp
  | Var String
  deriving Eq

instance Show Exp where
  showsPrec n e = case e of
                    Abs h b -> showParen (n > 1) $ shows h . showString "." . showsPrec 1 b
                    App f x -> showParen (n > 2) $ showsPrec 2 f . showSpace . showsPrec 2 x
                    Var x -> showString x
  -- showsPrec _ (Var x)   = showString x
  -- showsPrec _ (Abs h b) = showString "λ" . shows h . showAbs b
  --  where
  --   showAbs (Abs h' b') = shows h' . showAbs b'
  --   showAbs e           = showString "." . showInner e
  --   showInner e@(Abs _ _) = showParen True $ shows e
  --   showInner (App f x)   = showInner f . showInner x
  --   showInner e           = shows e
  -- showsPrec _ (App f x) = shows f . showSpace . shows x

instance IsString Exp where
  fromString = Var

abs :: Exp -> Bool
abs (Abs {}) = True
abs _ = False

-- app :: Exp -> Bool
-- app (App {}) = True
-- app _ = False

λ :: Exp -> Exp -> Exp
λ = Abs

(∘) :: Exp -> Exp -> Exp
(∘) = App
