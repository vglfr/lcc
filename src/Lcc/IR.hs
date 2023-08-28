module Lcc.IR where

import Data.List (intercalate)

data Spool a = Spool Name [a]

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
type Name = String

instance Show a => Show (Spool a) where
  show (Spool n as) = intercalate "\n\n" $ "; " <> n : fmap show as
