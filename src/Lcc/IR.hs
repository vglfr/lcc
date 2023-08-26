module Lcc.IR where

data IR
  = Main [Ins]
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
