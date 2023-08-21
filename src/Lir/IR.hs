module Lir.IR where

data Ins
  = Cal Dat Dat

data Dat
  = Pro Int
  | Val Char
