module Lcc.Typecheck where

{-
All values are single characters

Program always returns Val
Program always terminates

Value can't be left:
  Val @ _ - _|_

Applications are flattened:
  App @ _ - _|_
  _ @ App - _|_

Eager is simple, lazy is powerful
Labeled functions are simple, inlined functions are powerful
Total functions are simple, partial functions are powerful

Let's start simple

Remains:
  Abs @ Val
  Abs @ Abs

Abstraction returns either Abs or Val
-}