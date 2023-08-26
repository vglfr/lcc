{-# LANGUAGE OverloadedStrings #-}

module Lcc.Example where

import Lcc.AST (λ, (∘), Exp)
import Lcc.IR
  (
    Dat (Arg, Ref, Ret, Val)
  , IR (Main, Proc)
  , Ins (Cal, Loa)
  )

{- x -> x -}
a1 :: Exp
a1 = "x"

i1 :: [IR]
i1 =
  [
    Main
    [
      Loa (Val 'x')
    ]
  ]

{- λx.x y -> y -}
a2 :: Exp
a2 = λ "x" "x" ∘ "y"

i2 :: [IR]
i2 =
  [
    Proc 0
    [
      Loa (Arg 0)
    ]
  , Main
    [
      Loa (Val 'y')
    , Loa (Ref 0)
    , Cal 2
    , Loa Ret
    ]
  ]

{- λx.z y -> z -}
a3 :: Exp
a3 = λ "x" "z" ∘ "y"

i3 :: [IR]
i3 =
  [
    Proc 0
    [
      Loa (Val 'z')
    ]
  , Main
    [
      Loa (Val 'y')
    , Loa (Ref 0)
    , Cal 2
    , Loa Ret
    ]
  ]

{- λxy.x u v -> u -}
a4 :: Exp
a4 = λ "x" (λ "y" "x") ∘ "u" ∘ "v"

i4 :: [IR]
i4 =
  [
    Proc 0
    [
      Loa (Arg 0)
    ]
  , Main
    [
      Loa (Val 'u')
    , Loa (Val 'v')
    , Loa (Ref 0)
    , Cal 3
    , Loa Ret
    ]
  ]

{- λxy.y u v -> v -}
a5 :: Exp
a5 = λ "x" (λ "y" "y") ∘ "u" ∘ "v"

i5 :: [IR]
i5 =
  [
    Proc 0
    [
      Loa (Arg 1)
    ]
  , Main
    [
      Loa (Val 'u')
    , Loa (Val 'v')
    , Loa (Ref 0)
    , Cal 3
    , Loa Ret
    ]
  ]

{- λx.x λy.y z -> z -}
a6 :: Exp
a6 = λ "x" "x" ∘ λ "y" "y" ∘ "z"

i6 :: [IR]
i6 =
  [
    Proc 0
    [
      Loa (Arg 0)
    ]
  , Proc 1
    [
      Loa (Arg 0)
    ]
  , Main
    [
      Loa (Ref 1)
    , Loa (Ref 0)
    , Cal 2
    , Loa (Val 'z')
    , Loa Ret
    , Cal 2
    , Loa Ret
    ]
  ]

{- λxy.xy λy.y z -> z -}
a7 :: Exp
a7 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "y" "y" ∘ "z"

i7 :: [IR]
i7 =
  [
    Proc 0
    [
      Loa (Arg 1)
    , Loa (Arg 0)
    , Cal 2
    , Loa Ret
    ]
  , Proc 1
    [
      Loa (Arg 0)
    ]
  , Main
    [
      Loa (Val 'z')
    , Loa (Ref 1)
    , Loa (Ref 0)
    , Cal 3
    , Loa Ret
    ]
  ]

{- λxy.xy (λuv.u k) z -> k -}
a8 :: Exp
a8 = λ "x" (λ "y" ("x" ∘ "y")) ∘ (λ "u" (λ "v" "u") ∘ "k") ∘ "z" -- Show broken

i8 :: [IR]
i8 =
  [
  --   Proc 0
  --   [
  --     Loa (Arg 1)
  --   , Loa (Arg 0)
  --   , Cal 2
  --   , Loa Ret
  --   ]
  -- , Proc 1
  --   [
  --     Loa (Arg 0)
  --   ]
  -- , Main
  --   [
  --     Loa (Val 'z')
  --   , Loa (Ref 1)
  --   , Loa (Ref 0)
  --   , Cal 3
  --   , Loa Ret
  --   ]
  ]
