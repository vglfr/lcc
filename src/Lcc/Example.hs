{-# LANGUAGE OverloadedStrings #-}

module Lcc.Example where

import Lcc.AST (λ, (∘), Exp)
import Lcc.IR
  (
    Dat (Arg, Ref, Ret, Val)
  , IR (Start, Proc)
  , Ins (Cal, Loa)
  , Spool (Spool)
  )

{- x -> x -}
a1 :: Exp
a1 = "x"

i1 :: Spool IR
i1 = Spool
  [
    Start
    [
      Loa (Val 'x')
    ]
  ]

{- λx.x y -> y -}
a2 :: Exp
a2 = λ "x" "x" ∘ "y"

i2 :: Spool IR
i2 = Spool
  [
    Proc 0 -- λx.x
    [
      Loa (Arg 0)
    ]
  , Start
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

i3 :: Spool IR
i3 = Spool
  [
    Proc 0 -- λx.z
    [
      Loa (Val 'z')
    ]
  , Start
    [
      Loa (Val 'y')
    , Loa (Ref 0)
    , Cal 2
    , Loa Ret
    ]
  ]

{- λxy.x u v -> u -}
{-
        @           0
       / \         / \
      @   v       2   2
     / \         / \
    λ   u      (2)  2
   / \         / \
  λ   x       1   1
 / \         / \
y   x       1   1
-}
a4 :: Exp
a4 = λ "x" (λ "y" "x") ∘ "u" ∘ "v"

i4 :: Spool IR
i4 = Spool
  [
    Proc 0 -- λxy.x
    [
      Loa (Arg 0)
    ]
  , Start
    [
      Loa (Val 'v')
    , Loa (Val 'u')
    , Loa (Ref 0)
    , Cal 3
    , Loa Ret
    ]
  ]

{- λxy.y u v -> v -}
a5 :: Exp
a5 = λ "x" (λ "y" "y") ∘ "u" ∘ "v"

i5 :: Spool IR
i5 = Spool
  [
    Proc 0 -- λxy.y
    [
      Loa (Arg 1)
    ]
  , Start
    [
      Loa (Val 'v')
    , Loa (Val 'u')
    , Loa (Ref 0)
    , Cal 3
    , Loa Ret
    ]
  ]

{- λx.x λy.y z -> z -}
{-
       @             0
      / \           / \
     @   z         2   2
    / \           / \
  λ     λ      (2)   (2)
 / \   / \     / \   / \
x   x y   y   1   1 1   1
-}
a6 :: Exp
a6 = λ "x" "x" ∘ λ "y" "y" ∘ "z"

i6 :: Spool IR
i6 = Spool
  [
    Proc 0 -- λx.x
    [
      Loa (Arg 0)
    ]
  , Proc 1 -- λy.y
    [
      Loa (Arg 0)
    ]
  , Start
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

i7 :: Spool IR
i7 = Spool
  [
    Proc 0 -- λxy.xy
    [
      Loa (Arg 1)
    , Loa (Arg 0)
    , Cal 2
    , Loa Ret
    ]
  , Proc 1 -- λy.y
    [
      Loa (Arg 0)
    ]
  , Start
    [
      Loa (Val 'z')
    , Loa (Ref 1)
    , Loa (Ref 0)
    , Cal 3
    , Loa Ret
    ]
  ]

{- λxy.xy (λuv.u k) z -> k -}
{-
            @                    0
           / \                  / \
          @   z                2   2
         / \                  / \
      λ       @           (2)      2
     / \     / \          / \     / \
    λ   x   λ   k        1   1  (2)  2
   / \     / \          / \     / \
  @   y   λ   u        1   1   1   1
 / \     / \          / \     / \
x   y   v   u        2   2   1   1

  λxy.xy (λuv.u k) z
  λxy.xy (λ v.k  ) z
  λ y.(λv.k)y z
       λv.k z
          k

  λxy.xy λuv.u k z
  λ y.(λuv.u)y k z
  λuv.u k z
  λ v.k z
      k
-}
a8 :: Exp
a8 = λ "x" (λ "y" ("x" ∘ "y")) ∘ (λ "u" (λ "v" "u") ∘ "k") ∘ "z"

i8 :: Spool IR
i8 = Spool
  [
  --   Proc 0 -- λxy.xy
  --   [
  --     Loa (Arg 1)
  --   , Loa (Arg 0)
  --   , Cal 2
  --   , Loa Ret
  --   ]
  -- , Proc 1 -- λuv.u
  --   [
  --     Loa (Arg 0)
  --   ]
  -- , Start
  --   [
  --     Loa (Val 'z')
  --   , Loa (Ref 1)
  --   , Loa (Ref 0)
  --   , Cal 3
  --   , Loa Ret
  --   ]
  ]

{- λxy.xy λuv.u k z -> k -}
{-
            @                    0
           / \                  / \
          @   z                2   2
         / \                  / \
        @   k                2   2
       / \                  / \
    λ       λ           (2)     (2)
   / \     / \          / \     / \
  λ   u   λ   u        1   1   1   1
 / \     / \          / \     / \
v   u   v  u         1   1   1   1

  xy.xy (uv.u k) z
  xy.xy ( v.k  ) z
   y.(v.k)y z
      v.k z
        k

  xy.xy uv.u k z
   y.(uv.u)y k z
  uv.u k z
   v.k z
     k
-}
a9 :: Exp
a9 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "u" (λ "v" "u") ∘ "k" ∘ "z"

i9 :: Spool IR
i9 = Spool
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
  -- , Start
  --   [
  --     Loa (Val 'k')
  --   , Loa (Ref 1)
  --   , Loa (Ref 0)
  --   , Cal 3
  --   , Loa (Val 'z')
  --   , Loa Ret
  --   , Cal 2
  --   , Loa Ret
  --   ]
  ]

{- xy.x(z.z) u.u k v -> v -}
{-
  xy.x    (z.z) u.u k v
   y.(u.u)(z.z) k v
     (u.u)(z.z)   v
      z.z         v
        v
-}
a10 :: Exp
a10 = λ "x" (λ "y" ("x" ∘ λ "z" "z")) ∘ (λ "u"  "u") ∘ "k" ∘ "z"

i10 :: Spool IR
i10 = Spool
  [
    Proc 0 -- xy.x(z.z)
    [
      Loa (Ref 2)
    , Loa (Arg 0)
    , Cal 2
    , Loa Ret
    ]
  , Proc 1 -- u.u
    [
      Loa (Arg 0)
    ]
  , Proc 2 -- z.z
    [
      Loa (Arg 0)
    ]
  , Start
    [
      Loa (Val 'k')
    , Loa (Ref 1)
    , Loa (Ref 0)
    , Cal 3
    , Loa (Val 'v')
    , Loa Ret
    , Cal 2
    , Loa Ret
    ]
  ]
