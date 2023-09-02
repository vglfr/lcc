{-# LANGUAGE OverloadedStrings #-}

module Lcc.Example where

import Lcc.AST
  (
    λ, (∘)
  , Exp
  -- , Exp' (Abs', App', Var')
  )
import Lcc.IR
  (
    Dat (Bin, Con, Ref, Ret, Unb)
  , IR (Start, Proc)
  , Ins (Cal, End, Sav)
  , Spool (Spool)
  )

{- x -> x -}
{-
  x
-}
e1 :: Exp
e1 = "x"

-- e1' :: Exp'
-- e1' = Var' 'x'

i1 :: Spool IR
i1 = Spool
  [
    Start
    [
      End (Con 'x')
    ]
  ]

{- x.x y -> y -}
{-
    @
   / \
  λ   y
 / \
x   x
-}
e2 :: Exp
e2 = λ "x" "x" ∘ "y"

-- e2' :: Exp'
-- e2' = App' (Abs' 0 [Var' 'x'] $ Var' 'x') [Var' 'y']

i2 :: Spool IR
i2 = Spool
  [
    Proc 0 1 -- x.x
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 0 1) (Con 'y')
    , End Ret
    ]
  ]

{- x.z y -> z -}
{-
    @
   / \
  λ   y
 / \
x   z
-}
e3 :: Exp
e3 = λ "x" "z" ∘ "y"

-- e3' :: Exp'
-- e3' = App' (Abs' 0 [Var' 'x'] $ Var' 'z') [Var' 'y']

i3 :: Spool IR
i3 = Spool
  [
    Proc 0 1 -- x.z
    [
      End (Con 'z')
    ]
  , Start
    [
      Cal (Ref 0 1) (Con 'y')
    , End Ret
    ]
  ]

{- xy.x u v -> u -}
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
e4 :: Exp
e4 = λ "x" (λ "y" "x") ∘ "u" ∘ "v"

i4 :: Spool IR
i4 = Spool
  [
    Proc 0 2 -- xy.x
    [
      Sav (Unb 0 0)
    , End (Ref 0 1)
    ]
  , Proc 0 1 -- y.x
    [
      End (Unb 0 0)
    ]
  , Start
    [
      Cal (Ref 0 2) (Con 'u')
    , Cal Ret (Con 'v')
    , End Ret
    ]
  ]

{- xy.y u v -> v -}
{-
        @
       / \
      @   v
     / \
    λ   u
   / \
  λ   x
 / \
y   y
-}
e5 :: Exp
e5 = λ "x" (λ "y" "y") ∘ "u" ∘ "v"

i5 :: Spool IR
i5 = Spool
  [
    Proc 0 2 -- xy.y
    [
      Sav (Unb 0 0)
    , End (Ref 0 1)
    ]
  , Proc 0 1 -- y.y
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 0 2) (Con 'u')
    , Cal Ret (Con 'v')
    , End Ret
    ]
  ]

{- x.x y.y z -> z -}
{-
       @             0
      / \           / \
     @   z         2   2
    / \           / \
  λ     λ      (2)   (2)
 / \   / \     / \   / \
x   x y   y   1   1 1   1
-}
e6 :: Exp
e6 = λ "x" "x" ∘ λ "y" "y" ∘ "z"

i6 :: Spool IR
i6 = Spool
  [
    Proc 0 1 -- x.x
    [
      End Bin
    ]
  , Proc 1 1 -- y.y
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 0 1) (Ref 1 1)
    , Cal Ret (Con 'z')
    , End Ret
    ]
  ]

{- xy.xy y.y z -> z -}
e7 :: Exp
e7 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "y" "y" ∘ "z"

i7 :: Spool IR
i7 = Spool
  [
    Proc 0 2 -- xy.xy
    [
      Sav (Unb 0 0)
    , End (Ref 0 1)
    ]
  , Proc 0 1 -- y.xy
    [
      Cal (Unb 0 0) Bin
    , End Ret
    ]
  , Proc 1 1 -- y.y
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 0 2) (Ref 1 1)
    , Cal Ret (Con 'z')
    , End Ret
    ]
  ]

{- xy.xy (uv.u k) z -> k -}
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

  xy.xy (uv.u k) z
  xy.xy ( v.k  ) z
   y.(v.k)y      z
      v.k z
        k
-}
e8 :: Exp
e8 = λ "x" (λ "y" ("x" ∘ "y")) ∘ (λ "u" (λ "v" "u") ∘ "k") ∘ "z"

i8 :: Spool IR
i8 = Spool
  [
    Proc 0 2 -- xy.xy
    [
      Sav (Unb 0 0)
    , End (Ref 0 1)
    ]
  , Proc 0 1 -- y.xy
    [
      Cal (Unb 0 0) Bin
    , End Ret
    ]
  , Proc 1 1 -- uv.u k
    [
      Cal (Ref 2 2) (Con 'k')
    , Cal Ret Bin
    , End Ret
    ]
  , Proc 2 2 -- uv.u
    [
      Sav (Unb 2 0)
    , End (Ref 2 1)
    ]
  , Proc 2 1 -- v.u
    [
      End (Unb 2 0)
    ]
  , Start
    [
      Cal (Ref 0 2) (Ref 1 1)
    , Cal Ret (Con 'z')
    , End Ret
    ]
  ]

{- xy.xy uv.u k z -> k -}
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

  xy.x     y uv.u k z
   y.(uv.u)y      k z
      uv.u k        z
       v.  k        z
           k
-}
e9 :: Exp
e9 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "u" (λ "v" "u") ∘ "k" ∘ "z"

i9 :: Spool IR
i9 = Spool
  [
    Proc 0 2 -- xy.xy
    [
      Sav (Unb 0 0)
    , End (Ref 0 1)
    ]
  , Proc 0 1 -- y.xy
    [
      Cal (Unb 0 0) Bin
    , End Ret
    ]
  , Proc 1 2 -- uv.u
    [
      Sav (Unb 1 0)
    , End (Ref 1 1)
    ]
  , Proc 1 1 -- v.u
    [
      End (Unb 1 0)
    ]
  , Start
    [
      Cal (Ref 0 2) (Ref 1 2)
    , Cal Ret (Con 'k')
    , Cal Ret (Con 'z')
    , End Ret
    ]
  ]

{- xy.x(z.z) u.u k v -> v -}
{-
  xy.x    (z.z) u.u k v
   y.(u.u)(z.z)     k v
     (u.u)(z.z)       v
      z.z             v
        v
-}
e10 :: Exp
e10 = λ "x" (λ "y" ("x" ∘ λ "z" "z")) ∘ (λ "u"  "u") ∘ "k" ∘ "z"

i10 :: Spool IR
i10 = Spool
  [
    Proc 0 2 -- xy.x(z.z)
    [
      Sav (Unb 0 0)
    , End (Ref 0 1)
    ]
  , Proc 0 1 -- y.x(z.z)
    [
      Cal (Unb 0 0) (Ref 1 1)
    , End Ret
    ]
  , Proc 1 1 -- z.z == u.u
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 0 2) (Ref 1 1)
    , Cal Ret (Con 'k')
    , Cal Ret (Con 'v')
    , End Ret
    ]
  ]
