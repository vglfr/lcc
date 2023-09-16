{-# LANGUAGE OverloadedStrings #-}

module Lcc.Example where

import Lcc.AST
  (
    λ, (∘)
  , Exp
  , Exp' (Abs', App', Bin', Unb')
  )
import Lcc.IR
  (
    Dat (Arg, Bin, Ref, Ret, Unb)
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

e1' :: Exp'
e1' = Unb' 'x'

i1 :: Spool IR
i1 = Spool
  [
    Start
    [
      End (Unb 'x')
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

e2' :: Exp'
e2' = App' 0 0
        (Abs' 1 (Bin' 1))
        (Unb' 'y')

i2 :: Spool IR
i2 = Spool
  [
    Start
    [
      Cal (Ref 1) (Unb 'y')
    , End Ret
    ]
  , Proc 1 -- x.x
    [
      End Arg
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

e3' :: Exp'
e3' = App' 0 0
        (Abs' 1 (Unb' 'z'))
        (Unb' 'y')

i3 :: Spool IR
i3 = Spool
  [
    Start
    [
      Cal (Ref 1) (Unb 'y')
    , End Ret
    ]
  , Proc 1 -- x.z
    [
      End (Unb 'z')
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
x   λ       1   1
   / \         / \
  y   x       1   1
-}
e4 :: Exp
e4 = λ "x" (λ "y" "x") ∘ "u" ∘ "v"

e4' :: Exp'
e4' = App' 0 0
        (App' 0 0
          (Abs' 1 (Abs' 2 (Bin' 1)))
          (Unb' 'u'))
        (Unb' 'v')

i4 :: Spool IR
i4 = Spool
  [
    Start
    [
      Cal (Ref 1) (Unb 'u')
    , Cal Ret (Unb 'v')
    , End Ret
    ]
  , Proc 1 -- xy.x
    [
      Sav (Bin 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.x
    [
      End (Bin 1)
    ]
  ]

{- xy.y u v -> v -}
{-
      @        @       '2
     / \      / \      / \
    @   v    @   v   '1   v
   / \      / \      / \
  λ   u    x.  u    1'  u
 / \       |        |
x   λ      y.       2'
   / \     |
  y   y    y
-}
e5 :: Exp
e5 = λ "x" (λ "y" "y") ∘ "u" ∘ "v"

e5' :: Exp'
e5' = App' 0 0
        (App' 0 0
          (Abs' 1 (Abs' 2 (Bin' 2)))
          (Unb' 'u'))
        (Unb' 'v')

i5 :: Spool IR
i5 = Spool
  [
    Start
    [
      Cal (Ref 1) (Unb 'u')
    , Cal Ret (Unb 'v')
    , End Ret
    ]
  , Proc 1 -- xy.y
    [
      Sav (Bin 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.y
    [
      End Arg
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

e6' :: Exp'
e6' = App' 0 0
        (App' 0 0
          (Abs' 1 (Bin' 1))
          (Abs' 2 (Bin' 2)))
        (Unb' 'z')

i6 :: Spool IR
i6 = Spool
  [
    Start
    [
      Cal (Ref 1) (Ref 2)
    , Cal Ret (Unb 'z')
    , End Ret
    ]
  , Proc 1 -- x.x
    [
      End Arg
    ]
  , Proc 2 -- y.y
    [
      End Arg
    ]
  ]

{- xy.xy y.y z -> z -}
e7 :: Exp
e7 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "y" "y" ∘ "z"

e7' :: Exp'
e7' = App' 0 0
        (App' 0 0
          (Abs' 1
            (Abs' 2
              (App' 1 0
                (Bin' 1)
                (Bin' 2))))
          (Abs' 3 (Bin' 3)))
        (Unb' 'z')

i7 :: Spool IR
i7 = Spool
  [
    Start
    [
      Cal (Ref 1) (Ref 3)
    , Cal Ret (Unb 'z')
    , End Ret
    ]
  , Proc 1 -- xy.xy
    [
      Sav (Bin 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.xy
    [
      Cal (Bin 1) Arg
    , End Ret
    ]
  , Proc 3 -- y.y
    [
      End Arg
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

e8' :: Exp'
e8' = App' 0 0
        (App' 0 0
          (Abs' 1
            (Abs' 2
              (App' 1 0
                (Bin' 1)
                (Bin' 2))))
          (App' 3 1
            (Abs' 4
              (Abs' 5
                (Bin' 4)))
            (Unb' 'k')))
        (Unb' 'z')

i8 :: Spool IR
i8 = Spool
  [
    Start
    [
      Cal (Ref 1) (Ref 3)
    , Cal Ret (Unb 'z')
    , End Ret
    ]
  , Proc 1 -- xy.xy
    [
      Sav (Bin 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.xy
    [
      Cal (Bin 1) Arg
    , End Ret
    ]
  , Proc 3 -- uv.u k
    [
      Cal (Ref 4) (Unb 'k')
    , Cal Ret Arg
    , End Ret
    ]
  , Proc 4 -- uv.u
    [
      Sav (Bin 4)
    , End (Ref 5)
    ]
  , Proc 5 -- v.u
    [
      End (Bin 4)
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
  λ   x   λ   u        1   1   1   1
 / \     / \          / \     / \
y   y   v   u        1   1   1   1

  xy.x     y uv.u k z
   y.(uv.u)y      k z
      uv.u k        z
       v.  k        z
           k
-}
e9 :: Exp
e9 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "u" (λ "v" "u") ∘ "k" ∘ "z"

e9' :: Exp'
e9' = App' 0 0
        (App' 0 0
          (App' 0 0
            (Abs' 1
              (Abs' 2
                (App' 1 0
                  (Bin' 1)
                  (Bin' 2))))
            (Abs' 3
              (Abs' 4
                (Bin' 3))))
          (Unb' 'k'))
        (Unb' 'z')

i9 :: Spool IR
i9 = Spool
  [
    Start
    [
      Cal (Ref 1) (Ref 3)
    , Cal Ret (Unb 'k')
    , Cal Ret (Unb 'z')
    , End Ret
    ]
  , Proc 1 -- xy.xy
    [
      Sav (Bin 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.xy
    [
      Cal (Bin 1) Arg
    , End Ret
    ]
  , Proc 3 -- uv.u
    [
      Sav (Bin 3)
    , End (Ref 4)
    ]
  , Proc 4 -- v.u
    [
      End (Bin 3)
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
e10 = λ "x" (λ "y" ("x" ∘ λ "z" "z")) ∘ (λ "u"  "u") ∘ "k" ∘ "v"

e10' :: Exp'
e10' = App' 0 0
         (App' 0 0
           (App' 0 0
             (Abs' 1
               (Abs' 2
                 (App' 1 0
                   (Bin' 1)
                   (Abs' 3 (Bin' 3)))))
             (Abs' 4 (Bin' 4)))
           (Unb' 'k'))
         (Unb' 'v')

i10 :: Spool IR
i10 = Spool
  [
    Start
    [
      Cal (Ref 1) (Ref 4)
    , Cal Ret (Unb 'k')
    , Cal Ret (Unb 'v')
    , End Ret
    ]
  , Proc 1 -- xy.x(z.z)
    [
      Sav (Bin 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.x(z.z)
    [
      Cal (Bin 1) (Ref 3)
    , End Ret
    ]
  , Proc 3 -- z.z
    [
      End Arg
    ]
  , Proc 4 -- u.u
    [
      End Arg
    ]
  ]
