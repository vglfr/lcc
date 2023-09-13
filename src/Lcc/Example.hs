{-# LANGUAGE OverloadedStrings #-}

module Lcc.Example where

import Lcc.AST
  (
    λ, (∘)
  , Exp
  -- , Exp' (Abs', App', Bin', Unb')
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
-- e1' = Unb' 'x'

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
-- e2' = App' 0 0
--         (Abs' 1 1 1)
--         (Unb' 'y')

i2 :: Spool IR
i2 = Spool
  [
    Proc 1 -- x.x
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 1) (Con 'y')
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
-- e3' = App' 0 0
--         (Abs' 1 1 (Unb' 'z'))
--         (Unb' 'y')

i3 :: Spool IR
i3 = Spool
  [
    Proc 1 -- x.z
    [
      End (Con 'z')
    ]
  , Start
    [
      Cal (Ref 1) (Con 'y')
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
  x   λ       1   1
     / \         / \
    y   x       1   1
-}
e4 :: Exp
e4 = λ "x" (λ "y" "x") ∘ "u" ∘ "v"

-- e4' :: Exp'
-- e4' = App' 0 0
--         (App' 0 0
--           (Abs' 1 2 (Bin' 1 2) (Abs' 1 1 (Bin' 1 1) (Bin' 1 2)))
--           (Unb' 'u'))
--         (Unb' 'v')

i4 :: Spool IR
i4 = Spool
  [
    Proc 1 -- xy.x
    [
      Sav (Unb 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.x
    [
      End (Unb 1)
    ]
  , Start
    [
      Cal (Ref 1) (Con 'u')
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

-- e5' :: Exp'
-- e5' = App' 0 0
--         (App' 0 0
--           (Abs' 1 2 (Bin' 1 2) (Abs' 1 1 (Bin' 1 1) (Bin' 1 1)))
--           (Unb' 'u'))
--         (Unb' 'v')

i5 :: Spool IR
i5 = Spool
  [
    Proc 1 -- xy.y
    [
      Sav (Unb 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.y
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 1) (Con 'u')
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

-- e6' :: Exp'
-- e6' = App' 0 0
--         (App' 0 0
--           (Abs' 1 1 (Bin' 1 1) (Bin' 1 1))
--           (Abs' 2 1 (Bin' 2 1) (Bin' 2 1)))
--         (Unb' 'z')

i6 :: Spool IR
i6 = Spool
  [
    Proc 1 -- x.x
    [
      End Bin
    ]
  , Proc 2 -- y.y
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 1) (Ref 2)
    , Cal Ret (Con 'z')
    , End Ret
    ]
  ]

{- xy.xy y.y z -> z -}
e7 :: Exp
e7 = λ "x" (λ "y" ("x" ∘ "y")) ∘ λ "y" "y" ∘ "z"

-- e7' :: Exp'
-- e7' = App' 0 0
--         (App' 0 0
--           (Abs' 1 2 (Bin' 1 2)
--             (Abs' 1 1 (Bin' 1 1)
--               (App' 1 0
--                 (Bin' 1 2)
--                 (Bin' 1 1))))
--           (Abs' 2 1 (Bin' 2 1) (Bin' 2 1)))
--         (Unb' 'z')

i7 :: Spool IR
i7 = Spool
  [
    Proc 1 -- xy.xy
    [
      Sav (Unb 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.xy
    [
      Cal (Unb 1) Bin
    , End Ret
    ]
  , Proc 3 -- y.y
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 1) (Ref 3)
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

-- e8' :: Exp'
-- e8' = App' 0 0
--         (App' 0 0
--           (Abs' 1 2 (Bin' 1 2)
--             (Abs' 1 1 (Bin' 1 1)
--               (App' 1 0
--                 (Bin' 1 2)
--                 (Bin' 1 1))))
--           (App' 2 1
--             (Abs' 3 2 (Bin' 3 2)
--               (Abs' 3 1 (Bin' 3 1)
--                 (Bin' 3 2)))
--             (Unb' 'k')))
--         (Unb' 'z')

i8 :: Spool IR
i8 = Spool
  [
    Proc 1 -- xy.xy
    [
      Sav (Unb 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.xy
    [
      Cal (Unb 1) Bin
    , End Ret
    ]
  , Proc 3 -- uv.u k
    [
      Cal (Ref 4) (Con 'k')
    , Cal Ret Bin
    , End Ret
    ]
  , Proc 4 -- uv.u
    [
      Sav (Unb 4)
    , End (Ref 5)
    ]
  , Proc 5 -- v.u
    [
      End (Unb 4)
    ]
  , Start
    [
      Cal (Ref 1) (Ref 3)
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

-- e9' :: Exp'
-- e9' = App' 0 0
--         (App' 0 0
--           (App' 0 0
--             (Abs' 1 2 (Bin' 1 2)
--               (Abs' 1 1 (Bin' 1 1)
--                 (App' 1 0
--                   (Bin' 1 2)
--                   (Bin' 1 1))))
--             (Abs' 2 2 (Bin' 2 2)
--               (Abs' 2 1 (Bin' 2 1)
--                 (Bin' 2 2))))
--           (Unb' 'k'))
--         (Unb' 'z')

i9 :: Spool IR
i9 = Spool
  [
    Proc 1 -- xy.xy
    [
      Sav (Unb 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.xy
    [
      Cal (Unb 1) Bin
    , End Ret
    ]
  , Proc 3 -- uv.u
    [
      Sav (Unb 3)
    , End (Ref 4)
    ]
  , Proc 4 -- v.u
    [
      End (Unb 3)
    ]
  , Start
    [
      Cal (Ref 1) (Ref 3)
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

-- e10' :: Exp'
-- e10' = App' 0 0
--          (App' 0 0
--            (App' 0 0
--              (Abs' 1 2 (Bin' 1 2)
--                (Abs' 1 1 (Bin' 1 1)
--                  (App' 1 0
--                    (Bin' 1 2)
--                    (Abs' 2 1 (Bin' 2 1) (Bin' 2 1)))))
--              (Abs' 2 1 (Bin' 2 1) (Bin' 2 1)))
--            (Unb' 'k'))
--          (Unb' 'v')

i10 :: Spool IR
i10 = Spool
  [
    Proc 1 -- xy.x(z.z)
    [
      Sav (Unb 1)
    , End (Ref 2)
    ]
  , Proc 2 -- y.x(z.z)
    [
      Cal (Unb 1) (Ref 3)
    , End Ret
    ]
  , Proc 3 -- z.z == u.u
    [
      End Bin
    ]
  , Start
    [
      Cal (Ref 1) (Ref 3)
    , Cal Ret (Con 'k')
    , Cal Ret (Con 'v')
    , End Ret
    ]
  ]
