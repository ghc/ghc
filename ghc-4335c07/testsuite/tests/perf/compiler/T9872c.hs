{-
 - Instant Insanity using Closed Type Families, but no DataKinds
 -
 - See:  http://stackoverflow.com/questions/26538595
 -}

{-# OPTIONS_GHC -freduction-depth=400 #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (all, flip, map, filter )

data Proxy a = Proxy

main = print (Proxy :: Proxy (Solutions Cubes))

data R -- Red
data G -- Green
data B -- Blue
data W -- White

data Cube u f r b l d

data True
data False

type family And b1 b2 :: * where
    And True True = True
    And b1 b2 = False

type family NE x y :: * where
    NE x x = False
    NE x y = True

type family EQ x y :: * where
    EQ a a = True
    EQ a b = False

data Nil = Nil
data Cons x xs = Cons x xs

type family All l :: * where
    All Nil = True
    All (Cons False xs) = False
    All (Cons True xs) = All xs

type family ListConcat xs ys :: * where
    ListConcat Nil ys = ys
    ListConcat (Cons x xs) ys = Cons x (ListConcat xs ys)

type family AppendIf b a as :: * where
    AppendIf False a as = as
    AppendIf True a as = Cons a as

data Rotate
data Twist
data Flip

type family Apply f a :: * where
    Apply Rotate (Cube u f r b l d) = (Cube u r b l f d)
    Apply Twist  (Cube u f r b l d) = (Cube f r u l d b)
    Apply Flip   (Cube u f r b l d) = (Cube d l b r f u)

type family Map f as :: * where
    Map f Nil = Nil
    Map f (Cons a as) = Cons (Apply f a) (Map f as)

type family MapAppend f as :: * where
    MapAppend f xs = ListConcat xs (Map f xs)

type family MapAppend2 f as :: * where
    MapAppend2 f xs = ListConcat xs (MapAppend f (Map f xs))

type family MapAppend3 f as :: * where
    MapAppend3 f xs = ListConcat xs (MapAppend2 f (Map f xs))

type family Iterate2 f as :: * where
    Iterate2 f Nil = Nil
    Iterate2 f (Cons a as) = ListConcat (Cons (Apply f a) (Cons a Nil)) (Iterate2 f as)

type family Iterate3 f as :: * where
    Iterate3 f (Cons a as) =
        ListConcat (Cons a
                    (Cons (Apply f a)
                     (Cons (Apply f (Apply f a))
                      Nil)))
                   (Iterate3 f as)

type family Iterate4 f as :: * where
    Iterate4 f Nil = Nil
    Iterate4 f (Cons a as) =
        ListConcat (Cons a
                    (Cons (Apply f a)
                     (Cons (Apply f (Apply f a))
                      (Cons (Apply f (Apply f (Apply f a)))
                       Nil))))
                   (Iterate4 f as)

type family Orientations c :: * where
    Orientations c = MapAppend3 Rotate (MapAppend2 Twist (MapAppend Flip (Cons c Nil)))

type Cube1 = Cube B G W G B R
type Cube2 = Cube W G B W R R
type Cube3 = Cube G W R B R R
type Cube4 = Cube B R G G W W

type Cubes = Cons Cube1 (Cons Cube2 (Cons Cube3 (Cons Cube4 Nil)))

type family Compatible c d :: * where
    Compatible (Cube u1 f1 r1 b1 l1 d1) (Cube u2 f2 r2 b2 l2 d2) =
        All (Cons (NE f1 f2) (Cons (NE r1 r2) (Cons (NE b1 b2) (Cons (NE l1 l2) Nil))))

type family Allowed c cs :: * where
    Allowed c Nil = True
    Allowed c (Cons s ss) = And (Compatible c s) (Allowed c ss)

type family MatchingOrientations as sol :: * where
    MatchingOrientations Nil sol = Nil
    MatchingOrientations (Cons o os) sol =
        AppendIf (Allowed o sol) (Cons o sol) (MatchingOrientations os sol)

type family AllowedCombinations os sols :: * where
    AllowedCombinations os Nil = Nil
    AllowedCombinations os (Cons sol sols) =
        ListConcat (MatchingOrientations os sol) (AllowedCombinations os sols)

type family Solutions c :: * where
    Solutions Nil = Cons Nil Nil
    Solutions (Cons c cs) = AllowedCombinations (Orientations c) (Solutions cs)
