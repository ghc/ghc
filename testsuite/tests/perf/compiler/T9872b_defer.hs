{-
 - Instant Insanity using Closed Type Families and DataKinds.
 -
 - See:  http://stackoverflow.com/questions/26538595
 -}

{-# OPTIONS_GHC -freduction-depth=400 #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

import Prelude hiding (all, flip, map, filter )

data Proxy (a :: k) = Proxy

main = print (Proxy :: Proxy (Solutions Cubes))

data Color = R | G | B | W

data Cube = Cube Color Color Color Color Color Color

type family And (b1 :: Bool) (b2 :: Bool) :: Bool where
    And True True = True
    And b1 b2 = False

type family NE (x :: Color) (y :: Color) :: Bool where
    NE x x = False
    NE x y = True

type family EQ (x :: Color) (y :: Color) :: Bool where
    EQ a a = True
    EQ a b = False

type family All (l :: [Bool]) :: Bool where
    All '[] = True
    All (False ': xs) = False
    All (True ': xs) = All xs

type family ListConcat (xs :: [k]) (ys :: [k]) :: [k] where
    ListConcat '[] ys = ys
    ListConcat (x ': xs) ys = x ': ListConcat xs ys

type family AppendIf (b :: Bool) (a :: [Cube]) (as :: [[Cube]]) :: [[Cube]] where
    AppendIf False a as = as
    AppendIf True a as = a ': as

data Transform = Rotate | Twist | Flip

type family Apply (f :: Transform) (a :: Cube) :: Cube where
    Apply Rotate ('Cube u f r b l d) = ('Cube u r b l f d)
    Apply Twist  ('Cube u f r b l d) = ('Cube f r u l d b)
    Apply Flip   ('Cube u f r b l d) = ('Cube d l b r f u)

type family Map (f :: Transform) (as :: [Cube]) :: [Cube] where
    Map f '[] = '[]
    Map f (a ': as) = (Apply f a) ': (Map f as)

type family MapAppend (f :: Transform) (as :: [Cube]) :: [Cube] where
    MapAppend f xs = ListConcat xs (Map f xs)

type family MapAppend2 (f :: Transform) (as :: [Cube]) :: [Cube] where
    MapAppend2 f xs = ListConcat xs (MapAppend f (Map f xs))

type family MapAppend3 (f :: Transform) (as :: [Cube]) :: [Cube] where
    MapAppend3 f xs = ListConcat xs (MapAppend2 f (Map f xs))

type family Iterate2 (f :: Transform) (as :: [Cube]) :: [Cube] where
    Iterate2 f '[] = '[]
    Iterate2 f (a ': as) = ListConcat [Apply f a, a] (Iterate2 f as)

type family Iterate3 (f :: Transform) (as :: [Cube]) :: [Cube] where
    Iterate3 f '[] = '[]
    Iterate3 f (a ': as) =
        ListConcat [a, Apply f a, Apply f (Apply f a)] (Iterate3 f as)

type family Iterate4 (f :: Transform) (as :: [Cube]) :: [Cube] where
    Iterate4 f '[] = '[]
    Iterate4 f (a ': as) =
        ListConcat [a, Apply f a, Apply f (Apply f a), Apply f (Apply f (Apply f a))]
                   (Iterate4 f as)

type family Orientations (c :: Cube) :: [Cube] where
    Orientations c = MapAppend3 Rotate (MapAppend2 Twist (MapAppend Flip '[c]))

type Cube1 = 'Cube B G W G B R
type Cube2 = 'Cube W G B W R R
type Cube3 = 'Cube G W R B R R
type Cube4 = 'Cube B R G G W W

type Cubes = [Cube1, Cube2, Cube3, Cube4]

type family Compatible (c :: Cube) (d :: Cube) :: Bool where
    Compatible ('Cube u1 f1 r1 b1 l1 d1) ('Cube u2 f2 r2 b2 l2 d2) =
        All [NE f1 f2, NE r1 r2, NE b1 b2, NE l1 l2]

type family Allowed (c :: Cube) (cs :: [Cube]) :: Bool where
    Allowed c '[] = True
    Allowed c (s ': ss) = And (Compatible c s) (Allowed c ss)

type family MatchingOrientations (as :: [Cube]) (sol :: [Cube]) :: [[Cube]] where
    MatchingOrientations '[] sol = '[]
    MatchingOrientations (o ': os) sol =
        AppendIf (Allowed o sol) (o ': sol) (MatchingOrientations os sol)

type family AllowedCombinations (os :: [Cube]) (sols :: [[Cube]]) where
    AllowedCombinations os '[] = '[]
    AllowedCombinations os (sol ': sols) =
        ListConcat (MatchingOrientations os sol) (AllowedCombinations os sols)

type family Solutions (cs :: [Cube]) :: [[Cube]] where
    Solutions '[] = '[ '[] ]
    Solutions (c ': cs) = AllowedCombinations (Orientations c) (Solutions cs)
