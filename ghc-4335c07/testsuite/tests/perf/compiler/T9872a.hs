{-
 - Instant Insanity using Type Families.
 -
 - See:  The Monad Read, Issue #8 - http://www.haskell.org/wikiupload/d/dd/TMR-Issue8.pdf
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

type family And b1 b2 :: *
type instance And True True   = True
type instance And True False  = False
type instance And False False = False
type instance And False True  = False

type family NE x y :: *
type instance NE R R = False
type instance NE R G = True
type instance NE R B = True
type instance NE R W = True
type instance NE G R = True
type instance NE G G = False
type instance NE G B = True
type instance NE G W = True
type instance NE B R = True
type instance NE B G = True
type instance NE B B = False
type instance NE B W = True
type instance NE W R = True
type instance NE W G = True
type instance NE W B = True
type instance NE W W = False

type family EQ x y :: *

type instance EQ R R = True
type instance EQ R G = False
type instance EQ R B = False
type instance EQ R W = False
type instance EQ G R = False
type instance EQ G G = True
type instance EQ G B = False
type instance EQ G W = False
type instance EQ B R = False
type instance EQ B G = False
type instance EQ B B = True
type instance EQ B W = False
type instance EQ W R = False
type instance EQ W G = False
type instance EQ W B = False
type instance EQ W W = True

data Nil = Nil
data Cons x xs = Cons x xs

type family All l :: *
type instance All Nil = True
type instance All (Cons False xs) = False
type instance All (Cons True xs) = All xs

type family ListConcat xs ys :: *
type instance ListConcat Nil ys = ys
type instance ListConcat (Cons x xs) ys = Cons x (ListConcat xs ys)

type family AppendIf b a as :: *
type instance AppendIf False a as = as
type instance AppendIf True a as = Cons a as

data Rotate
data Twist
data Flip

type family Apply f a :: *
type instance Apply Rotate (Cube u f r b l d) = (Cube u r b l f d)
type instance Apply Twist  (Cube u f r b l d) = (Cube f r u l d b)
type instance Apply Flip   (Cube u f r b l d) = (Cube d l b r f u)

-- orientations c = [ z | x <- [ c, flip c ], y <- [ x, twist x, twist (twist x) ], z <- [y, rot y, rot(rot y), rot(rot(rot(y))) ] ]

type family Map f as :: *
type instance Map f Nil = Nil
type instance Map f (Cons a as) = Cons (Apply f a) (Map f as)

type family MapAppend f as :: *
type instance MapAppend f xs = ListConcat xs (Map f xs)

type family MapAppend2 f as :: *
type instance MapAppend2 f xs = ListConcat xs (MapAppend f (Map f xs))

type family MapAppend3 f as :: *
type instance MapAppend3 f xs = ListConcat xs (MapAppend2 f (Map f xs))

type family Iterate2 f as :: *
type instance Iterate2 f Nil = Nil
type instance Iterate2 f (Cons a as) = ListConcat (Cons (Apply f a) (Cons a Nil)) (Iterate2 f as)

type family Iterate3 f as :: *
type instance Iterate3 f (Cons a as) =
  ListConcat (Cons a
             (Cons (Apply f a)
             (Cons (Apply f (Apply f a))
              Nil)))
             (Iterate3 f as)

type family Iterate4 f as :: *
type instance Iterate4 f Nil = Nil
type instance Iterate4 f (Cons a as) =
   ListConcat (Cons a
              (Cons (Apply f a)
              (Cons (Apply f (Apply f a))
              (Cons (Apply f (Apply f (Apply f a)))
               Nil))))
              (Iterate4 f as)

type family Orientations c :: *
-- type instance Orientations c = Iterate4 Rotate (Iterate3 Twist (Iterate2 Flip (Cons c Nil)))
type instance Orientations c = MapAppend3 Rotate (MapAppend2 Twist (MapAppend Flip (Cons c Nil)))

type Cube1 = Cube B G W G B R
type Cube2 = Cube W G B W R R
type Cube3 = Cube G W R B R R
type Cube4 = Cube B R G G W W

type Cubes = Cons Cube1 (Cons Cube2 (Cons Cube3 (Cons Cube4 Nil)))

type family Compatible c d :: *
type instance Compatible (Cube u1 f1 r1 b1 l1 d1) (Cube u2 f2 r2 b2 l2 d2) =
  All (Cons (NE f1 f2) (Cons (NE r1 r2) (Cons (NE b1 b2) (Cons (NE l1 l2) Nil))))

type family Allowed c cs :: *
type instance Allowed c Nil = True
type instance Allowed c (Cons s ss) = And (Compatible c s) (Allowed c ss)

type family MatchingOrientations as sol :: *
type instance MatchingOrientations Nil sol = Nil
type instance MatchingOrientations (Cons o os) sol = AppendIf (Allowed o sol) (Cons o sol) (MatchingOrientations os sol)

type family AllowedCombinations os sols :: *
type instance AllowedCombinations os Nil = Nil
type instance AllowedCombinations os (Cons sol sols) = ListConcat (MatchingOrientations os sol) (AllowedCombinations os sols)

type family Solutions c :: *
type instance Solutions Nil = Cons Nil Nil
type instance Solutions (Cons c cs) = AllowedCombinations (Orientations c) (Solutions cs)

{-
 - solutions [] = [ [] ]
 - solutions (c:cs) = [ (o:sol) | sol <- solutions cs, o <- orientations c, allowed o 
-}
