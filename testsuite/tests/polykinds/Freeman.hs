-- From the blog post Fun With XPolyKinds : Polykinded Folds
--   http://www.typesandotherdistractions.com/2012/02/fun-with-xpolykinds-polykinded-folds.html

{-
In the following, I will write a polykinded version of the combinators
fold and unfold, along with three examples: folds for regular
datatypes (specialized to kind *), folds for nested datatypes
(specialized to kind * -> *), and folds for mutually recursive data
types (specialized to the product kind (*,*)). The approach should
generalise easily enough to things such as types indexed by another
kind (e.g. by specializing to kind Nat -> *, using the XDataKinds
extension), or higher order nested datatypes (e.g. by specializing to
kind (* -> *) -> (* -> *)).

The following will compile in the new GHC 7.4.1 release. We require
the following GHC extensions:
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

{- The basic fold and unfold combinators can be written as follows:

fold phi = phi . fmap (fold phi) . out
unfold psi = in . fmap (unfold psi) . psi

The idea now is to generalize these combinators by working over
different categories. We can capture the basic operations in a
category with a typeclass: -}

class Category hom where
  ident :: hom a a
  compose :: hom a b -> hom b c -> hom a c

{- A category has two operations: an identity morphism for every
object, and for every two compatible morphisms, the composition of
those morphisms.

In earlier versions of GHC, the type hom would have been specialized
to kind * -> * -> *, but with the new PolyKinds extension, hom is
polykinded, and the Category typeclass can be instantiated to k -> k
-> * for any kind k. This means that in addition to all of the
Category instances that we could have written before, we can now write
instances of Category for type constructors, type constructor
constructors, etc.

Here is the instance for the category Hask of Haskell types. Objects
are Haskell types and morphisms are functions between types. The
identity is the regular polymorphic identity function id, and
composition is given by the (flipped) composition operator (.) -}

instance Category (->) where
  ident = id
  compose = flip (.)

{- Another example is the category of type constructors and natural
transformations. A natural transformation is defined as follows: -}

newtype Nat f g = Nat { nu :: (forall a. f a -> g a) } 

{- Here is the Category instance for natural transformations. This
time the type hom is inferred to have kind (* -> *) -> (* -> *) ->
*. Identity and composition are both defined pointwise. -}

instance Category (Nat :: (* -> *) -> (* -> *) -> *) where
  ident = Nat id
  compose f g = Nat (nu g . nu f)

{- Let's define a type class which will capture the idea of a fixed point
in a category. This generalizes the idea of recursive types in Hask: -}

class Rec hom f t where
  _in :: hom (f t) t
  out :: hom t (f t)

{- The class Rec defines two morphisms: _in, which is the constructor of
the fixed point type t, and out, its destructor.

The final piece is the definition of a higher order functor, which
generalizes the typeclass Functor: -}

class HFunctor hom f where
  hmap :: hom a b -> hom (f a) (f b)

{- Note the similarity with the type signature of the function fmap ::
(Functor f) => (a -> b) -> f a -> f b. Indeed, specializing hom to
(->) in the definition of HFunctor gives back the type signature of
fmap. 

Finally, we can define folds and unfolds in a category. The
definitions are as before, but with explicit composition, constructors
and destructors replaced with the equivalent type class methods, and
hmap in place of fmap: -}

fold :: (Category hom, HFunctor hom f, Rec hom f rec) => hom (f t) t -> hom rec t
fold phi = compose out (compose (hmap (fold phi)) phi)

unfold :: (Category hom, HFunctor hom f, Rec hom f rec) => hom t (f t) -> hom t rec
unfold phi = compose phi (compose (hmap (unfold phi)) _in)

-- Now for some examples.

-- The first example is a regular recursive datatype of binary leaf
-- trees. The functor FTree is the base functor of this recursive type:

data FTree a b = FLeaf a | FBranch b b
data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- An instance of Rec shows the relationship between the defining functor
-- and the recursive type itself:

instance Rec (->) (FTree a) (Tree a) where
  _in (FLeaf a) = Leaf a
  _in (FBranch a b) = Branch a b
  out (Leaf a) = FLeaf a
  out (Branch a b) = FBranch a b

-- FTree is indeed a functor, so it is also a HFunctor:

instance HFunctor (->) (FTree a) where
  hmap f (FLeaf a) = FLeaf a
  hmap f (FBranch a b) = FBranch (f a) (f b)

-- These instances are enough to define folds and unfolds for this
-- type. The following fold calculates the depth of a tree:

depth :: Tree a -> Int
depth = (fold :: (FTree a Int -> Int) -> Tree a -> Int) phi where
  phi :: FTree a Int -> Int
  phi (FLeaf a) = 1
  phi (FBranch a b) = 1 + max a b

-- The second example is a fold for the nested (or non-regular)
-- datatype of complete binary leaf trees. The higher order functor
-- FCTree defines the type constructor CTree as its fixed point:

data FCTree f a = FCLeaf a | FCBranch (f (a, a))
  -- FCTree :: (* -> *) -> * -> *

data CTree a = CLeaf a | CBranch (CTree (a, a))

-- Again, we define type class instances for HFunctor and Rec:

instance HFunctor Nat FCTree where
  hmap (f :: Nat (f :: * -> *) (g :: * -> *)) = Nat ff where
    ff :: forall a. FCTree f a -> FCTree g a
    ff (FCLeaf a) = FCLeaf a
    ff (FCBranch a) = FCBranch (nu f a)

instance Rec Nat FCTree CTree where
  _in = Nat inComplete where
    inComplete (FCLeaf a) = CLeaf a
    inComplete (FCBranch a) = CBranch a
  out = Nat outComplete where
    outComplete(CLeaf a) = FCLeaf a
    outComplete(CBranch a) = FCBranch a

-- Morphisms between type constructors are natural transformations, so we
-- need a type constructor to act as the target of the fold. For our
-- purposes, a constant functor will do:

data K a b = K a  -- K :: forall k. * -> k -> *


-- And finally, the following fold calculates the depth of a complete binary leaf tree:

cdepth :: CTree a -> Int
cdepth c = let (K d) = nu (fold (Nat phi)) c in d where
  phi :: FCTree (K Int) a -> K Int a
  phi (FCLeaf a) = K 1
  phi (FCBranch (K n)) = K (n + 1)

{- The final example is a fold for the pair of mutually recursive
datatype of lists of even and odd lengths. The fold will take a list
of even length and produce a list of pairs.

We cannot express type constructors in Haskell whose return kind is
anything other than *, so we cheat a little and emulate the product
kind using an arrow kind Choice -> *, where Choice is a two point
kind, lifted using the XDataKinds extension: -}

data Choice = Fst | Snd

-- A morphism of pairs of types is just a pair of morphisms. For
-- technical reasons, we represent this using a Church-style encoding,
-- along with helper methods, as follows:

newtype PHom h1 h2 p1 p2 = PHom { runPHom :: forall r. (h1 (p1 Fst) (p2 Fst) -> h2 (p1 Snd) (p2 Snd) -> r) -> r }

mkPHom f g = PHom (\h -> h f g)
fstPHom p = runPHom p (\f -> \g -> f)
sndPHom p = runPHom p (\f -> \g -> g)

-- Now, PHom allows us to take two categories and form the product category:

instance (Category h1, Category h2) => Category (PHom h1 h2) where
  ident = mkPHom ident ident
  compose p1 p2 = mkPHom (compose (fstPHom p1) (fstPHom p2)) (compose (sndPHom p1) (sndPHom p2))

-- We can define the types of lists of even and odd length as
-- follows. Note that the kind annotation indicates the appearance of the
-- kind Choice -> *:

data FAlt :: * -> (Choice -> *) -> Choice -> * where
  FZero :: FAlt a p Fst
  FSucc1 :: a -> (p Snd) -> FAlt a p Fst
  FSucc2 :: a -> (p Fst) -> FAlt a p Snd

data Alt :: * -> Choice -> * where
  Zero :: Alt a Fst
  Succ1 :: a -> Alt a Snd -> Alt a Fst
  Succ2 :: a -> Alt a Fst -> Alt a Snd

deriving instance Show a => Show (Alt a b)

-- Again, we need to define instances of Rec and HFunctor:

instance Rec (PHom (->) (->)) (FAlt a) (Alt a) where
  _in = mkPHom f g where
    f,g :: FAlt a (Alt a) s -> Alt a s
    f FZero = Zero
    f (FSucc1 a b) = Succ1 a b
    g (FSucc2 a b) = Succ2 a b

  out = mkPHom f g where
    f,g :: Alt a s -> FAlt a (Alt a) s
    f Zero = FZero
    f (Succ1 a b) = FSucc1 a b
    g (Succ2 a b) = FSucc2 a b

instance HFunctor (PHom (->) (->)) (FAlt a) where
  hmap p = mkPHom hf hg where
    hf FZero = FZero
    hf (FSucc1 a x) = FSucc1 a (sndPHom p x)
    hg (FSucc2 a x) = FSucc2 a (fstPHom p x)

-- As before, we create a target type for our fold, and this time a type synonym as well:

data K2 :: * -> * -> Choice -> * where
  K21 :: a -> K2 a b Fst
  K22 :: b -> K2 a b Snd

type PairUpResult a = K2 [(a, a)] (a, [(a, a)])

-- At last, here is the fold pairUp, taking even length lists to lists of pairs:

pairUp :: Alt a Fst -> [(a, a)]
pairUp xs = let (K21 xss) = (fstPHom (fold (mkPHom phi psi))) xs in xss 
 where
   phi :: FAlt y (K2 v (r,[(y,r)])) s -> K2 [(y,r)] (y,z) s
   phi FZero                       = K21 []
   phi (FSucc1 x1 (K22 (x2, xss))) = K21 ((x1, x2):xss) 

   psi :: FAlt y (K2 z w) s -> K2 [x] (y,z) s
   psi (FSucc2 x (K21 xss)) = K22 (x, xss) 

main = print (Succ1 (0::Int) $ Succ2 1 $ Succ1 2 $ Succ2 3 $ Succ1 4 $ Succ2 5 Zero)
