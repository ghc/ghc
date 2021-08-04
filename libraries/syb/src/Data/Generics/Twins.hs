{-# LANGUAGE RankNTypes, ScopedTypeVariables, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Twins
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module
-- provides support for multi-parameter traversal, which is also
-- demonstrated with generic operations like equality.
--
-----------------------------------------------------------------------------

module Data.Generics.Twins (

        -- * Generic folds and maps that also accumulate
        gfoldlAccum,
        gmapAccumT,
        gmapAccumM,
        gmapAccumQl,
        gmapAccumQr,
        gmapAccumQ,
        gmapAccumA,

        -- * Mapping combinators for twin traversal
        gzipWithT,
        gzipWithM,
        gzipWithQ,

        -- * Typical twin traversals
        geq,
        gzip,
        gcompare

  ) where


------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Data.Data
import Data.Generics.Aliases

#ifdef __GLASGOW_HASKELL__
import Prelude hiding ( GT )
#endif

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative (Applicative(..))
import Data.Monoid         ( mappend, mconcat )
#endif

------------------------------------------------------------------------------


------------------------------------------------------------------------------
--
--      Generic folds and maps that also accumulate
--
------------------------------------------------------------------------------

{--------------------------------------------------------------

A list map can be elaborated to perform accumulation.
In the same sense, we can elaborate generic maps over terms.

We recall the type of map:
map :: (a -> b) -> [a] -> [b]

We recall the type of an accumulating map (see Data.List):
mapAccumL :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])

Applying the same scheme we obtain an accumulating gfoldl.

--------------------------------------------------------------}

-- | gfoldl with accumulation

gfoldlAccum :: Data d
            => (forall e r. Data e => a -> c (e -> r) -> e -> (a, c r))
            -> (forall g. a -> g -> (a, c g))
            -> a -> d -> (a, c d)

gfoldlAccum k z a0 d = unA (gfoldl k' z' d) a0
 where
  k' c y = A (\a -> let (a', c') = unA c a in k a' c' y)
  z' f   = A (\a -> z a f)


-- | A type constructor for accumulation
newtype A a c d = A { unA :: a -> (a, c d) }


-- | gmapT with accumulation
gmapAccumT :: Data d
           => (forall e. Data e => a -> e -> (a,e))
           -> a -> d -> (a, d)
gmapAccumT f a0 d0 = let (a1, d1) = gfoldlAccum k z a0 d0
                     in (a1, unID d1)
 where
  k a (ID c) d = let (a',d') = f a d
                  in (a', ID (c d'))
  z a x = (a, ID x)


-- | Applicative version
gmapAccumA :: forall b d a. (Data d, Applicative a)
           => (forall e. Data e => b -> e -> (b, a e))
           -> b -> d -> (b, a d)
gmapAccumA f a0 d0 = gfoldlAccum k z a0 d0
    where
      k :: forall d' e. (Data d') =>
           b -> a (d' -> e) -> d' -> (b, a e)
      k a c d = let (a',d') = f a d
                    c' = c <*> d'
                in (a', c')
      z :: forall t c a'. (Applicative a') =>
           t -> c -> (t, a' c)
      z a x = (a, pure x)


-- | gmapM with accumulation
gmapAccumM :: (Data d, Monad m)
           => (forall e. Data e => a -> e -> (a, m e))
           -> a -> d -> (a, m d)
gmapAccumM f = gfoldlAccum k z
 where
  k a c d = let (a',d') = f a d
             in (a', d' >>= \d'' -> c >>= \c' -> return (c' d''))
  z a x = (a, return x)


-- | gmapQl with accumulation
gmapAccumQl :: Data d
            => (r -> r' -> r)
            -> r
            -> (forall e. Data e => a -> e -> (a,r'))
            -> a -> d -> (a, r)
gmapAccumQl o r0 f a0 d0 = let (a1, r1) = gfoldlAccum k z a0 d0
                           in (a1, unCONST r1)
 where
  k a (CONST c) d = let (a', r) = f a d
                     in (a', CONST (c `o` r))
  z a _ = (a, CONST r0)


-- | gmapQr with accumulation
gmapAccumQr :: Data d
            => (r' -> r -> r)
            -> r
            -> (forall e. Data e => a -> e -> (a,r'))
            -> a -> d -> (a, r)
gmapAccumQr o r0 f a0 d0 = let (a1, l) = gfoldlAccum k z a0 d0
                           in (a1, unQr l r0)
 where
  k a (Qr c) d = let (a',r') = f a d
                  in (a', Qr (\r -> c (r' `o` r)))
  z a _ = (a, Qr id)


-- | gmapQ with accumulation
gmapAccumQ :: Data d
           => (forall e. Data e => a -> e -> (a,q))
           -> a -> d -> (a, [q])
gmapAccumQ f = gmapAccumQr (:) [] f



------------------------------------------------------------------------------
--
--      Helper type constructors
--
------------------------------------------------------------------------------


-- | The identity type constructor needed for the definition of gmapAccumT
newtype ID x = ID { unID :: x }


-- | The constant type constructor needed for the definition of gmapAccumQl
newtype CONST c a = CONST { unCONST :: c }


-- | The type constructor needed for the definition of gmapAccumQr
newtype Qr r a = Qr { unQr  :: r -> r }



------------------------------------------------------------------------------
--
--      Mapping combinators for twin traversal
--
------------------------------------------------------------------------------


-- | Twin map for transformation
gzipWithT :: GenericQ (GenericT) -> GenericQ (GenericT)
gzipWithT f x y = case gmapAccumT perkid funs y of
                    ([], c) -> c
                    _       -> error "gzipWithT"
 where
  perkid a d = (tail a, unGT (head a) d)
  funs = gmapQ (\k -> GT (f k)) x



-- | Twin map for monadic transformation
gzipWithM :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
gzipWithM f x y = case gmapAccumM perkid funs y of
                    ([], c) -> c
                    _       -> error "gzipWithM"
 where
  perkid a d = (tail a, unGM (head a) d)
  funs = gmapQ (\k -> GM (f k)) x


-- | Twin map for queries
gzipWithQ :: GenericQ (GenericQ r) -> GenericQ (GenericQ [r])
gzipWithQ f x y = case gmapAccumQ perkid funs y of
                   ([], r) -> r
                   _       -> error "gzipWithQ"
 where
  perkid a d = (tail a, unGQ (head a) d)
  funs = gmapQ (\k -> GQ (f k)) x



------------------------------------------------------------------------------
--
--      Typical twin traversals
--
------------------------------------------------------------------------------

-- | Generic equality: an alternative to \"deriving Eq\"
geq :: Data a => a -> a -> Bool

{-

Testing for equality of two terms goes like this. Firstly, we
establish the equality of the two top-level datatype
constructors. Secondly, we use a twin gmap combinator, namely tgmapQ,
to compare the two lists of immediate subterms.

(Note for the experts: the type of the worker geq' is rather general
but precision is recovered via the restrictive type of the top-level
operation geq. The imprecision of geq' is caused by the type system's
unability to express the type equivalence for the corresponding
couples of immediate subterms from the two given input terms.)

-}

geq x0 y0 = geq' x0 y0
  where
    geq' :: GenericQ (GenericQ Bool)
    geq' x y =     (toConstr x == toConstr y)
                && and (gzipWithQ geq' x y)


-- | Generic zip controlled by a function with type-specific branches
gzip :: GenericQ (GenericM Maybe) -> GenericQ (GenericM Maybe)
-- See testsuite/.../Generics/gzip.hs for an illustration
gzip f = go
  where
    go :: GenericQ (GenericM Maybe)
    go x y =
      f x y
      `orElse`
      if toConstr x == toConstr y
        then gzipWithM go x y
        else Nothing

-- | Generic comparison: an alternative to \"deriving Ord\"
gcompare :: Data a => a -> a -> Ordering
gcompare = gcompare'
  where
    gcompare' :: (Data a, Data b) => a -> b -> Ordering
    gcompare' x y
      = let repX = constrRep $ toConstr x
            repY = constrRep $ toConstr y
        in
        case (repX, repY) of
          (AlgConstr nX,   AlgConstr nY)   ->
            nX `compare` nY `mappend` mconcat (gzipWithQ (\a -> gcompare' a) x y)
          (IntConstr iX,   IntConstr iY)   -> iX `compare` iY
          (FloatConstr rX, FloatConstr rY) -> rX `compare` rY
          (CharConstr cX,  CharConstr cY)  -> cX `compare` cY
          _ -> error "type incompatibility in gcompare"
