-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Twins
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module 
-- provides support for multi-parameter traversal, which is also 
-- demonstrated with generic operations like equality.
--
-----------------------------------------------------------------------------

module Data.Generics.Twins ( 

	-- * The idiom for multi-parameter traversal
        gzipWith,

	-- * Mapping combinators with an additional list
	gzipWithT,
	gzipWithM,
	gzipWithQ,
	gzipWithQl,
	gzipWithQr,

	-- * Mapping combinators for twin traversal
	tmapT,
  	tmapM,
  	tmapQ,


	-- * Typical twin traversals
	geq,
	gzip

  ) where


------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Data.Generics.Basics
import Data.Generics.Aliases

------------------------------------------------------------------------------


------------------------------------------------------------------------------
--
--	The idiom for multi-parameter traversal
--
------------------------------------------------------------------------------

{-

gfoldl and friends so far facilitated traversal of a single term. We
will now consider an idiom gfoldlWith to traverse two terms
semi-simultaneously. By cascasding this idiom, we can also traverse
more than two terms. The gfoldlWith primitive completes gfoldl in a
way that is similar to the well-known couple map and
zipWith. Basically, gfoldlWith takes an additional argument, namely a
list, and this list is traversed simultaneously with the immediate
subterms of a given term.

-}


-- | gfoldl with an additional list
gzipWith :: Data a
         => (forall a b. Data a => d -> c (a -> b) -> a -> c b)
         -> (forall g. g -> c g)
         -> [d]
         -> a
         -> c a

gzipWith k z l x = case gfoldl k' z' x of { WITH _ c -> c }
 where
   k' (WITH (h:t) c) y = WITH t (k h c y)
   k' (WITH []    _) _ = error "gzipWith"
   z' f                = WITH l (z f)


-- | A type constructor for folding over the extra list
data WITH q c a   = WITH [q] (c a) 



------------------------------------------------------------------------------
--
--	Mapping combinators with an additional list
--
------------------------------------------------------------------------------


-- | gmapT with an additional list
gzipWithT :: Data a 
          => (forall a. Data a => b -> a -> a)
          -> [b]
          -> a
          -> a

gzipWithT f l = unID . gzipWith k ID l
  where
    k b (ID c) x = ID $ c $ f b x


-- | gmapM with an additional list
gzipWithM :: (Data a, Monad m) 
          => (forall a. Data a => b -> a -> m a)
          -> [b]
          -> a
          -> m a

gzipWithM f = gzipWith k return 
  where
    k b c x = do c' <- c
                 x' <- f b x
                 return (c' x')


-- | gmapQl with an additional list
gzipWithQl :: Data a
           => (r -> r -> r) 
           -> r
           -> (forall a. Data a => b -> a -> r)
           -> [b]
           -> a 
           -> r

gzipWithQl o r f l = unCONST . gzipWith k z l
  where
    k b (CONST c) x = CONST (c `o` f b x)
    z _ = CONST r


-- | gmapQr with an additional list
gzipWithQr :: Data a
           => (r' -> r -> r) 
           -> r
           -> (forall a. Data a => b -> a -> r')
           -> [b]
           -> a 
           -> r

gzipWithQr o r f l x = unQr (gzipWith k z l x) r
    where
      k b (Qr c) x = Qr (\r -> c (f b x `o` r))
      z _ = Qr id


-- | gmapQ with an additional list
gzipWithQ :: Data a
      => (forall a. Data a => b -> a -> u)
      -> [b]  
      -> a 
      -> [u]

gzipWithQ f = gzipWithQr (:) [] f



------------------------------------------------------------------------------
--
--	Helper type constructors
--
------------------------------------------------------------------------------



-- | The identity type constructor needed for the definition of gzipWithT
newtype ID x = ID { unID :: x }


-- | The constant type constructor needed for the definition of gzipWithQl
newtype CONST c a = CONST { unCONST :: c }


-- | The type constructor needed for the definition of gzipWithQr
newtype Qr r a = Qr { unQr  :: r -> r }



------------------------------------------------------------------------------
--
--	Mapping combinators for twin traversal
--
------------------------------------------------------------------------------


-- | Twin map for transformation 
tmapT :: GenericQ (GenericT) -> GenericQ (GenericT)
tmapT f x y =
  gzipWithT unGenericT'
            (gmapQ (\x -> GenericT' (f x)) x)
            y


-- | Twin map for monadic transformation 
tmapM :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
tmapM f x y =
  gzipWithM unGenericM'
            (gmapQ (\x -> GenericM' (f x)) x)
            y


-- | Twin map for monadic transformation 
tmapQ :: GenericQ (GenericQ r) -> GenericQ (GenericQ [r])
tmapQ f x y =
  gzipWithQ unGenericQ'
            (gmapQ (\x -> GenericQ' (f x)) x)
            y



------------------------------------------------------------------------------
--
--	Typical twin traversals
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

geq x y = geq' x y
  where
    geq' :: forall a b. (Data a, Data b) => a -> b -> Bool
    geq' x y =     (toConstr x == toConstr y)
                && and (tmapQ geq' x y)


-- | Generic zip controlled by a function with type-specific branches
gzip :: (forall a b. (Data a, Data b) => a -> b -> Maybe b)
     -> (forall a b. (Data a, Data b) => a -> b -> Maybe b)


-- See testsuite/.../Generics/gzip.hs for an illustration
gzip f x y = 
  f x y
  `orElse`
  if toConstr x == toConstr y
    then tmapM (gzip f) x y
    else Nothing
