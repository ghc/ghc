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

	-- * The idiom of multi-parameter traversal
	tfoldl,

	-- * Twin mapping combinators
	tmapT,
	tmapQl,
	tmapM,

	-- * Prime examples of twin traversal
	geq,
	gzip

  ) where


------------------------------------------------------------------------------


import Data.Generics.Basics
import Data.Generics.Aliases


------------------------------------------------------------------------------


------------------------------------------------------------------------------
--
--	The idiom of multi-parameter traversal
--
------------------------------------------------------------------------------

{-

The fact that we traverse two terms semi-simultaneously is reflected
by the nested generic function type that occurs as the result type of
tfoldl. By "semi-simultaneously", we mean that we first fold over the
first term and compute a LIST of generic functions to be folded over
the second term. So the outermost generic function type is GenericQ
because we compute a list of generic functions which is a kind of
query.  The inner generic function type is parameterised in a type
constructor c so that we can instantiate twin traversal for
transformations (T), queries (Q), and monadic transformations (M).
The combinator tfoldl is also parameterised by a nested generic
function which serves as the function to be mapped over the first term
to get the functions to be mapped over the second term. The combinator
tfoldl is further parameterised by gfoldl-like parameters k and z
which however need to be lifted to k' and z' such that plain term
traversal is combined with list traversal (of the list of generic
functions).  That is, the essence of multi-parameter term traversal is
a single term traversal interleaved with a list fold. As the
definition of k' and z' details, the list fold can be arranged by the
ingredients of the term fold. To this end, we use a designated TWIN
datatype constructor which pairs a given type constructor c with a
list of generic functions.

-}

tfoldl :: (forall a b. Data a => c (a -> b) -> c a -> c b)
       -> (forall g. g -> c g)
       -> GenericQ (Generic c)
       -> GenericQ (Generic c)

tfoldl k z t xs ys = case gfoldl k' z' ys of { TWIN _ c -> c }
 where
   l = gmapQ (\x -> Generic' (t x)) xs
   k' (TWIN (r:rs) c) y = TWIN rs (k c (unGeneric' r y))
   z' f                 = TWIN l (z f)


-- Pairing ID, CONST, m or others with lists of generic functions
data TWIN c a   = TWIN [Generic' c] (c a) 



------------------------------------------------------------------------------
--
--	Twin mapping combinators
--
------------------------------------------------------------------------------

tmapT :: GenericQ (GenericT) -> GenericQ (GenericT)
tmapT f x y = unID $ tfoldl k z f' x y
 where
  f' x y = ID $ f x y
  k (ID c) (ID x) = ID (c x)
  z = ID


tmapQl :: (r -> r -> r) 
       -> r
       -> GenericQ (GenericQ r)
       -> GenericQ (GenericQ r)
tmapQl o r f x y = unCONST $ tfoldl k z f' x y
 where
  f' x y = CONST $ f x y
  k (CONST c) (CONST x) = CONST (c `o` x)  
  z _ = CONST r


tmapM :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
tmapM f x y = tfoldl k z f x y
 where
  k c x = do c' <- c
             x' <- x
             return $ c' x'
  z = return


-- The identity type constructor needed for the definition of tmapT
newtype ID x = ID { unID :: x }


-- The constant type constructor needed for the definition of tmapQl
newtype CONST c a = CONST { unCONST :: c }



------------------------------------------------------------------------------
--
--	Prime examples of twin traversal
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
  geq' x y = and [ (toConstr x == toConstr y)
                 , tmapQl (\b1 b2 -> and [b1,b2]) True geq' x y
                 ]


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
