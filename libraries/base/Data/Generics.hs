-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org, ralf@cwi.nl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generic programming in Haskell; 
-- see <http://www.cs.vu.nl/boilerplate>.
--
-----------------------------------------------------------------------------

module Data.Generics ( 

	-- The Typeable class and the type-safe cast operation;
        -- re-exported for convenience
	Typeable(..), cast,

	-- * Prime types of generic functions
        GenericT, GenericQ, GenericM, GenericB,

	-- * Combinators to \"make\" generic functions
	mkT, mkQ, mkM, mkF, mkB,
	extT, extQ, extM, extF, extB,

	-- * The Data class for folding and unfolding constructor applications
	Data( 
	      gfoldl,
              gunfold,
	      conOf,
              consOf 
            ),

        -- * Typical generic maps defined in terms of gfoldl 

	gmapT,
        gmapQ, 
        gmapM,
        gmapF,

        -- * The Constr datatype for describing datatype constructors
        Constr(..),	

	-- * Frequently used generic traversal schemes
        everywhere,
        everywhere',
        everywhereBut,
        everywhereM,
        somewhere,
	everything,
        something,
	synthesize,

	-- * Generic operations such as show, equality, read
	glength,
	gcount,
	garity,
	gundefineds,
	gnodecount,
	gtypecount,
	gshow,
	geq,
	gzip,
	gread,

	-- * Miscellaneous further combinators
	sameType, orElse, recoverF, recoverQ, choiceF, choiceQ

#ifndef __HADDOCK__
	,
	-- Data types for the sum-of-products type encoding;
        -- included for backwards compatibility; maybe obsolete
	(:*:)(..), (:+:)(..), Unit(..)
#endif

 ) where

------------------------------------------------------------------------------

import Prelude	-- So that 'make depend' works

#ifdef __GLASGOW_HASKELL__
#ifndef __HADDOCK__
import GHC.Base ( (:*:)(..), (:+:)(..), Unit(..) )
#endif
#endif

import Data.Maybe
import Data.Dynamic
import Control.Monad



------------------------------------------------------------------------------
--
--	Prime types of generic functions
--
------------------------------------------------------------------------------

-- | Generic transformations,
--   i.e., take an \"a\" and return an \"a\"
--
type GenericT = forall a. Data a => a -> a


-- | Generic queries of type "r",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Generic monadic transformations,
--   i.e., take an \"a\" and compute an \"a\"
--
type GenericM m = forall a. Data a => a -> m a


-- | Generic builders with input i,
--   i.e., take an \"i\" and compute a pair of type (a,i)
--
type GenericB m i = forall a. Data a => i -> m (a,i)



------------------------------------------------------------------------------
--
--	Combinators to "make" generic functions
--	We use type-safe cast in a number of ways to make generic functions.
--
------------------------------------------------------------------------------

-- | Make a generic transformation;
--   start from a type-specific case;
--   preserve the term otherwise
--
mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f = case cast f of
               Just g -> g
               Nothing -> id


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` br) a = case cast a of
                    Just b  -> br b
                    Nothing -> r


-- | Make a generic monadic transformation;
--   start from a type-specific case;
--   resort to return otherwise
--
mkM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m)
    => (b -> m b) -> a -> m a
mkM f = case cast f of
          Just g  -> g
          Nothing -> return


{-

For the remaining definitions, we stick to a more concise style, i.e.,
we fold maybies with "maybe" instead of case ... of ..., and we also
use a point-free style whenever possible.

-}


-- | Make a generic monadic transformation for MonadPlus;
--   use "const mzero" (i.e., failure) instead of return as default.
--
mkF :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), MonadPlus m)
    => (b -> m b) -> a -> m a
mkF = maybe (const mzero) id . cast


-- | Make a generic builder;
--   start from a type-specific ase;
--   resort to no build (i.e., mzero) otherwise
--
mkB :: (Typeable a, Typeable b,
	Typeable i,
        Typeable (m (a,i)), Typeable (m (b,i)),
        MonadPlus m)
    => (i -> m (b,i)) -> i -> m (a,i)
mkB = maybe (const mzero) id . cast


-- | Extend a generic transformation by a type-specific case
extT :: (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT f = maybe f id . cast


-- | Extend a generic query by a type-specific case
extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = maybe (f a) g (cast a)


-- | Extend a generic monadic transformation by a type-specific case
extM :: (Typeable a, Typeable b,
         Typeable (m a), Typeable (m b), 
         Monad m)
     => (a -> m a) -> (b -> m b) -> a -> m a
extM f = maybe f id . cast


-- | Extend a generic MonadPlus transformation by a type-specific case
extF :: (Typeable a, Typeable b,
         Typeable (m a), Typeable (m b), 
         MonadPlus m)
     => (a -> m a) -> (b -> m b) -> a -> m a
extF = extM


-- | Extend a generic builder by a type-specific case
extB :: (Typeable a, Typeable b,
         Typeable i,
         Typeable (m (a,i)), Typeable (m (b,i)),
         MonadPlus m)
     => (i -> m (a,i)) -> (i -> m (b,i)) -> i -> m (a,i)
extB f = maybe f id . cast



------------------------------------------------------------------------------
--
--	The Data class
--
------------------------------------------------------------------------------

{- 

The Data class comprehends two important primitives "gfoldl" and
"gunfold" for folding and unfolding constructor applications, say
terms. Besides, there are helpers "conOf" and "consOf" for retrieving
constructors from terms and types. Finally, typical ways of mapping
over immediate subterms are defined as "gmap" combinators in terms
of gfoldl. A generic programmer does not necessarily need to use
the ingenious gfoldl/gunfold couple but rather the "gmap" combinators. 

-}

class Typeable a => Data a where

{-

Folding constructor applications ("gfoldl")

The combinator takes two arguments "f" and "z" to fold over a term
"x".  The result type is parametric via a type constructor "c" in the
type of "gfoldl". The purpose of "z" is to define how the empty
constructor application is folded. So "z" is like the neutral / start
element for list folding. The purpose of "f" is to define how the
nonempty constructor application is folded. That is, "f" takes the
folded "tail" of the constructor application and its head, i.e., an
immediate subterm, and combines them in some way. See the Data
instances in this file which illustrate gfoldl. Conclusion: the type
of gfoldl is a headache, but operationally it is simple generalisation
of a list fold.

-}

  -- | Left-associative fold operation for constructor applications
  gfoldl  :: (forall a b. Data a => c (a -> b) -> a -> c b)
          -> (forall g. g -> c g)
          -> a -> c a

{-

Unfolding constructor applications ("gunfold")

The combinator takes alike "gfoldl" two arguments "f" and "z", but
this time its about constructing (say, unfolding) constructor
applications rather than folding. The input for unfolding is primarily
an opaque representation of the desired constructor, which is
essentially a string representation of the constructor. (It is in the
responsibility of the programmer not to attempt unfolding invalid
constructors.  This is like the side condition that a programmer must
not apply the "head" function to the empty list.) Besides the
constructor, we also have to provide the "input" for constructing
immediate subterms. This is anticipated via the type constructor "c"
in the type of "gunfold". For example, in the case of a generic read
function, "c" models string-processing functions. So "z" defines how
to construct the empty constructor application, and "f" takes an
incomplete constructor application to add more immediate subterm.
Conclusion: the type of gunfoldl and what it does is a headache, but
operationally it is a simple generalisation of the underappreciated
list unfold.

-}

  -- | Unfold operation to build terms from constructors and others
  gunfold :: (forall a b. Data a => c (a -> b) -> c b)
          -> (forall g. g -> c g)
          -> Constr
          -> c a

  -- Default definition for gfoldl
  -- which copes immediately with basic datatypes
  --
  gfoldl _ z = z

  -- | Obtain the constructor from a given term
  conOf   :: a -> Constr

  -- | List all constructors for a given type
  consOf  :: a -> [Constr]



------------------------------------------------------------------------------
--
--	Typical generic maps defined in terms of gfoldl
--
------------------------------------------------------------------------------

{-

The combinators gmapT, gmapQ, gmapM, gmapF can all be defined in terms
of gfoldl. We provide corresponding default definitions leaving open
the opportunity to provide datatype-specific definitions if needed.

(Also, the inclusion of the gmap combinators as members of class Data
allows the programmer or the compiler to derive specialised, and maybe
more efficient code per datatype. Note: gfoldl is more higher-order
than the gmap combinators. This is subject to ongoing benchmarking
experiments.)

Conceptually, the definition of the gmap combinators in terms of the
primitive gfoldl requires the identification of the gfoldl function
arguments. Technically, we also need to identify the type constructor
c used all over the type of gfoldl.

-}

  -- | A generic transformation that maps over the immediate subterms
  gmapT   :: (forall b. Data b => b -> b) -> a -> a

  -- Use an identity datatype constructor ID (see below)
  -- to instantiate the type constructor c in the type of gfoldl,
  -- and perform injections ID and projections unID accordingly.
  --
  gmapT f x = unID (gfoldl k ID x)
    where
      k (ID c) x = ID (c (f x))


  -- | A generic query that processes the immediate subterms and returns a list
  gmapQ   :: (forall a. Data a => a -> u) -> a -> [u]

  -- Use a phantom + function datatype constructor Q (see below),
  -- to instantiate the type constructor c in the type of gfoldl,
  -- and perform injections Q and projections unQ accordingly.
  --
  gmapQ f x = unQ (gfoldl k (const (Q id)) x) []
    where
      k (Q c) x = Q (\rs -> c (f x : rs))


  -- | A generic monadic transformation that maps over the immediate subterms
  gmapM   :: Monad m => (forall a. Data a => a -> m a) -> a -> m a

  -- Use immediately the monad datatype constructor 
  -- to instantiate the type constructor c in the type of gfoldl,
  -- so injection and projection is done by return and >>=.
  --  
  gmapM f = gfoldl k return
    where
      k c x = do c' <- c
                 x' <- f x
                 return (c' x')


  -- | Transformation of at least one immediate subterm does not fail
  gmapF :: MonadPlus m => (forall a. Data a => a -> m a) -> a -> m a

  -- Use a datatype constructor F (see below)
  -- to instantiate the type constructor c in the type of gfoldl.
  --  
  gmapF f x = unF (gfoldl k z x) >>= \(x',b) ->
              if b then return x' else mzero
    where
      z g = F (return (g,False))
      k (F c) x
        = F ( c >>= \(h,b) -> 
              (f x >>= \x' -> return (h x',True))
              `mplus` return (h x, b)
            )


-- | The identity type constructor needed for the definition of gmapT
newtype ID x = ID { unID :: x }


-- | A phantom datatype constructor used in definition of gmapQ;
--   the function-typed component is needed to mediate between
--   left-associative constructor application vs. right-associative lists.
-- 
newtype Q r a = Q { unQ  :: [r] -> [r] }


-- | A pairing type constructor needed for the definition of gmapF;
-- we keep track of the fact if a subterm was ever transformed successfully.
newtype F m x = F { unF :: m (x, Bool) }



------------------------------------------------------------------------------
--
--	The Constr datatype for describing datatype constructors
--      To be extended by fixity, associativity, and maybe others.
--
------------------------------------------------------------------------------

-- | Description of datatype constructors
data Constr = Constr { conString :: String } deriving (Eq, Typeable)


{-

It is interesting to observe that we can determine the arity of a
constructor without further meta-information. To this end, we use
gunfold to construct a term from a given constructor while leaving the
subterms undefined; see "gundefineds" below. Here we instantiate the
type constructor c of the gunfold type by the identity type
constructor ID. In a subsequent step we determine the number of
subterms by folding as captured in the generic operation "glength"
elsewhere in this module. Note that we need a type argument to specify
the intended type of the constructor.

-}


-- | Compute arity of a constructor against a type argument
garity :: Data a => (a -> ()) -> Constr -> Int
garity ta = glength . gundefineds ta


-- | Construct a term from a constructor with undefined subterms
gundefineds :: Data a => (a -> ()) -> Constr -> a
gundefineds (_::a -> ()) = (unID :: ID a -> a)
                         . gunfold ((\f -> ID (f undefined)) . unID) ID



------------------------------------------------------------------------------
--
--	Frequently used generic traversal schemes
--
------------------------------------------------------------------------------

-- | Apply a transformation everywhere in bottom-up manner
everywhere :: (forall a. Data a => a -> a)
           -> (forall a. Data a => a -> a)

-- Use gmapT to recurse into immediate subterms;
-- recall: gmapT preserves the outermost constructor;
-- post-process recursively transformed result via f
-- 
everywhere f = f . gmapT (everywhere f)


-- | Apply a transformation everywhere in top-down manner
everywhere' :: (forall a. Data a => a -> a)
            -> (forall a. Data a => a -> a)

-- Arguments of (.) are flipped compared to everywhere
everywhere' f = gmapT (everywhere' f) . f


-- | Variation on everywhere with an extra stop condition
everywhereBut :: GenericQ Bool -> GenericT -> GenericT

-- Guarded to let traversal cease if predicate q holds for x
everywhereBut q f x
    | q x       = x
    | otherwise = f (gmapT (everywhereBut q f) x)


-- | Monadic variation on everywhere
everywhereM :: Monad m => GenericM m -> GenericM m

-- Bottom-up order is also reflected in order of do-actions
everywhereM f x = do x' <- gmapM (everywhereM f) x
                     f x'


-- | Apply a monadic transformation at least somewhere
somewhere :: MonadPlus m => GenericM m -> GenericM m

-- We try "f" in top-down manner, but descent into "x" when we fail
-- at the root of the term. The transformation fails if "f" fails
-- everywhere, say succeeds nowhere.
-- 
somewhere f x = f x `mplus` gmapF (somewhere f) x


-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results
-- 
everything k f x 
     = foldl k (f x) (gmapQ (everything k f) x)


-- | Look up a subterm by means of a maybe-typed filter
something :: GenericQ (Maybe u) -> GenericQ (Maybe u)

-- "something" can be defined in terms of "everything"
-- when a suitable "choice" operator is used for reduction
-- 
something = everything orElse


-- | Bottom-up synthesis of a data structure;
--   1st argument z is the initial element for the synthesis;
--   2nd argument o is for reduction of results from subterms;
--   3rd argument f updates the sythesised data according to the given term
--
synthesize :: s  -> (s -> s -> s) -> GenericQ (s -> s) -> GenericQ s
synthesize z o f x = f x (foldr o z (gmapQ (synthesize z o f) x))



-----------------------------------------------------------------------------
--
--	"Twin" variations on gmapT, gmapQ. gmapM,
--      i.e., these combinators take two terms at the same time.
--	They are needed for multi-parameter traversal as generic equality.
--	They are not exported.
--
-----------------------------------------------------------------------------

{-

We need type constructors for twin traversal as we needed type
constructor for the ordinary gmap combinators. These type constructors
again serve for the instantiation of the type constructor c used in
the definition of gfoldl. The type constructors for twin traversal are
elaborations of the type constructors ID, Q and monads that were used
for the ordinary gmap combinators. More precisely, we use a pairing
technique to always attach an additional component to the results of
folding. This additional component carries the list of generic 
functions to be used for the intermediate subterms encountered during
folding.

-}

newtype TT r a = TT { unTT :: (a,[GenericT']) }
newtype TQ r a = TQ { unTQ :: ([r]->[r],[GenericQ' r]) }
newtype TM m a = TM { unTM :: (m a,[GenericM' m]) }


-- First-class polymorphic versions of GenericT/GenericQ/GenericM;
-- they are referenced in TQ amd TM above
-- 
data GenericT' = T' { unT' :: forall a. Data a => a -> a }
data GenericQ' u = Q' { unQ' :: forall a. Data a => a -> u }
data Monad m => GenericM' m = M' { unM' :: forall a. Data a => a -> m a }


{-

A twin variation on gmapT, where the pattern "GenericQ GenericT"
expresses that the argument terms x and y are processed rather
independently. So firstly, x is "queried" with a generic
transformation as intermediate result, and secondly, this generic
transformation is applied to y.

-}

tmapT :: GenericQ GenericT -> GenericQ GenericT
tmapT g x y = fst (unTT (gfoldl k z y))
  where
    k (TT (f,l)) x = TT (f (unT' (head l) x),tail l)
    z f            = TT (f,gmapQ (\x -> T' (g x)) x)



-- A twin variation on gmapQ

tmapQ :: forall r.
         (forall a b. (Data a, Data b) => a -> b -> r)
      -> (forall a b. (Data a, Data b) => a -> b -> [r])

tmapQ g x y = fst (unTQ (gfoldl k z y)) []
    where
      k (TQ (c,l)) x = TQ (\rs -> c (unQ' (head l) x:rs), tail l)
      z _            = TQ (id,gmapQ (\x -> Q' (g x)) x)


-- A twin variation on gmapM

tmapM :: forall m. Monad m
      => (forall a b. (Data a, Data b) => a -> b -> m b)
      -> (forall a b. (Data a, Data b) => a -> b -> m b)
tmapM g x y = fst (unTM (gfoldl k z y))
  where
    k (TM (f,l)) x = TM (f >>= \f' -> unM' (head l) x >>= return . f',tail l)
    z f            = TM (return f,gmapQ (\x -> M' (g x)) x)



------------------------------------------------------------------------------
--
--	Generic operations such as show, equality, read
--
------------------------------------------------------------------------------

-- | Count the number of immediate subterms of the given term
glength :: GenericQ Int
glength = length . gmapQ (const ())


-- | Determine the number of all suitable nodes in a given term
gcount :: GenericQ Bool -> GenericQ Int
gcount p =  everything (+) (\x -> if p x then 1 else 0)


-- | Determine the number of all nodes in a given term
gnodecount :: GenericQ Int
gnodecount = gcount (const True)


-- | Determine the number of nodes of a given type in a given term
gtypecount :: Typeable a => (a -> ()) -> GenericQ Int
gtypecount f = gcount (False `mkQ` (const True . f))


-- | Generic show: an alternative to "deriving Show"
gshow :: Data a => a -> String

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
-- 
gshow = ( \t ->
                "("
             ++ conString (conOf t)
             ++ concat (gmapQ ((++) " " . gshow) t)
             ++ ")"
        ) `extQ` (show :: String -> String)


-- | Generic equality: an alternative to "deriving Eq"
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
  geq' x y = and ( (conString (conOf x) == conString (conOf y))
                 : tmapQ geq' x y
                 )


-- | Generic zip controlled by a function with type-specific branches
gzip :: (forall a b. (Data a, Data b) => a -> b -> Maybe b)
     -> (forall a b. (Data a, Data b) => a -> b -> Maybe b)

-- See testsuite/.../Generics/gzip.hs for an illustration
gzip f x y = 
  f x y
  `orElse`
  if conString (conOf x) == conString (conOf y)
   then tmapM (gzip f) x y
   else Nothing


-- | The type constructor for gunfold a la ReadS from the Haskell 98 Prelude;
--   we don't use lists here for simplicity but only maybes.
newtype GRead i a = GRead (i -> Maybe (a, i))
unGRead (GRead x) = x


-- | Generic read: an alternative to "deriving Read"
gread :: GenericB Maybe String

{-

This is a read operation which insists on prefix notation.  (The
Haskell 98 read deals with infix operators as well. We will be able to
deal with such special cases as well as sonn as we include fixity
information into the definition of "Constr".)  We use gunfold to
"parse" the input. To be precise, gunfold is used for all result types
except String. The type-specific case for String uses basic String
read. Another source of customisation would be to properly deal with
infix operators subject to the capture of that information in the
definition of Constr. The "gread" combinator properly checks the 
validity of constructors before invoking gunfold in order to rule
out run-time errors.

-}

gread = gdefault `extB` scase

 where

  -- a specific case for strings
  scase s = case reads s of
              [x::(String,String)] -> Just x
              _ -> Nothing

  -- the generic default of gread
  gdefault s =
    do s' <- return $ dropWhile ((==) ' ') s
       guard (not (s' == ""))
       guard (head s' == '(')
       (c,s'')  <- prefixConstr (dropWhile ((==) ' ') (tail s'))
       u <- return undefined 
       guard (or [consOf u == [], c `elem` consOf u])
       (a,s''') <- unGRead (gunfold f z c) s''
       _ <- return $ constrainTypes a u
       guard (not (s''' == "")) 
       guard (head s''' == ')')
       return (a, tail s''')

  -- To force two types to be the same
  constrainTypes :: a -> a -> ()
  constrainTypes _ _ = ()

  -- Argument f for unfolding
  f :: Data a => GRead String (a -> b) -> GRead String b
  f x = GRead (\s -> do (r,s') <- unGRead x s
                        (t,s'')  <- gread s'
                        return (r t,s''))

  -- Argument z for unfolding
  z ::  forall g. g -> GRead String g
  z g = GRead (\s -> return (g,s))

  -- Get Constr at front of string
  prefixConstr :: String -> Maybe (Constr, String)

  -- Assume an infix operators in parantheses
  prefixConstr ('(':s)
    = case break ((==) ')') s of
        (s'@(_:_),(')':s'')) -> Just (Constr ("(" ++ s' ++ ")"), s'')
        _ -> Nothing

  -- Special treatment of multiple token constructors
  prefixConstr ('[':']':s) = Just (Constr "[]",s)

  -- Try lex for ordinary constructor and basic datatypes
  prefixConstr s
    = case lex s of
        [(s'@(_:_),s'')] -> Just (Constr s',s'')
        _ -> Nothing



------------------------------------------------------------------------------
--
--	Instances of the Data class
--
------------------------------------------------------------------------------

-- Basic datatype Int; folding and unfolding is trivial
instance Data Int where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z c = z (read (conString c))

-- Another basic datatype instance
instance Data Integer where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z c = z (read (conString c))

-- Another basic datatype instance
instance Data Float where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z c = z (read (conString c))

-- Another basic datatype instance
instance Data Char where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z c = z (read (conString c))

{-

Commented out;
subject to inclusion of a missing Typeable instance

-- Another basic datatype instance
instance Data Rational where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z c = z (read (conString c))

-}

-- Bool as a kind of enumeration type
instance Data Bool where
 conOf False = Constr "False"
 conOf True  = Constr "True"
 consOf _    = [Constr "False",Constr "True"]
 gunfold f z (Constr "False") = z False
 gunfold f z (Constr "True")  = z True

{-

We should better not fold over characters in a string for efficiency.
However, the following instance would clearly overlap with the
instance for polymorphic lists. Given the current scheme of allowing
overlapping instances, this would imply that ANY module that imports
Data.Generics would need to explicitly and generally allow overlapping
instances. This is prohibitive and calls for a more constrained model
of allowing overlapping instances. The present instance would also be
more sensible for UNFOLDING. In the definition of gread, we still
obtained the favoured behaviour by using a type-specific case for
String.

-- instance Data String where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z c = z (read (conString c))

-}

-- Cons-lists are terms with two immediate subterms. Hence, the gmap
-- combinators do NOT coincide with the list fold/map combinators.
--
instance Data a => Data [a] where
  gmapT  f   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  f   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  f   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')
  gfoldl f z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  conOf [] = Constr "[]"
  conOf (_:_) = Constr "(:)"
  consOf _ = [Constr "[]",Constr "(:)"]
  gunfold f z (Constr "[]")  = z []
  gunfold f z (Constr "(:)") = f (f (z (:)))

-- Yet enother polymorphic datatype constructor
instance Data a => Data (Maybe a) where
 gfoldl f z Nothing  = z Nothing
 gfoldl f z (Just x) = z Just `f` x
 conOf Nothing  = Constr "Nothing"
 conOf (Just _) = Constr "Just"
 consOf _ = [Constr "Nothing", Constr "Just"]
 gunfold f z c | conString c == "Nothing" = z Nothing
 gunfold f z c | conString c == "Just"    = f (z Just)

-- Yet enother polymorphic datatype constructor
instance (Data a, Data b) => Data (a,b) where
 gfoldl f z (a,b) = z (,) `f` a `f` b
 conOf _ = Constr "(,)"
 consOf _ = [Constr "(,)"]
 gunfold f z c | conString c == "(,)" = f (f (z (,)))

-- Functions are treated as "non-compound" data regarding folding while
-- unfolding is out of reach, maybe not anymore with Template Haskell.
-- 
instance (Typeable a, Typeable b) => Data (a -> b) where
 conOf _ = Constr "->"
 consOf _ = [Constr "->"]
 gunfold _ _ _ = undefined



------------------------------------------------------------------------------
--
--	Miscellaneous
--
------------------------------------------------------------------------------

-- | Test for two objects to agree on the type
sameType :: (Typeable a, Typeable b) => a -> b -> Bool
sameType (_::a) = maybe False (\(_::a) -> True) . cast


-- | Left-biased choice on maybes (non-strict in right argument)
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = maybe y Just x


-- Another definition of orElse
-- where the folding over maybies as defined by maybe is inlined
-- to ease readability
-- 
x `orElse'` y = case x of
                  Just _  -> x
                  Nothing -> y


{-

The following variations take "orElse" to the function
level. Furthermore, we generalise from "Maybe" to any
"MonadPlus". This makes sense for monadic transformations and
queries. We say that the resulting combinators modell choice. We also
provide a prime example of choice, that is, recovery from failure. In
the case of transformations, we recover via return whereas for
queries a given constant is returned.

-}

-- | Choice for monadic transformations
choiceF :: MonadPlus m => GenericM m -> GenericM m -> GenericM m
choiceF f g x = f x `mplus` g x


-- | Choice for monadic queries
choiceQ :: MonadPlus m => GenericQ (m r) -> GenericQ (m r) -> GenericQ (m r)
choiceQ f g x = f x `mplus` g x


-- | Recover from the failure of monadic transformation by identity
recoverF :: MonadPlus m => GenericM m -> GenericM m
recoverF f = f `choiceF` return


-- | Recover from the failure of monadic query by a constant
recoverQ :: MonadPlus m => r -> GenericQ (m r) -> GenericQ (m r)
recoverQ r f = f `choiceQ` const (return r)
