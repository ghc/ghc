-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Basics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module provides
-- the Data class with its primitives for generic programming.
--
-----------------------------------------------------------------------------

module Data.Generics.Basics ( 

	-- Module Data.Typeable re-exported for convenience
	module Data.Typeable,

	-- * The Data class for processing constructor applications
	Data( 
		gfoldl,		-- :: ... -> a -> c a
		toConstr, 	-- :: a -> Constr
		fromConstr,	-- :: Constr -> a
		dataTypeOf	-- :: a -> DataType
		
            ),

	-- * Constructor representations
	Constr,		-- abstract, instance of: Eq, Show
	ConIndex,	-- alias for Int, start at 1
	Fixity(..),	-- instance of: Eq, Show
	DataType,	-- abstract, instance of: Show

	-- * Constructing constructor representations
	mkConstr,   	-- :: ConIndex -> String -> Fixity -> Constr
	mkDataType,	-- :: [Constr] -> DataType

	-- * Observing constructor representations
	conString,	-- :: Constr -> String
	conFixity,	-- :: Constr -> Fixity
	conIndex,	-- :: Constr -> ConIndex
	stringCon,	-- :: DataType -> String -> Maybe Constr
	indexCon,	-- :: DataType -> ConIndex -> Constr
	maxConIndex,	-- :: DataType -> ConIndex
	dataTypeCons, 	-- :: DataType -> [Constr]

        -- * Generic maps defined in terms of gfoldl 
	gmapT,
        gmapQ, 
        gmapQl,
        gmapQr,
        gmapM,
        gmapMp,
        gmapMo,

  ) where


------------------------------------------------------------------------------


import Data.Typeable
import Data.Maybe
import Control.Monad


------------------------------------------------------------------------------
--
--	The Data class
--
------------------------------------------------------------------------------

{- 

The Data class comprehends a fundamental primitive "gfoldl" for
folding over constructor applications, say terms. This primitive can
be instantiated in several ways to map over the immediate subterms of
a term; see the "gmap" combinators later in this module. Indeed, a
generic programmer does not necessarily need to use the ingenious
gfoldl primitive but rather the intuitive "gmap" combinators. The
"gfoldl" primitive is completed by means to query top-level
constructors, to turn constructor representations into proper terms,
and to list all possible datatype constructors. This completion
allows us to serve generic programming scenarios like read, show,
equality, term generation.

-}

class Typeable a => Data a where

{-

Folding constructor applications ("gfoldl")

The combinator takes two arguments "f" and "z" to fold over a term
"x".  The result type is defined in terms of "x" but variability is
achieved by means of type constructor "c" for the construction of the
actual result type. The purpose of the argument "z" is to define how
the empty constructor application is folded. So "z" is like the
neutral / start element for list folding. The purpose of the argument
"f" is to define how the nonempty constructor application is
folded. That is, "f" takes the folded "tail" of the constructor
application and its head, i.e., an immediate subterm, and combines
them in some way. See the Data instances in this file for an
illustration of "gfoldl". Conclusion: the type of gfoldl is a
headache, but operationally it is simple generalisation of a list
fold.

-}

  -- | Left-associative fold operation for constructor applications
  gfoldl  :: (forall a b. Data a => c (a -> b) -> a -> c b)
          -> (forall g. g -> c g)
          -> a -> c a

  -- Default definition for gfoldl
  -- which copes immediately with basic datatypes
  --
  gfoldl _ z = z


  -- | Obtaining the constructor from a given datum.
  -- For proper terms, this is meant to be the top-level constructor.
  -- Primitive datatypes are here viewed as potentially infinite sets of
  -- values (i.e., constructors).
  --
  toConstr   :: a -> Constr


  -- | Building a term from a constructor
  fromConstr   :: Constr -> a


  -- | Provide access to list of all constructors
  dataTypeOf  :: a -> DataType


------------------------------------------------------------------------------
--
--	Typical generic maps defined in terms of gfoldl
--
------------------------------------------------------------------------------

{-

The combinators gmapT, gmapQ, gmapM, ... can all be defined in terms
of gfoldl. We provide corresponding default definitions leaving open
the opportunity to provide datatype-specific definitions.

(The inclusion of the gmap combinators as members of class Data allows
the programmer or the compiler to derive specialised, and maybe more
efficient code per datatype. Note: gfoldl is more higher-order than
the gmap combinators. This is subject to ongoing benchmarking
experiments. It might turn out that the gmap combinators will be moved
out of the class Data.)

Conceptually, the definition of the gmap combinators in terms of the
primitive gfoldl requires the identification of the gfoldl function
arguments. Technically, we also need to identify the type constructor
"c" for the construction of the result type from the folded term type.

-}


  -- | A generic transformation that maps over the immediate subterms
  gmapT :: (forall b. Data b => b -> b) -> a -> a

  -- Use an identity datatype constructor ID (see below)
  -- to instantiate the type constructor c in the type of gfoldl,
  -- and perform injections ID and projections unID accordingly.
  --
  gmapT f x = unID (gfoldl k ID x)
    where
      k (ID c) x = ID (c (f x))


  -- | A generic query with a left-associative binary operator
  gmapQl :: (r -> r' -> r) -> r -> (forall a. Data a => a -> r') -> a -> r
  gmapQl o r f = unCONST . gfoldl k z
    where
      k c x = CONST $ (unCONST c) `o` f x 
      z _   = CONST r

{-

In the definition of gmapQ? combinators, we use phantom type
constructors for the "c" in the type of "gfoldl" because the result
type of a query does not involve the (polymorphic) type of the term
argument. In the definition of gmapQl we simply use the plain constant
type constructor because gfoldl is left-associative anyway and so it
is readily suited to fold a left-associative binary operation over the
immediate subterms. In the definition of gmapQr, extra effort is
needed. We use a higher-order accumulation trick to mediate between
left-associative constructor application vs. right-associative binary
operation (e.g., (:)). When the query is meant to compute a value of
type r, then the result type withing generic folding is r -> r. So the
result of folding is a function to which we finally pass the right
unit.

-}

  -- | A generic query with a right-associative binary operator
  gmapQr :: (r' -> r -> r) -> r -> (forall a. Data a => a -> r') -> a -> r
  gmapQr o r f x = unQr (gfoldl k (const (Qr id)) x) r
    where
      k (Qr c) x = Qr (\r -> c (f x `o` r))

  -- | A generic query that processes the immediate subterms and returns a list
  gmapQ :: (forall a. Data a => a -> u) -> a -> [u]
  gmapQ f = gmapQr (:) [] f


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
  gmapMp :: MonadPlus m => (forall a. Data a => a -> m a) -> a -> m a

{-

The type constructor that we use here simply keeps track of the fact
if we already succeeded for an immediate subterm; see Mp below. To
this end, we couple the monadic computation with a Boolean.

-}

  gmapMp f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) x
        = Mp ( c >>= \(h,b) -> 
                 (f x >>= \x' -> return (h x',True))
                 `mplus` return (h x,b)
             )

  -- | Transformation of one immediate subterm with success
  gmapMo :: MonadPlus m => (forall a. Data a => a -> m a) -> a -> m a

{-

We use the same pairing trick as for gmapMp, 
i.e., we use an extra Bool component to keep track of the 
fact whether an immediate subterm was processed successfully.
However, we cut of mapping over subterms once a first subterm
was transformed successfully.

-}

  gmapMo f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) x
        = Mp ( c >>= \(h,b) -> if b 
                        then return (h x,b)
                        else (f x >>= \x' -> return (h x',True))
                             `mplus` return (h x,b)
             )


-- | The identity type constructor needed for the definition of gmapT
newtype ID x = ID { unID :: x }


-- | The constant type constructor needed for the definition of gmapQl
newtype CONST c a = CONST { unCONST :: c }


-- | The type constructor used in definition of gmapQr
newtype Qr r a = Qr { unQr  :: r -> r }


-- | The type constructor used in definition of gmapMp
newtype Mp m x = Mp { unMp :: m (x, Bool) }



------------------------------------------------------------------------------
--
--	Constructor representations
--
------------------------------------------------------------------------------


-- | Representation of constructors
data Constr =
	-- The prime case for proper datatype constructors
	       DataConstr ConIndex String Fixity

	-- Provision for built-in types
	    | IntConstr     Int
	    | IntegerConstr Integer
	    | FloatConstr   Float
	    | CharConstr    Char

	-- Provision for any type that can be read/shown as string
	    | StringConstr  String

	-- Provision for function types
	    | FunConstr

              deriving (Show, Typeable)

-- 
-- Equality of datatype constructors via index.
-- Use designated equalities for primitive types.
-- 
instance Eq Constr where
  (DataConstr i1 _ _) == (DataConstr i2 _ _) = i1 == i2
  (IntConstr i1)      == (IntConstr i2)      = i1 == i2
  (IntegerConstr i1)  == (IntegerConstr i2)  = i1 == i2
  (FloatConstr i1)    == (FloatConstr i2)    = i1 == i2
  (CharConstr i1)     == (CharConstr i2)     = i1 == i2
  (StringConstr i1)   == (StringConstr i2)   = i1 == i2
  _ == _ = False


-- | Unique index for datatype constructors.
--   Textual order is respected. Starts at 1.
--
type ConIndex = Int


-- | Fixity of constructors
data Fixity = Prefix
            | Infix	-- Later: add associativity and precedence
	    deriving (Eq,Show)

-- | A package of constructor representations;
--   could be a list, an array, a balanced tree, or others.
--
data DataType =
	-- The prime case for algebraic datatypes
	       DataType [Constr]

	-- Provision for built-in types
	    | IntType
	    | IntegerType
	    | FloatType
	    | CharType

	-- Provision for any type that can be read/shown as string
	    | StringType

	-- Provision for function types
	    | FunType

              deriving Show


------------------------------------------------------------------------------
--
--	Constructing constructor representations
--
------------------------------------------------------------------------------


-- | Make a representation for a datatype constructor
mkConstr   :: ConIndex -> String -> Fixity -> Constr
--	ToDo: consider adding arity?
mkConstr = DataConstr

-- | Make a package of constructor representations
mkDataType :: [Constr] -> DataType
mkDataType = DataType


------------------------------------------------------------------------------
--
--	Observing constructor representations
--
------------------------------------------------------------------------------


-- | Turn a constructor into a string
conString :: Constr -> String
conString (DataConstr _ str _) = str
conString (IntConstr int)      = show int
conString (IntegerConstr int)  = show int
conString (FloatConstr real)   = show real
conString (CharConstr char)    = show char
conString (StringConstr str)   = show str
conString FunConstr            = "->"


-- | Determine fixity of a constructor;
--   undefined for primitive types.
conFixity :: Constr -> Fixity
conFixity (DataConstr _ _ fix) = fix
conFixity _                    = undefined


-- | Determine index of a constructor.
--   Undefined for primitive types.
conIndex   :: Constr -> ConIndex
conIndex (DataConstr idx _ _) = idx
conIndex _                    = undefined


-- | Lookup a constructor via a string
stringCon :: DataType -> String -> Maybe Constr
stringCon (DataType cs) str = worker cs
  where
    worker []     = Nothing
    worker (c:cs) =
      case c of
        (DataConstr _ str' _) -> if str == str'
                                   then Just c
                                   else worker cs
        _ -> undefined -- other forms of Constr not valid here

stringCon IntType str       = Just . IntConstr     $ read str
stringCon IntegerType str   = Just . IntegerConstr $ read str
stringCon FloatType str     = Just . FloatConstr   $ read str
stringCon CharType str      = Just . CharConstr    $ read str
stringCon StringType str    = Just . StringConstr  $ read str
stringCon FunType str       = Just FunConstr


-- | Lookup a constructor by its index;
---  not defined for primitive types.
indexCon :: DataType -> ConIndex -> Constr
indexCon (DataType cs) idx = cs !! (idx-1)
indexCon _ _ = undefined -- otherwise


-- | Return maximum index;
--   0 for primitive types
maxConIndex :: DataType -> ConIndex
maxConIndex (DataType cs) = length cs
maxConIndex _ = 0 -- otherwise


-- | Return all constructors in increasing order of indicies;
-- empty list for primitive types
dataTypeCons :: DataType -> [Constr] 
dataTypeCons (DataType cs) = cs
dataTypeCons _ = [] -- otherwise


------------------------------------------------------------------------------
--
--	Instances of the Data class for Prelude types
--
------------------------------------------------------------------------------

-- Basic datatype Int; folding and unfolding is trivial
instance Data Int where
  toConstr x = IntConstr x
  fromConstr (IntConstr x) = x
  dataTypeOf _ = IntType

-- Another basic datatype instance
instance Data Integer where
  toConstr x = IntegerConstr x
  fromConstr (IntegerConstr x) = x
  dataTypeOf _ = IntegerType

-- Another basic datatype instance
instance Data Float where
  toConstr x = FloatConstr x
  fromConstr (FloatConstr x) = x
  dataTypeOf _ = FloatType

-- Another basic datatype instance
instance Data Char where
  toConstr x = CharConstr x
  fromConstr (CharConstr x) = x
  dataTypeOf _ = CharType

-- A basic datatype without a specific branch in Constr
instance Data Rational where
  toConstr x = StringConstr (show x)
  fromConstr (StringConstr x) = read x
  dataTypeOf _ = StringType

--
-- Bool as the most trivial algebraic datatype;
-- define top-level definitions for representations.
--

falseConstr  = mkConstr 1 "False" Prefix
trueConstr   = mkConstr 2 "True"  Prefix
boolDataType = mkDataType [falseConstr,trueConstr]

instance Data Bool where
  toConstr False = falseConstr
  toConstr True  = trueConstr
  fromConstr c = case conIndex c of
                   1 -> False
                   2 -> True
  dataTypeOf _ = boolDataType


--
-- Lists as an example of a polymorphic algebraic datatype.
-- Cons-lists are terms with two immediate subterms.
--

nilConstr    = mkConstr 1 "[]"  Prefix
consConstr   = mkConstr 2 "(:)" Infix
listDataType = mkDataType [nilConstr,consConstr]

instance Data a => Data [a] where
  gfoldl f z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  toConstr []    = nilConstr
  toConstr (_:_) = consConstr
  fromConstr c = case conIndex c of
                   1 -> []
                   2 -> undefined:undefined
  dataTypeOf _ = listDataType

--
-- The gmaps are given as an illustration.
-- This shows that the gmaps for lists are different from list maps.
--
  gmapT  f   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  f   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  f   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')


--
-- Yet another polymorphic datatype constructor
-- No surprises.
--

nothingConstr = mkConstr 1 "Nothing" Prefix
justConstr    = mkConstr 2 "Just"    Prefix
maybeDataType = mkDataType [nothingConstr,justConstr]

instance Data a => Data (Maybe a) where
  gfoldl f z Nothing  = z Nothing
  gfoldl f z (Just x) = z Just `f` x
  toConstr Nothing  = nothingConstr
  toConstr (Just _) = justConstr
  fromConstr c = case conIndex c of
                   1 -> Nothing
                   2 -> Just undefined
  dataTypeOf _ = maybeDataType

--
-- Yet another polymorphic datatype constructor.
-- No surprises.
--

pairConstr = mkConstr 1 "(,)" Infix
productDataType = mkDataType [pairConstr]

instance (Data a, Data b) => Data (a,b) where
  gfoldl f z (a,b) = z (,) `f` a `f` b
  toConstr _ = pairConstr
  fromConstr c = case conIndex c of
                   1 -> (undefined,undefined)
  dataTypeOf _ = productDataType

--
-- Yet another polymorphic datatype constructor.
-- No surprises.
--


leftConstr     = mkConstr 1 "Left"  Prefix
rightConstr    = mkConstr 2 "Right" Prefix
eitherDataType = mkDataType [leftConstr,rightConstr]

instance (Data a, Data b) => Data (Either a b) where
  gfoldl f z (Left a)   = z Left  `f` a
  gfoldl f z (Right a)  = z Right `f` a
  toConstr (Left _)  = leftConstr
  toConstr (Right _) = rightConstr
  fromConstr c = case conIndex c of
                   1 -> Left undefined
                   2 -> Right undefined
  dataTypeOf _ = eitherDataType


{-

We should better not FOLD over characters in a string for efficiency.
However, the following instance would clearly overlap with the
instance for polymorphic lists. Given the current scheme of allowing
overlapping instances, this would imply that ANY module that imports
Data.Generics would need to explicitly and generally allow overlapping
instances. This is prohibitive and calls for a more constrained model
of allowing overlapping instances. The present instance would be
sensible even more for UNFOLDING. In the definition of "gread"
(generic read --- based on unfolding), we succeed handling strings in a
special way by using a type-specific case for String.

instance Data String where
  toConstr x = StringConstr x
  fromConstr (StringConstr x) = x
  dataTypeOf _ = StringType

-}

-- A last resort for functions
instance (Typeable a, Typeable b) => Data (a -> b) where
  toConstr _   = FunConstr
  fromConstr _ = undefined
  dataTypeOf _ = FunType
