-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Basics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
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

	-- * Module Data.Typeable re-exported for convenience
	module Data.Typeable,

	-- * The Data class for processing constructor applications
	Data( 
		gfoldl,		-- :: ... -> a -> c a
		gunfold,	-- :: ... -> Constr -> c a
		toConstr, 	-- :: a -> Constr
		dataTypeOf,	-- :: a -> DataType
		dataCast1,	-- mediate types and unary type constructors
		dataCast2	-- mediate types and binary type constructors
            ),

	-- * Datatype representations
	DataType,	-- abstract, instance of: Show
	Constr,		-- abstract, instance of: Eq, Show
	DataRep(..),	-- instance of: Eq, Show
	ConstrRep(..),	-- instance of: Eq, Show
	ConIndex,	-- alias for Int, start at 1
	Fixity(..),	-- instance of: Eq, Show

	-- * Observers for datatype representations
	dataTypeName,	-- :: DataType -> String
	dataTypeRep,	-- :: DataType -> DataRep
 	constrType,	-- :: Constr   -> DataType
 	constrRep,	-- :: Constr   -> ConstrRep
 	repConstr,		-- :: DataType -> ConstrRep -> Constr

	-- * Representations of algebraic data types
	mkDataType,	-- :: String   -> [Constr] -> DataType
 	mkConstr,	-- :: DataType -> String -> Fixity -> Constr
	dataTypeConstrs,-- :: DataType -> [Constr]
 	constrFields,	-- :: Constr   -> [String]
 	constrFixity,	-- :: Constr   -> Fixity

	-- * From strings to constr's and vice versa: all data types
	showConstr, 	-- :: Constr   -> String
	readConstr,	-- :: DataType -> String -> Maybe Constr

	-- * Convenience funtions: algebraic data types
	isAlgType,	-- :: DataType -> Bool
	indexConstr,	-- :: DataType -> ConIndex -> Constr
	constrIndex,	-- :: Constr   -> ConIndex
	maxConstrIndex,	-- :: DataType -> ConIndex

	-- * Representation of primitive types
	mkIntType,	-- :: String -> DataType
	mkFloatType,	-- :: String -> DataType
	mkStringType,	-- :: String -> DataType
	mkIntConstr,	-- :: DataType -> Integer -> Constr
	mkFloatConstr,	-- :: DataType -> Double  -> Constr
	mkStringConstr,	-- :: DataType -> String  -> Constr

	-- * Non-representations for non-presentable types
	mkNorepType,	-- :: String -> DataType
	isNorepType,	-- :: DataType -> Bool

	-- * Convenience functions: take type constructors apart
	tyconUQname,	-- :: String -> String
	tyconModule,	-- :: String -> String

        -- * Generic maps defined in terms of gfoldl 
	gmapT,
        gmapQ, 
        gmapQl,
        gmapQr,
        gmapQi,
        gmapM,
        gmapMp,
        gmapMo,

	-- * Generic operation(s) defined in terms of gunfold
        fromConstr,	-- :: Constr -> a
        fromConstrB,	-- :: ... -> Constr -> a
	fromConstrM	-- :: Monad m => ... -> Constr -> m a

  ) where


------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif

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

The combinator takes two arguments "k" and "z" to fold over a term
"x".  The result type is defined in terms of "x" but variability is
achieved by means of type constructor "c" for the construction of the
actual result type. The purpose of the argument "z" is to define how
the empty constructor application is folded. So "z" is like the
neutral / start element for list folding. The purpose of the argument
"k" is to define how the nonempty constructor application is
folded. That is, "k" takes the folded "tail" of the constructor
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

  -- | Unfolding constructor applications
  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  -- | Obtaining the constructor from a given datum.
  -- For proper terms, this is meant to be the top-level constructor.
  -- Primitive datatypes are here viewed as potentially infinite sets of
  -- values (i.e., constructors).
  --
  toConstr   :: a -> Constr


  -- | Provide access to list of all constructors
  dataTypeOf  :: a -> DataType



------------------------------------------------------------------------------
--
-- Mediate types and type constructors
--
------------------------------------------------------------------------------

  -- | Mediate types and unary type constructors
  dataCast1 :: Typeable1 t
            => (forall a. Data a => c (t a))
            -> Maybe (c a)
  dataCast1 _ = Nothing

  -- | Mediate types and binary type constructors
  dataCast2 :: Typeable2 t
            => (forall a b. (Data a, Data b) => c (t a b))
            -> Maybe (c a)
  dataCast2 _ = Nothing



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


  -- | A generic query that processes one child by index (zero-based)
  gmapQi :: Int -> (forall a. Data a => a -> u) -> a -> u
  gmapQi i f x = case gfoldl k z x of { Qi _ q -> fromJust q } 
    where
      k (Qi i' q) a = Qi (i'+1) (if i==i' then Just (f a) else q) 
      z f           = Qi 0 Nothing


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


-- | Type constructor for adding counters to queries
data Qi q a = Qi Int (Maybe q)


-- | The type constructor used in definition of gmapQr
newtype Qr r a = Qr { unQr  :: r -> r }


-- | The type constructor used in definition of gmapMp
newtype Mp m x = Mp { unMp :: m (x, Bool) }



------------------------------------------------------------------------------
--
--	Generic unfolding
--
------------------------------------------------------------------------------


-- | Build a term skeleton
fromConstr :: Data a => Constr -> a
fromConstr = fromConstrB undefined


-- | Build a term and use a generic function for subterms
fromConstrB :: Data a
            => (forall a. Data a => a)
            -> Constr
            -> a
fromConstrB f = unID . gunfold k z
 where
  k c = ID (unID c f)
  z = ID


-- | Monadic variation on \"fromConstrB\"
fromConstrM :: (Monad m, Data a)
            => (forall a. Data a => m a)
            -> Constr
            -> m a
fromConstrM f = gunfold k z 
 where
  k c = do { c' <- c; b <- f; return (c' b) }
  z = return



------------------------------------------------------------------------------
--
--	Datatype and constructor representations
--
------------------------------------------------------------------------------


--
-- | Representation of datatypes.
-- | A package of constructor representations with names of type and module.
-- | The list of constructors could be an array, a balanced tree, or others.
--
data DataType = DataType
			{ tycon   :: String
			, datarep :: DataRep
			}

              deriving Show


-- | Representation of constructors
data Constr = Constr
			{ conrep    :: ConstrRep
			, constring :: String
			, confields :: [String]	-- for AlgRep only
			, confixity :: Fixity	-- for AlgRep only
			, datatype  :: DataType
			}

instance Show Constr where
 show = constring


-- | Equality of constructors
instance Eq Constr where
  c == c' = constrRep c == constrRep c'


-- | Public representation of datatypes
data DataRep = AlgRep [Constr]
             | IntRep
	     | FloatRep
	     | StringRep
             | NoRep

	    deriving (Eq,Show)


-- | Public representation of constructors
data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
    	       | FloatConstr  Double
    	       | StringConstr String

	       deriving (Eq,Show)


--
-- | Unique index for datatype constructors.
-- | Textual order is respected. Starts at 1.
--
type ConIndex = Int


-- | Fixity of constructors
data Fixity = Prefix
            | Infix	-- Later: add associativity and precedence

	    deriving (Eq,Show)


------------------------------------------------------------------------------
--
--	Observers for datatype representations
--
------------------------------------------------------------------------------


-- | Gets the type constructor including the module
dataTypeName :: DataType -> String
dataTypeName = tycon



-- | Gets the public presentation of datatypes
dataTypeRep :: DataType -> DataRep
dataTypeRep = datarep


-- | Gets the datatype of a constructor
constrType :: Constr -> DataType
constrType = datatype


-- | Gets the public presentation of constructors
constrRep :: Constr -> ConstrRep
constrRep = conrep


-- | Look up a constructor by its representation
repConstr :: DataType -> ConstrRep -> Constr
repConstr dt cr =
      case (dataTypeRep dt, cr) of
	(AlgRep cs, AlgConstr i)      -> cs !! (i-1)
	(IntRep,    IntConstr i)      -> mkIntConstr dt i
	(FloatRep,  FloatConstr f)    -> mkFloatConstr dt f
	(StringRep, StringConstr str) -> mkStringConstr dt str
	_ -> error "repConstr"



------------------------------------------------------------------------------
--
--	Representations of algebraic data types
--
------------------------------------------------------------------------------


-- | Constructs an algebraic datatype
mkDataType :: String -> [Constr] -> DataType
mkDataType str cs = DataType
			{ tycon   = str
			, datarep = AlgRep cs
			}


-- | Constructs a constructor
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dt str fields fix =
	Constr
		{ conrep    = AlgConstr idx
		, constring = str
		, confields = fields
		, confixity = fix
		, datatype  = dt 
		}
  where
    idx = head [ i | (c,i) <- dataTypeConstrs dt `zip` [1..],
                     showConstr c == str ]


-- | Gets the constructors
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dt = case datarep dt of 
			(AlgRep cons) -> cons
			_ -> error "dataTypeConstrs"


-- | Gets the field labels of a constructor
constrFields :: Constr -> [String]
constrFields = confields


-- | Gets the fixity of a constructor
constrFixity :: Constr -> Fixity
constrFixity = confixity



------------------------------------------------------------------------------
--
--	From strings to constr's and vice versa: all data types
--	
------------------------------------------------------------------------------


-- | Gets the string for a constructor
showConstr :: Constr -> String
showConstr = constring


-- | Lookup a constructor via a string
readConstr :: DataType -> String -> Maybe Constr
readConstr dt str =
      case dataTypeRep dt of
	AlgRep cons -> idx cons
	IntRep      -> mkReadCon (\i -> (mkPrimCon dt str (IntConstr i)))
	FloatRep    -> mkReadCon (\f -> (mkPrimCon dt str (FloatConstr f)))
	StringRep   -> Just (mkStringConstr dt str)
        NoRep       -> Nothing
  where

    -- Read a value and build a constructor
    mkReadCon :: Read t => (t -> Constr) -> Maybe Constr
    mkReadCon f = case (reads str) of
		    [(t,"")] -> Just (f t)
		    _ -> Nothing

    -- Traverse list of algebraic datatype constructors
    idx :: [Constr] -> Maybe Constr
    idx cons = let fit = filter ((==) str . showConstr) cons 
                in if fit == []
                     then Nothing
                     else Just (head fit)


------------------------------------------------------------------------------
--
--	Convenience funtions: algebraic data types
--
------------------------------------------------------------------------------


-- | Test for an algebraic type
isAlgType :: DataType -> Bool
isAlgType dt = case datarep dt of
		 (AlgRep _) -> True
		 _ -> False 


-- | Gets the constructor for an index
indexConstr :: DataType -> ConIndex -> Constr
indexConstr dt idx = case datarep dt of
			(AlgRep cs) -> cs !! (idx-1)
			_           -> error "indexConstr"


-- | Gets the index of a constructor
constrIndex :: Constr -> ConIndex
constrIndex con = case constrRep con of
                    (AlgConstr idx) -> idx
 		    _ -> error "constrIndex"


-- | Gets the maximum constructor index
maxConstrIndex :: DataType -> ConIndex
maxConstrIndex dt = case dataTypeRep dt of
			AlgRep cs -> length cs
			_ 	     -> error "maxConstrIndex"



------------------------------------------------------------------------------
--
--	Representation of primitive types
--
------------------------------------------------------------------------------


-- | Constructs the Int type
mkIntType :: String -> DataType
mkIntType = mkPrimType IntRep


-- | Constructs the Float type
mkFloatType :: String -> DataType
mkFloatType = mkPrimType FloatRep


-- | Constructs the String type
mkStringType :: String -> DataType
mkStringType = mkPrimType StringRep


-- | Helper for mkIntType, mkFloatType, mkStringType
mkPrimType :: DataRep -> String -> DataType
mkPrimType dr str = DataType
			{ tycon   = str
			, datarep = dr
			}


-- Makes a constructor for primitive types
mkPrimCon :: DataType -> String -> ConstrRep -> Constr
mkPrimCon dt str cr = Constr 
			{ datatype  = dt
			, conrep    = cr
			, constring = str
			, confields = error "constrFields"
			, confixity = error "constrFixity"
			}


mkIntConstr :: DataType -> Integer -> Constr
mkIntConstr dt i = case datarep dt of
		  IntRep -> mkPrimCon dt (show i) (IntConstr i)
		  _ -> error "mkIntConstr"


mkFloatConstr :: DataType -> Double -> Constr
mkFloatConstr dt f = case datarep dt of
		    FloatRep -> mkPrimCon dt (show f) (FloatConstr f)
		    _ -> error "mkFloatConstr"


mkStringConstr :: DataType -> String -> Constr
mkStringConstr dt str = case datarep dt of
		       StringRep -> mkPrimCon dt str (StringConstr str)
		       _ -> error "mkStringConstr"


------------------------------------------------------------------------------
--
--	Non-representations for non-presentable types
--
------------------------------------------------------------------------------


-- | Constructs a non-representation
mkNorepType :: String -> DataType
mkNorepType str = DataType
			{ tycon   = str
			, datarep = NoRep
			}


-- | Test for a non-representable type
isNorepType :: DataType -> Bool
isNorepType dt = case datarep dt of
	 	   NoRep -> True
		   _ -> False 



------------------------------------------------------------------------------
--
--	Convenience for qualified type constructors
--
------------------------------------------------------------------------------


-- | Gets the unqualified type constructor
-- Drop *.*.*... before name
--
tyconUQname :: String -> String
tyconUQname x = let x' = dropWhile (not . (==) '.') x
                 in if x' == [] then x else tyconUQname (tail x')


-- | Gets the module of a type constructor
-- Take *.*.*... before name
tyconModule :: String -> String
tyconModule x = let (a,b) = break ((==) '.') x
                 in if b == ""
                      then b 
                      else a ++ tyconModule' (tail b)
  where
    tyconModule' x = let x' = tyconModule x
                      in if x' == "" then "" else ('.':x')
