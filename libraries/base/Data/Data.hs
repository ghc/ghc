{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Data
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell.  See
-- <http://www.haskell.org/haskellwiki/Research_papers/Generics#Scrap_your_boilerplate.21>.
-- This module provides the 'Data' class with its primitives for
-- generic programming, along with instances for many datatypes. It
-- corresponds to a merge between the previous "Data.Generics.Basics"
-- and almost all of "Data.Generics.Instances". The instances that are
-- not present in this module were moved to the
-- @Data.Generics.Instances@ module in the @syb@ package.
--
-- For more information, please visit the new
-- SYB wiki: <http://www.cs.uu.nl/wiki/bin/view/GenericProgramming/SYB>.
--
-----------------------------------------------------------------------------

module Data.Data (

        -- * Module Data.Typeable re-exported for convenience
        module Data.Typeable,

        -- * The Data class for processing constructor applications
        Data(
                gfoldl,
                gunfold,
                toConstr,
                dataTypeOf,
                dataCast1,      -- mediate types and unary type constructors
                dataCast2,      -- mediate types and binary type constructors
                -- Generic maps defined in terms of gfoldl
                gmapT,
                gmapQ,
                gmapQl,
                gmapQr,
                gmapQi,
                gmapM,
                gmapMp,
                gmapMo
            ),

        -- * Datatype representations
        DataType,       -- abstract
        -- ** Constructors
        mkDataType,
        mkIntType,
        mkFloatType,
        mkCharType,
        mkNoRepType,
        -- ** Observers
        dataTypeName,
        DataRep(..),
        dataTypeRep,
        -- ** Convenience functions
        repConstr,
        isAlgType,
        dataTypeConstrs,
        indexConstr,
        maxConstrIndex,
        isNorepType,

        -- * Data constructor representations
        Constr,         -- abstract
        ConIndex,       -- alias for Int, start at 1
        Fixity(..),
        -- ** Constructors
        mkConstr,
        mkConstrTag,
        mkIntegralConstr,
        mkRealConstr,
        mkCharConstr,
        -- ** Observers
        constrType,
        ConstrRep(..),
        constrRep,
        constrFields,
        constrFixity,
        -- ** Convenience function: algebraic data types
        constrIndex,
        -- ** From strings to constructors and vice versa: all data types
        showConstr,
        readConstr,

        -- * Convenience functions: take type constructors apart
        tyconUQname,
        tyconModule,

        -- * Generic operations defined in terms of 'gunfold'
        fromConstr,
        fromConstrB,
        fromConstrM

  ) where


------------------------------------------------------------------------------

import Data.Functor.Const
import Data.Either
import Data.Eq
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.List (findIndex)
import Data.Typeable
import Data.Version( Version(..) )
import GHC.Base hiding (Any, IntRep, FloatRep)
import GHC.List
import GHC.Num
import GHC.Read
import GHC.Show
import GHC.Tuple (Solo (..))
import Text.Read( reads )

-- Imports for the instances
import Control.Applicative (WrappedArrow(..), WrappedMonad(..), ZipList(..))
       -- So we can give them Data instances
import Data.Functor.Identity -- So we can give Data instance for Identity
import Data.Int              -- So we can give Data instance for Int8, ...
import Data.Type.Coercion
import Data.Word             -- So we can give Data instance for Word8, ...
import GHC.Real              -- So we can give Data instance for Ratio
--import GHC.IOBase            -- So we can give Data instance for IO, Handle
import GHC.Ptr               -- So we can give Data instance for Ptr
import GHC.ForeignPtr        -- So we can give Data instance for ForeignPtr
import Foreign.Ptr (IntPtr(..), WordPtr(..))
                             -- So we can give Data instance for IntPtr and WordPtr
--import GHC.Stable            -- So we can give Data instance for StablePtr
--import GHC.ST                -- So we can give Data instance for ST
--import GHC.Conc              -- So we can give Data instance for MVar & Co.
import GHC.Arr               -- So we can give Data instance for Array
import qualified GHC.Generics as Generics (Fixity(..))
import GHC.Generics hiding (Fixity(..))
                             -- So we can give Data instance for U1, V1, ...

------------------------------------------------------------------------------
--
--      The Data class
--
------------------------------------------------------------------------------

{- |
The 'Data' class comprehends a fundamental primitive 'gfoldl' for
folding over constructor applications, say terms. This primitive can
be instantiated in several ways to map over the immediate subterms
of a term; see the @gmap@ combinators later in this class.  Indeed, a
generic programmer does not necessarily need to use the ingenious gfoldl
primitive but rather the intuitive @gmap@ combinators.  The 'gfoldl'
primitive is completed by means to query top-level constructors, to
turn constructor representations into proper terms, and to list all
possible datatype constructors.  This completion allows us to serve
generic programming scenarios like read, show, equality, term generation.

The combinators 'gmapT', 'gmapQ', 'gmapM', etc are all provided with
default definitions in terms of 'gfoldl', leaving open the opportunity
to provide datatype-specific definitions.
(The inclusion of the @gmap@ combinators as members of class 'Data'
allows the programmer or the compiler to derive specialised, and maybe
more efficient code per datatype.  /Note/: 'gfoldl' is more higher-order
than the @gmap@ combinators.  This is subject to ongoing benchmarking
experiments.  It might turn out that the @gmap@ combinators will be
moved out of the class 'Data'.)

Conceptually, the definition of the @gmap@ combinators in terms of the
primitive 'gfoldl' requires the identification of the 'gfoldl' function
arguments.  Technically, we also need to identify the type constructor
@c@ for the construction of the result type from the folded term type.

In the definition of @gmapQ@/x/ combinators, we use phantom type
constructors for the @c@ in the type of 'gfoldl' because the result type
of a query does not involve the (polymorphic) type of the term argument.
In the definition of 'gmapQl' we simply use the plain constant type
constructor because 'gfoldl' is left-associative anyway and so it is
readily suited to fold a left-associative binary operation over the
immediate subterms.  In the definition of gmapQr, extra effort is
needed. We use a higher-order accumulation trick to mediate between
left-associative constructor application vs. right-associative binary
operation (e.g., @(:)@).  When the query is meant to compute a value
of type @r@, then the result type within generic folding is @r -> r@.
So the result of folding is a function to which we finally pass the
right unit.

With the @-XDeriveDataTypeable@ option, GHC can generate instances of the
'Data' class automatically.  For example, given the declaration

> data T a b = C1 a b | C2 deriving (Typeable, Data)

GHC will generate an instance that is equivalent to

> instance (Data a, Data b) => Data (T a b) where
>     gfoldl k z (C1 a b) = z C1 `k` a `k` b
>     gfoldl k z C2       = z C2
>
>     gunfold k z c = case constrIndex c of
>                         1 -> k (k (z C1))
>                         2 -> z C2
>
>     toConstr (C1 _ _) = con_C1
>     toConstr C2       = con_C2
>
>     dataTypeOf _ = ty_T
>
> con_C1 = mkConstr ty_T "C1" [] Prefix
> con_C2 = mkConstr ty_T "C2" [] Prefix
> ty_T   = mkDataType "Module.T" [con_C1, con_C2]

This is suitable for datatypes that are exported transparently.

-}

class Typeable a => Data a where

  -- | Left-associative fold operation for constructor applications.
  --
  -- The type of 'gfoldl' is a headache, but operationally it is a simple
  -- generalisation of a list fold.
  --
  -- The default definition for 'gfoldl' is @'const' 'id'@, which is
  -- suitable for abstract datatypes with no substructures.
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
                -- ^ defines how nonempty constructor applications are
                -- folded.  It takes the folded tail of the constructor
                -- application and its head, i.e., an immediate subterm,
                -- and combines them in some way.
          -> (forall g. g -> c g)
                -- ^ defines how the empty constructor application is
                -- folded, like the neutral \/ start element for list
                -- folding.
          -> a
                -- ^ structure to be folded.
          -> c a
                -- ^ result, with a type defined in terms of @a@, but
                -- variability is achieved by means of type constructor
                -- @c@ for the construction of the actual result type.

  -- See the 'Data' instances in this file for an illustration of 'gfoldl'.

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
  toConstr   :: a -> Constr


  -- | The outer type constructor of the type
  dataTypeOf  :: a -> DataType



------------------------------------------------------------------------------
--
-- Mediate types and type constructors
--
------------------------------------------------------------------------------

  -- | Mediate types and unary type constructors.
  --
  -- In 'Data' instances of the form
  --
  -- @
  --     instance (Data a, ...) => Data (T a)
  -- @
  --
  -- 'dataCast1' should be defined as 'gcast1'.
  --
  -- The default definition is @'const' 'Nothing'@, which is appropriate
  -- for instances of other forms.
  dataCast1 :: Typeable t
            => (forall d. Data d => c (t d))
            -> Maybe (c a)
  dataCast1 _ = Nothing

  -- | Mediate types and binary type constructors.
  --
  -- In 'Data' instances of the form
  --
  -- @
  --     instance (Data a, Data b, ...) => Data (T a b)
  -- @
  --
  -- 'dataCast2' should be defined as 'gcast2'.
  --
  -- The default definition is @'const' 'Nothing'@, which is appropriate
  -- for instances of other forms.
  dataCast2 :: Typeable t
            => (forall d e. (Data d, Data e) => c (t d e))
            -> Maybe (c a)
  dataCast2 _ = Nothing



------------------------------------------------------------------------------
--
--      Typical generic maps defined in terms of gfoldl
--
------------------------------------------------------------------------------


  -- | A generic transformation that maps over the immediate subterms
  --
  -- The default definition instantiates the type constructor @c@ in the
  -- type of 'gfoldl' to an identity datatype constructor, using the
  -- isomorphism pair as injection and projection.
  gmapT :: (forall b. Data b => b -> b) -> a -> a

  -- Use the Identity datatype constructor
  -- to instantiate the type constructor c in the type of gfoldl,
  -- and perform injections Identity and projections runIdentity accordingly.
  --
  gmapT f x0 = runIdentity (gfoldl k Identity x0)
    where
      k :: Data d => Identity (d->b) -> d -> Identity b
      k (Identity c) x = Identity (c (f x))


  -- | A generic query with a left-associative binary operator
  gmapQl :: forall r r'. (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQl o r f = getConst . gfoldl k z
    where
      k :: Data d => Const r (d->b) -> d -> Const r b
      k c x = Const $ (getConst c) `o` f x
      z :: g -> Const r g
      z _   = Const r

  -- | A generic query with a right-associative binary operator
  gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQr o r0 f x0 = unQr (gfoldl k (const (Qr id)) x0) r0
    where
      k :: Data d => Qr r (d->b) -> d -> Qr r b
      k (Qr c) x = Qr (\r -> c (f x `o` r))


  -- | A generic query that processes the immediate subterms and returns a list
  -- of results.  The list is given in the same order as originally specified
  -- in the declaration of the data constructors.
  gmapQ :: (forall d. Data d => d -> u) -> a -> [u]
  gmapQ f = gmapQr (:) [] f


  -- | A generic query that processes one child by index (zero-based)
  gmapQi :: forall u. Int -> (forall d. Data d => d -> u) -> a -> u
  gmapQi i f x = case gfoldl k z x of { Qi _ q -> fromJust q }
    where
      k :: Data d => Qi u (d -> b) -> d -> Qi u b
      k (Qi i' q) a = Qi (i'+1) (if i==i' then Just (f a) else q)
      z :: g -> Qi q g
      z _           = Qi 0 Nothing


  -- | A generic monadic transformation that maps over the immediate subterms
  --
  -- The default definition instantiates the type constructor @c@ in
  -- the type of 'gfoldl' to the monad datatype constructor, defining
  -- injection and projection using 'return' and '>>='.
  gmapM :: forall m. Monad m => (forall d. Data d => d -> m d) -> a -> m a

  -- Use immediately the monad datatype constructor
  -- to instantiate the type constructor c in the type of gfoldl,
  -- so injection and projection is done by return and >>=.
  --
  gmapM f = gfoldl k return
    where
      k :: Data d => m (d -> b) -> d -> m b
      k c x = do c' <- c
                 x' <- f x
                 return (c' x')


  -- | Transformation of at least one immediate subterm does not fail
  gmapMp :: forall m. MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a

{-

The type constructor that we use here simply keeps track of the fact
if we already succeeded for an immediate subterm; see Mp below. To
this end, we couple the monadic computation with a Boolean.

-}

  gmapMp f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z :: g -> Mp m g
      z g = Mp (return (g,False))
      k :: Data d => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y
        = Mp ( c >>= \(h, b) ->
                 (f y >>= \y' -> return (h y', True))
                 `mplus` return (h y, b)
             )

  -- | Transformation of one immediate subterm with success
  gmapMo :: forall m. MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a

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
      z :: g -> Mp m g
      z g = Mp (return (g,False))
      k :: Data d => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y
        = Mp ( c >>= \(h,b) -> if b
                        then return (h y, b)
                        else (f y >>= \y' -> return (h y',True))
                             `mplus` return (h y, b)
             )


-- | Type constructor for adding counters to queries
data Qi q a = Qi Int (Maybe q)


-- | The type constructor used in definition of gmapQr
newtype Qr r a = Qr { unQr  :: r -> r }


-- | The type constructor used in definition of gmapMp
newtype Mp m x = Mp { unMp :: m (x, Bool) }



------------------------------------------------------------------------------
--
--      Generic unfolding
--
------------------------------------------------------------------------------


-- | Build a term skeleton
fromConstr :: Data a => Constr -> a
fromConstr = fromConstrB (errorWithoutStackTrace "Data.Data.fromConstr")


-- | Build a term and use a generic function for subterms
fromConstrB :: Data a
            => (forall d. Data d => d)
            -> Constr
            -> a
fromConstrB f = runIdentity . gunfold k z
 where
  k :: forall b r. Data b => Identity (b -> r) -> Identity r
  k c = Identity (runIdentity c f)

  z :: forall r. r -> Identity r
  z = Identity


-- | Monadic variation on 'fromConstrB'
fromConstrM :: forall m a. (Monad m, Data a)
            => (forall d. Data d => m d)
            -> Constr
            -> m a
fromConstrM f = gunfold k z
 where
  k :: forall b r. Data b => m (b -> r) -> m r
  k c = do { c' <- c; b <- f; return (c' b) }

  z :: forall r. r -> m r
  z = return



------------------------------------------------------------------------------
--
--      Datatype and constructor representations
--
------------------------------------------------------------------------------


--
-- | Representation of datatypes.
-- A package of constructor representations with names of type and module.
--
data DataType = DataType
                        { tycon   :: String
                        , datarep :: DataRep
                        }

              deriving Show -- ^ @since 4.0.0.0

-- | Representation of constructors. Note that equality on constructors
-- with different types may not work -- i.e. the constructors for 'False' and
-- 'Nothing' may compare equal.
data Constr = Constr
                        { conrep    :: ConstrRep
                        , constring :: String
                        , confields :: [String] -- for AlgRep only
                        , confixity :: Fixity   -- for AlgRep only
                        , datatype  :: DataType
                        }

-- | @since 4.0.0.0
instance Show Constr where
 show = constring


-- | Equality of constructors
--
-- @since 4.0.0.0
instance Eq Constr where
  c == c' = constrRep c == constrRep c'


-- | Public representation of datatypes
data DataRep = AlgRep [Constr]
             | IntRep
             | FloatRep
             | CharRep
             | NoRep

            deriving ( Eq   -- ^ @since 4.0.0.0
                     , Show -- ^ @since 4.0.0.0
                     )
-- The list of constructors could be an array, a balanced tree, or others.


-- | Public representation of constructors
data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
               | FloatConstr  Rational
               | CharConstr   Char

               deriving ( Eq   -- ^ @since 4.0.0.0
                        , Show -- ^ @since 4.0.0.0
                        )


-- | Unique index for datatype constructors,
-- counting from 1 in the order they are given in the program text.
type ConIndex = Int


-- | Fixity of constructors
data Fixity = Prefix
            | Infix     -- Later: add associativity and precedence

            deriving ( Eq   -- ^ @since 4.0.0.0
                     , Show -- ^ @since 4.0.0.0
                     )


------------------------------------------------------------------------------
--
--      Observers for datatype representations
--
------------------------------------------------------------------------------


-- | Gets the type constructor including the module
dataTypeName :: DataType -> String
dataTypeName = tycon



-- | Gets the public presentation of a datatype
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
        (IntRep,    IntConstr i)      -> mkIntegralConstr dt i
        (FloatRep,  FloatConstr f)    -> mkRealConstr dt f
        (CharRep,   CharConstr c)     -> mkCharConstr dt c
        _ -> errorWithoutStackTrace "Data.Data.repConstr: The given ConstrRep does not fit to the given DataType."



------------------------------------------------------------------------------
--
--      Representations of algebraic data types
--
------------------------------------------------------------------------------


-- | Constructs an algebraic datatype
mkDataType :: String -> [Constr] -> DataType
mkDataType str cs = DataType
                        { tycon   = str
                        , datarep = AlgRep cs
                        }

-- | Constructs a constructor
mkConstrTag :: DataType -> String -> Int -> [String] -> Fixity -> Constr
mkConstrTag dt str idx fields fix =
        Constr
                { conrep    = AlgConstr idx
                , constring = str
                , confields = fields
                , confixity = fix
                , datatype  = dt
                }

-- | Constructs a constructor
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dt str fields fix = mkConstrTag dt str idx fields fix
  where
    idx = case findIndex (\c -> showConstr c == str) (dataTypeConstrs dt) of
            Just i  -> i+1 -- ConTag starts at 1
            Nothing -> errorWithoutStackTrace $
                        "Data.Data.mkConstr: couldn't find constructor " ++ str


-- | Gets the constructors of an algebraic datatype
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dt = case datarep dt of
                        (AlgRep cons) -> cons
                        _ -> errorWithoutStackTrace $ "Data.Data.dataTypeConstrs is not supported for "
                                    ++ dataTypeName dt ++
                                    ", as it is not an algebraic data type."


-- | Gets the field labels of a constructor.  The list of labels
-- is returned in the same order as they were given in the original
-- constructor declaration.
constrFields :: Constr -> [String]
constrFields = confields


-- | Gets the fixity of a constructor
constrFixity :: Constr -> Fixity
constrFixity = confixity



------------------------------------------------------------------------------
--
--      From strings to constr's and vice versa: all data types
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
        FloatRep    -> mkReadCon ffloat
        CharRep     -> mkReadCon (\c -> (mkPrimCon dt str (CharConstr c)))
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

    ffloat :: Double -> Constr
    ffloat =  mkPrimCon dt str . FloatConstr . toRational

------------------------------------------------------------------------------
--
--      Convenience functions: algebraic data types
--
------------------------------------------------------------------------------


-- | Test for an algebraic type
isAlgType :: DataType -> Bool
isAlgType dt = case datarep dt of
                 (AlgRep _) -> True
                 _ -> False


-- | Gets the constructor for an index (algebraic datatypes only)
indexConstr :: DataType -> ConIndex -> Constr
indexConstr dt idx = case datarep dt of
                        (AlgRep cs) -> cs !! (idx-1)
                        _           -> errorWithoutStackTrace $ "Data.Data.indexConstr is not supported for "
                                               ++ dataTypeName dt ++
                                               ", as it is not an algebraic data type."


-- | Gets the index of a constructor (algebraic datatypes only)
constrIndex :: Constr -> ConIndex
constrIndex con = case constrRep con of
                    (AlgConstr idx) -> idx
                    _ -> errorWithoutStackTrace $ "Data.Data.constrIndex is not supported for "
                                 ++ dataTypeName (constrType con) ++
                                 ", as it is not an algebraic data type."


-- | Gets the maximum constructor index of an algebraic datatype
maxConstrIndex :: DataType -> ConIndex
maxConstrIndex dt = case dataTypeRep dt of
                        AlgRep cs -> length cs
                        _            -> errorWithoutStackTrace $ "Data.Data.maxConstrIndex is not supported for "
                                                 ++ dataTypeName dt ++
                                                 ", as it is not an algebraic data type."



------------------------------------------------------------------------------
--
--      Representation of primitive types
--
------------------------------------------------------------------------------


-- | Constructs the 'Int' type
mkIntType :: String -> DataType
mkIntType = mkPrimType IntRep


-- | Constructs the 'Float' type
mkFloatType :: String -> DataType
mkFloatType = mkPrimType FloatRep


-- | Constructs the 'Char' type
mkCharType :: String -> DataType
mkCharType = mkPrimType CharRep


-- | Helper for 'mkIntType', 'mkFloatType'
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
                        , confields = errorWithoutStackTrace "Data.Data.confields"
                        , confixity = errorWithoutStackTrace "Data.Data.confixity"
                        }

mkIntegralConstr :: (Integral a, Show a) => DataType -> a -> Constr
mkIntegralConstr dt i = case datarep dt of
                  IntRep -> mkPrimCon dt (show i) (IntConstr (toInteger  i))
                  _ -> errorWithoutStackTrace $ "Data.Data.mkIntegralConstr is not supported for "
                               ++ dataTypeName dt ++
                               ", as it is not an Integral data type."

mkRealConstr :: (Real a, Show a) => DataType -> a -> Constr
mkRealConstr dt f = case datarep dt of
                    FloatRep -> mkPrimCon dt (show f) (FloatConstr (toRational f))
                    _ -> errorWithoutStackTrace $ "Data.Data.mkRealConstr is not supported for "
                                 ++ dataTypeName dt ++
                                 ", as it is not a Real data type."

-- | Makes a constructor for 'Char'.
mkCharConstr :: DataType -> Char -> Constr
mkCharConstr dt c = case datarep dt of
                   CharRep -> mkPrimCon dt (show c) (CharConstr c)
                   _ -> errorWithoutStackTrace $ "Data.Data.mkCharConstr is not supported for "
                                ++ dataTypeName dt ++
                                ", as it is not an Char data type."


------------------------------------------------------------------------------
--
--      Non-representations for non-representable types
--
------------------------------------------------------------------------------


-- | Constructs a non-representation for a non-representable type
mkNoRepType :: String -> DataType
mkNoRepType str = DataType
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
--      Convenience for qualified type constructors
--
------------------------------------------------------------------------------


-- | Gets the unqualified type constructor:
-- drop *.*.*... before name
--
tyconUQname :: String -> String
tyconUQname x = let x' = dropWhile (not . (==) '.') x
                 in if x' == [] then x else tyconUQname (tail x')


-- | Gets the module of a type constructor:
-- take *.*.*... before name
tyconModule :: String -> String
tyconModule x = let (a,b) = break ((==) '.') x
                 in if b == ""
                      then b
                      else a ++ tyconModule' (tail b)
  where
    tyconModule' y = let y' = tyconModule y
                      in if y' == "" then "" else ('.':y')




------------------------------------------------------------------------------
------------------------------------------------------------------------------
--
--      Instances of the Data class for Prelude-like types.
--      We define top-level definitions for representations.
--
------------------------------------------------------------------------------

-- | @since 4.0.0.0
deriving instance Data Bool

------------------------------------------------------------------------------

charType :: DataType
charType = mkCharType "Prelude.Char"

-- | @since 4.0.0.0
instance Data Char where
  toConstr x = mkCharConstr charType x
  gunfold _ z c = case constrRep c of
                    (CharConstr x) -> z x
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Char."
  dataTypeOf _ = charType


------------------------------------------------------------------------------

floatType :: DataType
floatType = mkFloatType "Prelude.Float"

-- | @since 4.0.0.0
instance Data Float where
  toConstr = mkRealConstr floatType
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Float."
  dataTypeOf _ = floatType


------------------------------------------------------------------------------

doubleType :: DataType
doubleType = mkFloatType "Prelude.Double"

-- | @since 4.0.0.0
instance Data Double where
  toConstr = mkRealConstr doubleType
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Double."
  dataTypeOf _ = doubleType


------------------------------------------------------------------------------

intType :: DataType
intType = mkIntType "Prelude.Int"

-- | @since 4.0.0.0
instance Data Int where
  toConstr x = mkIntegralConstr intType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int."
  dataTypeOf _ = intType


------------------------------------------------------------------------------

integerType :: DataType
integerType = mkIntType "Prelude.Integer"

-- | @since 4.0.0.0
instance Data Integer where
  toConstr = mkIntegralConstr integerType
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z x
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Integer."
  dataTypeOf _ = integerType


------------------------------------------------------------------------------

-- This follows the same style as the other integral 'Data' instances
-- defined in "Data.Data"
naturalType :: DataType
naturalType = mkIntType "Numeric.Natural.Natural"

-- | @since 4.8.0.0
instance Data Natural where
  toConstr x = mkIntegralConstr naturalType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Natural"
  dataTypeOf _ = naturalType


------------------------------------------------------------------------------

int8Type :: DataType
int8Type = mkIntType "Data.Int.Int8"

-- | @since 4.0.0.0
instance Data Int8 where
  toConstr x = mkIntegralConstr int8Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int8."
  dataTypeOf _ = int8Type


------------------------------------------------------------------------------

int16Type :: DataType
int16Type = mkIntType "Data.Int.Int16"

-- | @since 4.0.0.0
instance Data Int16 where
  toConstr x = mkIntegralConstr int16Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int16."
  dataTypeOf _ = int16Type


------------------------------------------------------------------------------

int32Type :: DataType
int32Type = mkIntType "Data.Int.Int32"

-- | @since 4.0.0.0
instance Data Int32 where
  toConstr x = mkIntegralConstr int32Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int32."
  dataTypeOf _ = int32Type


------------------------------------------------------------------------------

int64Type :: DataType
int64Type = mkIntType "Data.Int.Int64"

-- | @since 4.0.0.0
instance Data Int64 where
  toConstr x = mkIntegralConstr int64Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int64."
  dataTypeOf _ = int64Type


------------------------------------------------------------------------------

wordType :: DataType
wordType = mkIntType "Data.Word.Word"

-- | @since 4.0.0.0
instance Data Word where
  toConstr x = mkIntegralConstr wordType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word"
  dataTypeOf _ = wordType


------------------------------------------------------------------------------

word8Type :: DataType
word8Type = mkIntType "Data.Word.Word8"

-- | @since 4.0.0.0
instance Data Word8 where
  toConstr x = mkIntegralConstr word8Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word8."
  dataTypeOf _ = word8Type


------------------------------------------------------------------------------

word16Type :: DataType
word16Type = mkIntType "Data.Word.Word16"

-- | @since 4.0.0.0
instance Data Word16 where
  toConstr x = mkIntegralConstr word16Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word16."
  dataTypeOf _ = word16Type


------------------------------------------------------------------------------

word32Type :: DataType
word32Type = mkIntType "Data.Word.Word32"

-- | @since 4.0.0.0
instance Data Word32 where
  toConstr x = mkIntegralConstr word32Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word32."
  dataTypeOf _ = word32Type


------------------------------------------------------------------------------

word64Type :: DataType
word64Type = mkIntType "Data.Word.Word64"

-- | @since 4.0.0.0
instance Data Word64 where
  toConstr x = mkIntegralConstr word64Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word64."
  dataTypeOf _ = word64Type


------------------------------------------------------------------------------

ratioConstr :: Constr
ratioConstr = mkConstr ratioDataType ":%" [] Infix

ratioDataType :: DataType
ratioDataType = mkDataType "GHC.Real.Ratio" [ratioConstr]

-- NB: This Data instance intentionally uses the (%) smart constructor instead
-- of the internal (:%) constructor to preserve the invariant that a Ratio
-- value is reduced to normal form. See #10011.

-- | @since 4.0.0.0
instance (Data a, Integral a) => Data (Ratio a) where
  gfoldl k z (a :% b) = z (%) `k` a `k` b
  toConstr _ = ratioConstr
  gunfold k z c | constrIndex c == 1 = k (k (z (%)))
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Ratio)"
  dataTypeOf _  = ratioDataType


------------------------------------------------------------------------------

nilConstr :: Constr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr :: Constr
consConstr   = mkConstr listDataType "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

-- | @since 4.0.0.0
instance Data a => Data [a] where
  gfoldl _ z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  toConstr []    = nilConstr
  toConstr (_:_) = consConstr
  gunfold k z c = case constrIndex c of
                    1 -> z []
                    2 -> k (k (z (:)))
                    _ -> errorWithoutStackTrace "Data.Data.gunfold(List)"
  dataTypeOf _ = listDataType
  dataCast1 f  = gcast1 f

--
-- The gmaps are given as an illustration.
-- This shows that the gmaps for lists are different from list maps.
--
  gmapT  _   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  _   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  _   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')


------------------------------------------------------------------------------

-- | @since 4.14.0.0
deriving instance (Typeable (a :: Type -> Type -> Type), Typeable b, Typeable c,
                   Data (a b c))
         => Data (WrappedArrow a b c)

-- | @since 4.14.0.0
deriving instance (Typeable (m :: Type -> Type), Typeable a, Data (m a))
         => Data (WrappedMonad m a)

-- | @since 4.14.0.0
deriving instance Data a => Data (ZipList a)

-- | @since 4.9.0.0
deriving instance Data a => Data (NonEmpty a)

-- | @since 4.0.0.0
deriving instance Data a => Data (Maybe a)

-- | @since 4.0.0.0
deriving instance Data Ordering

-- | @since 4.0.0.0
deriving instance (Data a, Data b) => Data (Either a b)

-- | @since 4.0.0.0
deriving instance Data ()

-- | @since 4.15
deriving instance Data a => Data (Solo a)

-- | @since 4.0.0.0
deriving instance (Data a, Data b) => Data (a,b)

-- | @since 4.0.0.0
deriving instance (Data a, Data b, Data c) => Data (a,b,c)

-- | @since 4.0.0.0
deriving instance (Data a, Data b, Data c, Data d)
         => Data (a,b,c,d)

-- | @since 4.0.0.0
deriving instance (Data a, Data b, Data c, Data d, Data e)
         => Data (a,b,c,d,e)

-- | @since 4.0.0.0
deriving instance (Data a, Data b, Data c, Data d, Data e, Data f)
         => Data (a,b,c,d,e,f)

-- | @since 4.0.0.0
deriving instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g)
         => Data (a,b,c,d,e,f,g)

------------------------------------------------------------------------------

-- | @since 4.8.0.0
instance Data a => Data (Ptr a) where
  toConstr _   = errorWithoutStackTrace "Data.Data.toConstr(Ptr)"
  gunfold _ _  = errorWithoutStackTrace "Data.Data.gunfold(Ptr)"
  dataTypeOf _ = mkNoRepType "GHC.Ptr.Ptr"
  dataCast1 x  = gcast1 x

------------------------------------------------------------------------------

-- | @since 4.8.0.0
instance Data a => Data (ForeignPtr a) where
  toConstr _   = errorWithoutStackTrace "Data.Data.toConstr(ForeignPtr)"
  gunfold _ _  = errorWithoutStackTrace "Data.Data.gunfold(ForeignPtr)"
  dataTypeOf _ = mkNoRepType "GHC.ForeignPtr.ForeignPtr"
  dataCast1 x  = gcast1 x

-- | @since 4.11.0.0
deriving instance Data IntPtr

-- | @since 4.11.0.0
deriving instance Data WordPtr

------------------------------------------------------------------------------
-- The Data instance for Array preserves data abstraction at the cost of
-- inefficiency. We omit reflection services for the sake of data abstraction.
-- | @since 4.8.0.0
instance (Data a, Data b, Ix a) => Data (Array a b)
 where
  gfoldl f z a = z (listArray (bounds a)) `f` (elems a)
  toConstr _   = errorWithoutStackTrace "Data.Data.toConstr(Array)"
  gunfold _ _  = errorWithoutStackTrace "Data.Data.gunfold(Array)"
  dataTypeOf _ = mkNoRepType "Data.Array.Array"
  dataCast2 x  = gcast2 x

----------------------------------------------------------------------------
-- Data instance for Proxy

-- | @since 4.7.0.0
deriving instance (Data t) => Data (Proxy t)

-- | @since 4.7.0.0
deriving instance (a ~ b, Data a) => Data (a :~: b)

-- | @since 4.10.0.0
deriving instance (Typeable i, Typeable j, Typeable a, Typeable b,
                    (a :: i) ~~ (b :: j))
    => Data (a :~~: b)

-- | @since 4.7.0.0
deriving instance (Coercible a b, Data a, Data b) => Data (Coercion a b)

-- | @since 4.9.0.0
deriving instance Data a => Data (Identity a)

-- | @since 4.10.0.0
deriving instance (Typeable k, Data a, Typeable (b :: k)) => Data (Const a b)

-- | @since 4.7.0.0
deriving instance Data Version

----------------------------------------------------------------------------
-- Data instances for Data.Monoid wrappers

-- | @since 4.8.0.0
deriving instance Data a => Data (Dual a)

-- | @since 4.8.0.0
deriving instance Data All

-- | @since 4.8.0.0
deriving instance Data Any

-- | @since 4.8.0.0
deriving instance Data a => Data (Sum a)

-- | @since 4.8.0.0
deriving instance Data a => Data (Product a)

-- | @since 4.8.0.0
deriving instance Data a => Data (First a)

-- | @since 4.8.0.0
deriving instance Data a => Data (Last a)

-- | @since 4.8.0.0
deriving instance (Data (f a), Data a, Typeable f) => Data (Alt f a)

-- | @since 4.12.0.0
deriving instance (Data (f a), Data a, Typeable f) => Data (Ap f a)

----------------------------------------------------------------------------
-- Data instances for GHC.Generics representations

-- | @since 4.9.0.0
deriving instance Data p => Data (U1 p)

-- | @since 4.9.0.0
deriving instance Data p => Data (Par1 p)

-- | @since 4.9.0.0
deriving instance (Data (f p), Typeable f, Data p) => Data (Rec1 f p)

-- | @since 4.9.0.0
deriving instance (Typeable i, Data p, Data c) => Data (K1 i c p)

-- | @since 4.9.0.0
deriving instance (Data p, Data (f p), Typeable c, Typeable i, Typeable f)
    => Data (M1 i c f p)

-- | @since 4.9.0.0
deriving instance (Typeable f, Typeable g, Data p, Data (f p), Data (g p))
    => Data ((f :+: g) p)

-- | @since 4.9.0.0
deriving instance (Typeable (f :: Type -> Type), Typeable (g :: Type -> Type),
          Data p, Data (f (g p)))
    => Data ((f :.: g) p)

-- | @since 4.9.0.0
deriving instance Data p => Data (V1 p)

-- | @since 4.9.0.0
deriving instance (Typeable f, Typeable g, Data p, Data (f p), Data (g p))
    => Data ((f :*: g) p)

-- | @since 4.9.0.0
deriving instance Data Generics.Fixity

-- | @since 4.9.0.0
deriving instance Data Associativity

-- | @since 4.9.0.0
deriving instance Data SourceUnpackedness

-- | @since 4.9.0.0
deriving instance Data SourceStrictness

-- | @since 4.9.0.0
deriving instance Data DecidedStrictness

----------------------------------------------------------------------------
-- Data instances for Data.Ord

-- | @since 4.12.0.0
deriving instance Data a => Data (Down a)
