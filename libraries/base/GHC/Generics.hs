{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE DeriveGeneric          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics
-- Copyright   :  (c) Universiteit Utrecht 2010-2011, University of Oxford 2012
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Support for generic programming with a sum-of-products view. 
-- For more information, please visit the HaskellWiki page
-- (<http://www.haskell.org/haskellwiki/GHC.Generics>)
-- or use the generic-deriving package on Hackage:
-- <http://hackage.haskell.org/package/generic-deriving>.
--
-- /Since: 4.6.0.0/
-----------------------------------------------------------------------------

module GHC.Generics  (
  -- * Generic representation types
    V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..)
  , (:+:)(..), (:*:)(..), (:.:)(..)

  -- ** Synonyms for convenience
  , Rec0, Par0, R, P
  , D1, C1, S1, D, C, S

  -- * Meta-information
  , Datatype(..), Constructor(..), Selector(..), NoSelector
  , Fixity(..), Associativity(..), Arity(..), prec

  -- * Generic type classes
  , Generic(..), Generic1(..)

  ) where

-- We use some base types
import GHC.Types
import Data.Maybe ( Maybe(..) )
import Data.Either ( Either(..) )

-- Needed for instances
import GHC.Classes ( Eq, Ord )
import GHC.Read ( Read )
import GHC.Show ( Show )
import Data.Proxy

--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
data V1 p

-- | Unit: used for constructors without arguments
data U1 p = U1
  deriving (Eq, Read, Show, Generic)

-- | Used for marking occurrences of the parameter
newtype Par1 p = Par1 { unPar1 :: p }
  deriving (Eq, Read, Show, Generic)

-- | Recursive calls of kind * -> *
newtype Rec1 f p = Rec1 { unRec1 :: f p }
  deriving (Eq, Read, Show, Generic)

-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }
  deriving (Eq, Read, Show, Generic)

-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }
  deriving (Eq, Read, Show, Generic)

-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)
  deriving (Eq, Read, Show, Generic)

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p
  deriving (Eq, Read, Show, Generic)

-- | Composition of functors
infixr 7 :.:
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }
  deriving (Eq, Read, Show, Generic)

-- | Tag for K1: recursion (of kind *)
data R
-- | Tag for K1: parameters (other than the last)
data P

-- | Type synonym for encoding recursion (of kind *)
type Rec0  = K1 R
-- | Type synonym for encoding parameters (other than the last)
type Par0  = K1 P
{-# DEPRECATED Par0 "Par0 is no longer used; use Rec0 instead" #-} -- deprecated in 7.6
{-# DEPRECATED P "P is no longer used; use R instead" #-} -- deprecated in 7.6

-- | Tag for M1: datatype
data D
-- | Tag for M1: constructor
data C
-- | Tag for M1: record selector
data S

-- | Type synonym for encoding meta-information for datatypes
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors
type C1 = M1 C

-- | Type synonym for encoding meta-information for record selectors
type S1 = M1 S


-- | Class for datatypes that represent datatypes
class Datatype d where
  -- | The name of the datatype (unqualified)
  datatypeName :: t d (f :: * -> *) a -> [Char]
  -- | The fully-qualified name of the module where the type is declared
  moduleName   :: t d (f :: * -> *) a -> [Char]
  -- | Marks if the datatype is actually a newtype
  isNewtype    :: t d (f :: * -> *) a -> Bool
  isNewtype _ = False


-- | Class for datatypes that represent records
class Selector s where
  -- | The name of the selector
  selName :: t s (f :: * -> *) a -> [Char]

-- | Used for constructor fields without a name
data NoSelector

instance Selector NoSelector where selName _ = ""

-- | Class for datatypes that represent data constructors
class Constructor c where
  -- | The name of the constructor
  conName :: t c (f :: * -> *) a -> [Char]

  -- | The fixity of the constructor
  conFixity :: t c (f :: * -> *) a -> Fixity
  conFixity _ = Prefix

  -- | Marks if this constructor is a record
  conIsRecord :: t c (f :: * -> *) a -> Bool
  conIsRecord _ = False


-- | Datatype to represent the arity of a tuple.
data Arity = NoArity | Arity Int
  deriving (Eq, Show, Ord, Read, Generic)

-- | Datatype to represent the fixity of a constructor. An infix
-- | declaration directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read, Generic)

-- | Get the precedence of a fixity value.
prec :: Fixity -> Int
prec Prefix      = 10
prec (Infix _ n) = n

-- | Datatype to represent the associativity of a constructor
data Associativity = LeftAssociative
                   | RightAssociative
                   | NotAssociative
  deriving (Eq, Show, Ord, Read, Generic)

-- | Representable types of kind *.
-- This class is derivable in GHC with the DeriveGeneric flag on.
class Generic a where
  -- | Generic representation type
  type Rep a :: * -> *
  -- | Convert from the datatype to its representation
  from  :: a -> (Rep a) x
  -- | Convert from the representation to the datatype
  to    :: (Rep a) x -> a


-- | Representable types of kind * -> *.
-- This class is derivable in GHC with the DeriveGeneric flag on.
class Generic1 f where
  -- | Generic representation type
  type Rep1 f :: * -> *
  -- | Convert from the datatype to its representation
  from1  :: f a -> (Rep1 f) a
  -- | Convert from the representation to the datatype
  to1    :: (Rep1 f) a -> f a


--------------------------------------------------------------------------------
-- Derived instances
--------------------------------------------------------------------------------
deriving instance Generic [a]
deriving instance Generic (Maybe a)
deriving instance Generic (Either a b)
deriving instance Generic Bool
deriving instance Generic Ordering
deriving instance Generic ()
deriving instance Generic ((,) a b)
deriving instance Generic ((,,) a b c)
deriving instance Generic ((,,,) a b c d)
deriving instance Generic ((,,,,) a b c d e)
deriving instance Generic ((,,,,,) a b c d e f)
deriving instance Generic ((,,,,,,) a b c d e f g)

deriving instance Generic1 []
deriving instance Generic1 Maybe
deriving instance Generic1 (Either a)
deriving instance Generic1 ((,) a)
deriving instance Generic1 ((,,) a b)
deriving instance Generic1 ((,,,) a b c)
deriving instance Generic1 ((,,,,) a b c d)
deriving instance Generic1 ((,,,,,) a b c d e)
deriving instance Generic1 ((,,,,,,) a b c d e f)

--------------------------------------------------------------------------------
-- Primitive representations
--------------------------------------------------------------------------------

-- Int
data D_Int
data C_Int

instance Datatype D_Int where
  datatypeName _ = "Int"
  moduleName   _ = "GHC.Int"

instance Constructor C_Int where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Int where
  type Rep Int = D1 D_Int (C1 C_Int (S1 NoSelector (Rec0 Int)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x


-- Float
data D_Float
data C_Float

instance Datatype D_Float where
  datatypeName _ = "Float"
  moduleName   _ = "GHC.Float"

instance Constructor C_Float where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Float where
  type Rep Float = D1 D_Float (C1 C_Float (S1 NoSelector (Rec0 Float)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x


-- Double
data D_Double
data C_Double

instance Datatype D_Double where
  datatypeName _ = "Double"
  moduleName   _ = "GHC.Float"

instance Constructor C_Double where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Double where
  type Rep Double = D1 D_Double (C1 C_Double (S1 NoSelector (Rec0 Double)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x


-- Char
data D_Char
data C_Char

instance Datatype D_Char where
  datatypeName _ = "Char"
  moduleName   _ = "GHC.Base"

instance Constructor C_Char where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Char where
  type Rep Char = D1 D_Char (C1 C_Char (S1 NoSelector (Rec0 Char)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x

deriving instance Generic (Proxy t)
