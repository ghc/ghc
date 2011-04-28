{-# OPTIONS_GHC -XNoImplicitPrelude      #-}
{-# OPTIONS_GHC -XEmptyDataDecls         #-}
{-# OPTIONS_GHC -XMultiParamTypeClasses  #-}
{-# OPTIONS_GHC -XTypeSynonymInstances   #-}
{-# OPTIONS_GHC -XTypeOperators          #-}
{-# OPTIONS_GHC -XKindSignatures         #-}
{-# OPTIONS_GHC -XTypeFamilies           #-}
{-# OPTIONS_GHC -XNoGenerics             #-}

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

  -- * Representable type classes
  , Representable0(..), Representable1(..)

  -- * Representation type families
  , Rep0, Rep1

  ) where
  
import {-# SOURCE #-} GHC.Types -- ([](..), Int, Char, Bool(..))

--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
data V1 p

-- | Unit: used for constructors without arguments
data U1 p = U1

-- | Used for marking occurrences of the parameter
newtype Par1 p = Par1 { unPar1 :: p }


-- | Recursive calls of kind * -> *
newtype Rec1 f p = Rec1 { unRec1 :: f p }

-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p

-- | Composition of functors
infixr 7 :.:
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }

-- | Tag for K1: recursion (of kind *)
data R
-- | Tag for K1: parameters (other than the last)
data P

-- | Type synonym for encoding recursion (of kind *)
type Rec0  = K1 R
-- | Type synonym for encoding parameters (other than the last)
type Par0  = K1 P

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
--  deriving (Eq, Show, Ord, Read)
-- TODO: Add these instances to the Prelude

-- | Datatype to represent the fixity of a constructor. An infix
-- | declaration directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
--  deriving (Eq, Show, Ord, Read)
-- TODO: Add these instances to the Prelude

-- | Get the precedence of a fixity value.
prec :: Fixity -> Int
prec Prefix      = I# 10#
prec (Infix _ n) = n

-- | Datatype to represent the associativy of a constructor
data Associativity =  LeftAssociative 
                   |  RightAssociative
                   |  NotAssociative
--  deriving (Eq, Show, Ord, Read)
-- TODO: Add these instances to the Prelude

-- | Generic representation type
type family Rep0 a :: * -> *

-- | Representable types of kind *
class Representable0 a where
  -- | Convert from the datatype to its representation
  from0  :: a -> (Rep0 a) x
  -- | Convert from the representation to the datatype
  to0    :: (Rep0 a) x -> a


-- | Generic representation type
type family Rep1 f :: * -> *

-- | Representable types of kind * -> *
class Representable1 f where
  -- | Convert from the datatype to its representation
  from1  :: f a -> (Rep1 f) a
  -- | Convert from the representation to the datatype
  to1    :: (Rep1 f) a -> f a

--------------------------------------------------------------------------------
-- Representation for base types
--------------------------------------------------------------------------------

-- Representation types
{-
type Rep1Par1 = Par1
instance Representable1 Par1 Rep1Par1 where
  from1 = id
  to1 = id

type Rep1Rec1 f = Rec1 f
instance Representable1 (Rec1 f) (Rep1Rec1 f) where
  from1 = id
  to1 = id
-}
-- Kind *

{-
type Rep0Char = Rec0 Char
instance Representable0 Char Rep0Char where
  from0 = K1
  to0 = unK1

type Rep0Int = Rec0 Int
instance Representable0 Int Rep0Int where
  from0 = K1
  to0 = unK1

type Rep0Float = Rec0 Float
instance Representable0 Float Rep0Float where
  from0 = K1
  to0 = unK1

-- etc...

-- Kind * -> *

data Maybe_
data Nothing_
data Just_

instance Datatype Maybe_ where
  datatypeName _ = "Maybe"
  moduleName   _ = "Representation"

instance Constructor Nothing_ where
  conName _ = "Nothing"

instance Constructor Just_ where
  conName _ = "Just"

type Rep0Maybe a = D1 Maybe_ (C1 Nothing_ U1 :+: C1 Just_ (Par0 a))
instance Representable0 (Maybe a) (Rep0Maybe a) where
  from0 Nothing  = M1 (L1 (M1 U1))
  from0 (Just x) = M1 (R1 (M1 (K1 x)))
  to0 (M1 (L1 (M1 U1)))     = Nothing
  to0 (M1 (R1 (M1 (K1 x)))) = Just x

type Rep1Maybe = D1 Maybe_ (C1 Nothing_ U1 :+: C1 Just_ Par1)
instance Representable1 Maybe Rep1Maybe where
  from1 Nothing  = M1 (L1 (M1 U1))
  from1 (Just x) = M1 (R1 (M1 (Par1 x)))
  to1 (M1 (L1 (M1 U1)))       = Nothing
  to1 (M1 (R1 (M1 (Par1 x)))) = Just x


data List__
data Nil__
data Cons__

instance Datatype [a] where
  datatypeName _ = "[]"
  moduleName   _ = "Data.List"

instance Constructor Nil__  where conName _ = "[]"
instance Constructor Cons__ where
  conName   _ = ":"
  conFixity _ = Infix RightAssociative 5

type Rep0List a = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par0 a :*: Rec0 [a])))
instance Representable0 [a] (Rep0List a) where
  from0 []    = M1 (L1 (M1 U1))
  from0 (h:t) = M1 (R1 (M1 (K1 h :*: K1 t)))
  to0 (M1 (L1 (M1 U1)))              = []
  to0 (M1 (R1 (M1 (K1 h :*: K1 t)))) = h : t

type Rep1List = D1 List__ ((C1 Nil__ U1) :+: (C1 Cons__ (Par1 :*: Rec1 [])))
instance Representable1 [] Rep1List where
  from1 []    = M1 (L1 (M1 U1))
  from1 (h:t) = M1 (R1 (M1 (Par1 h :*: Rec1 t)))
  to1 (M1 (L1 (M1 U1)))                  = []
  to1 (M1 (R1 (M1 (Par1 h :*: Rec1 t)))) = h : t

-- etc...
-}

