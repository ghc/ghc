{-# LANGUAGE UndecidableInstances, OverlappingInstances, Rank2Types,
    KindSignatures, EmptyDataDecls, MultiParamTypeClasses, CPP #-}

{-

(C) 2004--2005 Ralf Laemmel, Simon D. Foster

This module approximates Data.Generics.Basics.

-}


module T1735_Help.Basics (

 module Data.Typeable,
 module T1735_Help.Context,
 module T1735_Help.Basics

) where

import Data.Typeable
import T1735_Help.Context


------------------------------------------------------------------------------
-- The ingenious Data class

class (Typeable a, Sat (ctx a)) => Data ctx a

   where

     gfoldl :: Proxy ctx
            -> (forall b c. Data ctx b => w (b -> c) -> b -> w c)
            -> (forall g. g -> w g)
            -> a -> w a

     -- Default definition for gfoldl
     -- which copes immediately with basic datatypes
     --
     gfoldl _ _ z = z

     gunfold :: Proxy ctx
             -> (forall b r. Data ctx b => c (b -> r) -> c r)
             -> (forall r. r -> c r)
             -> Constr
             -> c a

     toConstr :: Proxy ctx -> a -> Constr

     dataTypeOf :: Proxy ctx -> a -> DataType

     -- incomplete implementation

     gunfold _ _ _ _ = undefined

     dataTypeOf _ _ = undefined

     -- | Mediate types and unary type constructors
     dataCast1 :: Typeable t
               => Proxy ctx
               -> (forall b. Data ctx b => w (t b))
               -> Maybe (w a)
     dataCast1 _ _ = Nothing

     -- | Mediate types and binary type constructors
     dataCast2 :: Typeable t
               => Proxy ctx
               -> (forall b c. (Data ctx b, Data ctx c) => w (t b c))
               -> Maybe (w a)
     dataCast2 _ _ = Nothing



------------------------------------------------------------------------------

-- Generic transformations

type GenericT ctx = forall a. Data ctx a => a -> a


-- Generic map for transformations

gmapT :: Proxy ctx -> GenericT ctx -> GenericT ctx

gmapT ctx f x = unID (gfoldl ctx k ID x)
  where
    k (ID g) y = ID (g (f y))


-- The identity type constructor

newtype ID x = ID { unID :: x }


------------------------------------------------------------------------------

-- Generic monadic transformations

type GenericM m ctx = forall a. Data ctx a => a -> m a

-- Generic map for monadic transformations

gmapM :: Monad m => Proxy ctx -> GenericM m ctx -> GenericM m ctx
gmapM ctx f = gfoldl ctx k return
    where k c x = do c' <- c
                     x' <- f x
                     return (c' x')


------------------------------------------------------------------------------

-- Generic queries

type GenericQ ctx r = forall a. Data ctx a => a -> r


-- Map for queries

gmapQ :: Proxy ctx -> GenericQ ctx r -> GenericQ ctx [r]
gmapQ ctx f = gmapQr ctx (:) [] f

gmapQr :: Data ctx a
       => Proxy ctx
       -> (r' -> r -> r)
       -> r
       -> GenericQ ctx r'
       -> a
       -> r
gmapQr ctx o r f x = unQr (gfoldl ctx k (const (Qr id)) x) r
  where
    k (Qr g) y = Qr (\s -> g (f y `o` s))

-- The type constructor used in definition of gmapQr
newtype Qr r a = Qr { unQr  :: r -> r }



------------------------------------------------------------------------------
--
-- Generic unfolding
--
------------------------------------------------------------------------------



-- | Build a term skeleton
fromConstr :: Data ctx a => Proxy ctx -> Constr -> a
fromConstr ctx = fromConstrB ctx undefined

-- | Build a term and use a generic function for subterms
fromConstrB :: Data ctx a
            => Proxy ctx
            -> (forall b. Data ctx b => b)
            -> Constr
            -> a
fromConstrB ctx f = unID . gunfold ctx k z
 where
  k c = ID (unID c f)
  z = ID



-- | Monadic variation on \"fromConstrB\"
fromConstrM :: (Monad m, Data ctx a)
            => Proxy ctx
            -> (forall b. Data ctx b => m b)
            -> Constr
            -> m a
fromConstrM ctx f = gunfold ctx k z
 where
  k c = do { c' <- c; b <- f; return (c' b) }
  z = return



------------------------------------------------------------------------------
--
-- Datatype and constructor representations
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
                        , confields :: [String] -- for AlgRep only
                        , confixity :: Fixity   -- for AlgRep only
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
            | Infix  -- Later: add associativity and precedence

            deriving (Eq,Show)


------------------------------------------------------------------------------
--
-- Observers for datatype representations
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
-- Representations of algebraic data types
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
-- From strings to constr's and vice versa: all data types
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
-- Convenience funtions: algebraic data types
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
                        _         -> error "maxConstrIndex"



------------------------------------------------------------------------------
--
-- Representation of primitive types
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
                        , confields = error $ concat ["constrFields : ", (tycon dt), " is primative"]
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
-- Non-representations for non-presentable types
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

