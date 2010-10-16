{-# LANGUAGE  DeriveDataTypeable,
              FlexibleContexts, FlexibleInstances,
              MultiParamTypeClasses,
              OverlappingInstances, UndecidableInstances,
              Rank2Types, KindSignatures, EmptyDataDecls #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Data.Typeable

class Sat a where
    dict :: a

data Proxy (a :: * -> *)

class (Typeable a, Sat (ctx a)) => Data ctx a where
    gfoldl :: Proxy ctx
           -> (forall b c. Data ctx b => w (b -> c) -> b -> w c)
           -> (forall g. g -> w g)
           -> a -> w a
    gunfold :: Proxy ctx
            -> (forall b r. Data ctx b => c (b -> r) -> c r)
            -> (forall r. r -> c r)
            -> Constr
            -> c a
    dataTypeOf :: Proxy ctx -> a -> DataType

newtype ID x = ID { unID :: x }

fromConstrB :: Data ctx a
            => Proxy ctx
            -> (forall b. Data ctx b => b)
            -> Constr
            -> a
fromConstrB ctx f = unID . gunfold ctx k z
 where
  k c = ID (unID c f)
  z = ID

data DataType = DataType
                        { tycon   :: String
                        , datarep :: DataRep
                        }

data Constr = Constr { conrep    :: ConstrRep
                     , constring :: String
                     , confields :: [String]
                     , confixity :: Fixity
                     , datatype  :: DataType
                     }

data DataRep = AlgRep [Constr]
data ConstrRep = AlgConstr ConIndex

type ConIndex = Int

data Fixity = Prefix
            | Infix

constrRep :: Constr -> ConstrRep
constrRep = conrep

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
                     AlgRep cons -> cons

-- | Gets the string for a constructor
showConstr :: Constr -> String
showConstr = constring

-- | Gets the index of a constructor
constrIndex :: Constr -> ConIndex
constrIndex con = case constrRep con of
                  AlgConstr idx -> idx

nilConstr :: Constr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr :: Constr
consConstr   = mkConstr listDataType "(:)" [] Infix
listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

instance (Sat (ctx [a]), Data ctx a) =>
         Data ctx [a] where
  gfoldl _ _ z []     = z []
  gfoldl _ f z (x:xs) = z (:) `f` x `f` xs
  gunfold _ k z c = case constrIndex c of
                      1 -> z []
                      2 -> k (k (z (:)))
                      _ -> error "gunfold List"
  dataTypeOf _ _ = listDataType

class (Data DefaultD a) => Default a where
    defaultValue :: a
    defaultValue = defaultDefaultValue

defaultDefaultValue :: (Data DefaultD a,Default a) => a
defaultDefaultValue = res
    where res = case datarep $ dataTypeOf defaultProxy res of
                    AlgRep (c:_) ->
                        fromConstrB defaultProxy (defaultValueD dict) c
                    AlgRep [] ->
                        error "defaultDefaultValue: Bad DataRep"

data DefaultD a = DefaultD { defaultValueD :: a }

defaultProxy :: Proxy DefaultD
defaultProxy = error "defaultProxy"

instance Default t => Sat (DefaultD t) where
    dict = DefaultD { defaultValueD = defaultValue }

instance Default a => Default [a] where
    defaultValue = []
data Proposition = Proposition Expression deriving (Show, Typeable)
data Expression = Conjunction [Expression] deriving (Show, Typeable)

constr_Proposition :: Constr
constr_Proposition = mkConstr dataType_Proposition "Proposition" [] Prefix
dataType_Proposition :: DataType
dataType_Proposition = mkDataType "Proposition" [constr_Proposition]

instance (Data ctx Expression, Sat (ctx Proposition), Sat (ctx Expression))
      => Data ctx Proposition
    where gfoldl _ f z x = case x of
                           Proposition arg -> f (z Proposition) arg
          gunfold _ k z c = case constrIndex c of
                            1 -> k (z Proposition)
                            _ -> error "gunfold: fallthrough"
          dataTypeOf _ _ = dataType_Proposition

constr_Conjunction :: Constr
constr_Conjunction = mkConstr dataType_Expression "Conjunction" [] Prefix
dataType_Expression :: DataType
dataType_Expression = mkDataType "Expression" [constr_Conjunction]

instance (Data ctx [Expression], Sat (ctx Expression), Sat (ctx [Expression]))
      => Data ctx Expression
    where gfoldl _ f z x = case x of
                           Conjunction arg -> f (z Conjunction) arg
          gunfold _ k z c = case constrIndex c of
                            1 -> k (z Conjunction)
                            _ -> error "gunfold: fallthrough"
          dataTypeOf _ _ = dataType_Expression

instance Default Proposition
instance Default Expression

main :: IO ()
main = putStrLn (show (defaultValue :: Proposition))

