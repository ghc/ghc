{-# LANGUAGE  DeriveDataTypeable,
              FlexibleContexts, FlexibleInstances,
              MultiParamTypeClasses,
              OverlappingInstances, UndecidableInstances,
              Rank2Types, KindSignatures, EmptyDataDecls #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

-- June 2021: no longer compiles.
-- See T3731_simple for a simpler test case which explains the situation.

import Data.Typeable

class Sat a where
    dict :: a


class ( Sat (ctx a)) => Data ctx a where
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
  gunfold _ k z c = case constrIndex c of
                      1 -> z []
                      2 -> k (k (z (:)))
                      _ -> error "gunfold List"
  dataTypeOf _ _ = listDataType

class (Data DefaultD a) => Default a where
    defaultValue :: a
    defaultValue = defaultDefaultValue

defaultDefaultValue :: Data DefaultD a => a
{-# NOINLINE defaultDefaultValue #-}
defaultDefaultValue = res
    where res = case datarep $ dataTypeOf defaultProxy res of
                    AlgRep (c:_) ->
                        fromConstrB defaultProxy (defaultValueD dict) c
                    AlgRep [] ->
                        error "defaultDefaultValue: Bad DataRep"

data DefaultD a = DefaultD { defaultValueD :: a }

defaultProxy :: Proxy DefaultD
defaultProxy = error "defaultProxy"

-- dfun3
instance Default t => Sat (DefaultD t) where
    dict = DefaultD { defaultValueD = defaultValue }

-- dfun5
instance Default a => Default [a] where
    defaultValue = []
data Proposition = Proposition Expression deriving (Show, Typeable)
data Expression = Conjunction [Expression] deriving (Show, Typeable)

constr_Proposition :: Constr
constr_Proposition = mkConstr dataType_Proposition "Proposition" [] Prefix
dataType_Proposition :: DataType
dataType_Proposition = mkDataType "Proposition" [constr_Proposition]

-- dfun1
instance Data DefaultD Proposition
    where gunfold _ k z c = case constrIndex c of
                            1 -> k (z Proposition)
                            _ -> error "gunfold: fallthrough"
          dataTypeOf _ _ = dataType_Proposition

constr_Conjunction :: Constr
constr_Conjunction = mkConstr dataType_Expression "Conjunction" [] Prefix
dataType_Expression :: DataType
dataType_Expression = mkDataType "Expression" [constr_Conjunction]

-- dfun2
instance (Sat (ctx [Expression]), Sat (ctx Expression))
      => Data ctx Expression
    where gunfold _ k z c = case constrIndex c of
                            1 -> k (z Conjunction)
                            _ -> error "gunfold: fallthrough"
          dataTypeOf _ _ = dataType_Expression

-- dfun0
instance Default Proposition where
    defaultValue = defaultDefaultValue

-- dfun4
instance Default Expression where
    defaultValue = defaultDefaultValue

main :: IO ()
main = putStrLn (show (defaultValue :: Proposition))

{- The trouble comes from "instance Default Expression"

Define:  dfun4 : Default Expression = MkDefault d_aCl (..)

Simplify the superclass:
  Wanted:  d_aCl : Data DefaultD Expression
  Derived: d_aCn : Sat DefaultD Expression       d_aCn = $p1 d_aCl  {irrelevant}

  by dfun2                                       d_aCl = dfun2 d_aCo d_aCp
  Wanted: d_aCo : Sat (DefaultD [Expression])
          d_aCp : Sat (DefaultD Expression)

  by dfun3                                       d_aCo = dfun3 d_aCq
  Wanted:  d_aCq : Default [Expression]
  Derived: d_aCr : Data DefaultD [Expression]    d_aCr = $p1 d_aCq  {irrelevant}

  by dfun5                                       d_aCq = dfun5 aCu
  Wanted:  d_aCu : Default Expression
  Derived: d_aCw : Data DefaultD Expression      d_aCw = $p1 d_aCu
  Derived: d_aCx : Sat (DefaultD Expression)     d_aCx = $p1 d_aCw
     -- These two deriveds are unnecessary,
     -- and dangerous, because we later satisfy
     -- d_aCu from dfun4 which does not visibly
     -- depend on d_aCl

Now we satisfy                                   d_aCu = dfun4
                                                 d_aCp = d_aCx
Result = disaster:
                d_aCp = d_aCx
                      = $p1 d_aCw
                      = $p1 ($p1 d_aCu)
                      = $p1 ($p1 dfun4)
                      = $p1 ($p1 (MkDefault d_aCl ...))
                      = $p1 d_aCl
                      = $p1 (dfun2 d_aCo d_aCp)
                      = d_aCp
-}
