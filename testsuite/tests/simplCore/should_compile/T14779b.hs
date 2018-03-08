{-# OPTIONS_GHC -g -O #-}
-- This used to fail with:
--
-- *** Core Lint errors : in result of Simplifier ***
-- <no location info>: warning:
--     [RHS of str_s2UI :: Addr#]
--     The type of this binder is unlifted: str_s2UI
--     Binder's type: Addr#

module T14779b where

data DataType = DataType
                        { tycon   :: String
                        , datarep :: DataRep
                        }

data Constr = Constr
                        { conrep    :: ConstrRep
                        , constring :: String
                        , confields :: [String] -- for AlgRep only
                        , confixity :: Fixity   -- for AlgRep only
                        , datatype  :: DataType
                        }

data DataRep = AlgRep [Constr]
             | IntRep
             | FloatRep
             | CharRep
             | NoRep

data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
               | FloatConstr  Rational
               | CharConstr   Char

type ConIndex = Int


-- | Fixity of constructors
data Fixity = Prefix
            | Infix     -- Later: add associativity and precedence


mkDataType :: String -> [Constr] -> DataType
mkDataType str cs = DataType
                        { tycon   = str
                        , datarep = AlgRep cs
                        }

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

dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dt = case datarep dt of
                        (AlgRep cons) -> cons
                        _ -> errorWithoutStackTrace $
                               "Data.Data.dataTypeConstrs is not supported for "
                                    ++ dataTypeName dt ++
                                    ", as it is not an algebraic data type."
dataTypeName :: DataType -> String
dataTypeName = tycon

showConstr :: Constr -> String
showConstr = constring

-- | The type parameter should be an instance of 'HasResolution'.
newtype Fixed a = MkFixed Integer -- ^ @since 4.7.0.0
        deriving (Eq,Ord)

tyFixed :: DataType
tyFixed = mkDataType "Data.Fixed.Fixed" [conMkFixed]
conMkFixed :: Constr
conMkFixed = mkConstr tyFixed "MkFixed" [] Prefix
