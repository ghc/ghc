{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName' represents names as strings with just a little more information:
--   the \"namespace\" that the name came from, e.g. the namespace of value, type constructors or
--   data constructors
--
-- * 'RdrName.RdrName': see "RdrName#name_types"
--
-- * 'Name.Name': see "Name#name_types"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var': see "Var#name_types"

module GHC.Types.Name.Occurrence (
        -- * The 'NameSpace' type
        NameSpace, -- Abstract

        nameSpacesRelated,

        -- ** Construction
        -- $real_vs_source_data_constructors
        tcName, clsName, tcClsName, dataName, varName,
        tvName, srcDataName,

        -- ** Pretty Printing
        pprNameSpace, pprNonVarNameSpace, pprNameSpaceBrief,

        -- * The 'OccName' type
        OccName,        -- Abstract, instance of Outputable
        pprOccName,

        -- ** Construction
        mkOccName, mkOccNameFS,
        mkVarOcc, mkVarOccFS,
        mkDataOcc, mkDataOccFS,
        mkTyVarOcc, mkTyVarOccFS,
        mkTcOcc, mkTcOccFS,
        mkClsOcc, mkClsOccFS,
        mkDFunOcc,
        setOccNameSpace,
        demoteOccName,
        HasOccName(..),

        -- ** Derived 'OccName's
        isDerivedOccName,
        mkDataConWrapperOcc, mkWorkerOcc,
        mkMatcherOcc, mkBuilderOcc,
        mkDefaultMethodOcc, isDefaultMethodOcc, isTypeableBindOcc,
        mkNewTyCoOcc, mkClassOpAuxOcc,
        mkCon2TagOcc, mkTag2ConOcc, mkMaxTagOcc,
        mkClassDataConOcc, mkDictOcc, mkIPOcc,
        mkSpecOcc, mkForeignExportOcc, mkRepEqOcc,
        mkGenR, mkGen1R,
        mkDataTOcc, mkDataCOcc, mkDataConWorkerOcc,
        mkSuperDictSelOcc, mkSuperDictAuxOcc,
        mkLocalOcc, mkMethodOcc, mkInstTyTcOcc,
        mkInstTyCoOcc, mkEqPredCoOcc,
        mkRecFldSelOcc,
        mkTyConRepOcc,

        -- ** Deconstruction
        occNameFS, occNameString, occNameSpace,

        isVarOcc, isTvOcc, isTcOcc, isDataOcc, isDataSymOcc, isSymOcc, isValOcc,
        parenSymOcc, startsWithUnderscore,

        isTcClsNameSpace, isTvNameSpace, isDataConNameSpace, isVarNameSpace, isValNameSpace,

        -- * The 'OccEnv' type
        OccEnv, emptyOccEnv, unitOccEnv, extendOccEnv, mapOccEnv,
        lookupOccEnv, mkOccEnv, mkOccEnv_C, extendOccEnvList, elemOccEnv,
        occEnvElts, foldOccEnv, plusOccEnv, plusOccEnv_C, extendOccEnv_C,
        extendOccEnv_Acc, filterOccEnv, delListFromOccEnv, delFromOccEnv,
        alterOccEnv, pprOccEnv,

        -- * The 'OccSet' type
        OccSet, emptyOccSet, unitOccSet, mkOccSet, extendOccSet,
        extendOccSetList,
        unionOccSets, unionManyOccSets, minusOccSet, elemOccSet,
        isEmptyOccSet, intersectOccSet, intersectsOccSet, disjointOccSet,
        filterOccSet,

        -- * Tidying up
        TidyOccEnv, emptyTidyOccEnv, initTidyOccEnv,
        tidyOccName, avoidClashesOccEnv, delTidyOccEnvList,

        -- FsEnv
        FastStringEnv, emptyFsEnv, lookupFsEnv, extendFsEnv, mkFsEnv
    ) where

import GhcPrelude

import Util
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import FastString
import FastStringEnv
import Outputable
import GHC.Utils.Lexeme
import Binary
import Control.DeepSeq
import Data.Char
import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{Name space}
*                                                                      *
************************************************************************
-}

data NameSpace = VarName        -- Variables, including "real" data constructors
               | DataName       -- "Source" data constructors
               | TvName         -- Type variables
               | TcClsName      -- Type constructors and classes; Haskell has them
                                -- in the same name space for now.
               deriving( Eq, Ord )

-- Note [Data Constructors]
-- see also: Note [Data Constructor Naming] in GHC.Core.DataCon
--
-- $real_vs_source_data_constructors
-- There are two forms of data constructor:
--
--      [Source data constructors] The data constructors mentioned in Haskell source code
--
--      [Real data constructors] The data constructors of the representation type, which may not be the same as the source type
--
-- For example:
--
-- > data T = T !(Int, Int)
--
-- The source datacon has type @(Int, Int) -> T@
-- The real   datacon has type @Int -> Int -> T@
--
-- GHC chooses a representation based on the strictness etc.

tcName, clsName, tcClsName :: NameSpace
dataName, srcDataName      :: NameSpace
tvName, varName            :: NameSpace

-- Though type constructors and classes are in the same name space now,
-- the NameSpace type is abstract, so we can easily separate them later
tcName    = TcClsName           -- Type constructors
clsName   = TcClsName           -- Classes
tcClsName = TcClsName           -- Not sure which!

dataName    = DataName
srcDataName = DataName  -- Haskell-source data constructors should be
                        -- in the Data name space

tvName      = TvName
varName     = VarName

isDataConNameSpace :: NameSpace -> Bool
isDataConNameSpace DataName = True
isDataConNameSpace _        = False

isTcClsNameSpace :: NameSpace -> Bool
isTcClsNameSpace TcClsName = True
isTcClsNameSpace _         = False

isTvNameSpace :: NameSpace -> Bool
isTvNameSpace TvName = True
isTvNameSpace _      = False

isVarNameSpace :: NameSpace -> Bool     -- Variables or type variables, but not constructors
isVarNameSpace TvName  = True
isVarNameSpace VarName = True
isVarNameSpace _       = False

isValNameSpace :: NameSpace -> Bool
isValNameSpace DataName = True
isValNameSpace VarName  = True
isValNameSpace _        = False

pprNameSpace :: NameSpace -> SDoc
pprNameSpace DataName  = text "data constructor"
pprNameSpace VarName   = text "variable"
pprNameSpace TvName    = text "type variable"
pprNameSpace TcClsName = text "type constructor or class"

pprNonVarNameSpace :: NameSpace -> SDoc
pprNonVarNameSpace VarName = empty
pprNonVarNameSpace ns = pprNameSpace ns

pprNameSpaceBrief :: NameSpace -> SDoc
pprNameSpaceBrief DataName  = char 'd'
pprNameSpaceBrief VarName   = char 'v'
pprNameSpaceBrief TvName    = text "tv"
pprNameSpaceBrief TcClsName = text "tc"

-- demoteNameSpace lowers the NameSpace if possible.  We can not know
-- in advance, since a TvName can appear in an HsTyVar.
-- See Note [Demotion] in GHC.Rename.Env
demoteNameSpace :: NameSpace -> Maybe NameSpace
demoteNameSpace VarName = Nothing
demoteNameSpace DataName = Nothing
demoteNameSpace TvName = Nothing
demoteNameSpace TcClsName = Just DataName

{-
************************************************************************
*                                                                      *
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
*                                                                      *
************************************************************************
-}

-- | Occurrence Name
--
-- In this context that means:
-- "classified (i.e. as a type name, value name, etc) but not qualified
-- and not yet resolved"
data OccName = OccName
    { occNameSpace  :: !NameSpace
    , occNameFS     :: !FastString
    }

instance Eq OccName where
    (OccName sp1 s1) == (OccName sp2 s2) = s1 == s2 && sp1 == sp2

instance Ord OccName where
        -- Compares lexicographically, *not* by Unique of the string
    compare (OccName sp1 s1) (OccName sp2 s2)
        = (s1  `compare` s2) `thenCmp` (sp1 `compare` sp2)

instance Data OccName where
  -- don't traverse?
  toConstr _   = abstractConstr "OccName"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "OccName"

instance HasOccName OccName where
  occName = id

instance NFData OccName where
  rnf x = x `seq` ()

{-
************************************************************************
*                                                                      *
\subsection{Printing}
*                                                                      *
************************************************************************
-}

instance Outputable OccName where
    ppr = pprOccName

instance OutputableBndr OccName where
    pprBndr _ = ppr
    pprInfixOcc n = pprInfixVar (isSymOcc n) (ppr n)
    pprPrefixOcc n = pprPrefixVar (isSymOcc n) (ppr n)

pprOccName :: OccName -> SDoc
pprOccName (OccName sp occ)
  = getPprStyle $ \ sty ->
    if codeStyle sty
    then ztext (zEncodeFS occ)
    else pp_occ <> pp_debug sty
  where
    pp_debug sty | debugStyle sty = braces (pprNameSpaceBrief sp)
                 | otherwise      = empty

    pp_occ = sdocOption sdocSuppressUniques $ \case
               True  -> text (strip_th_unique (unpackFS occ))
               False -> ftext occ

        -- See Note [Suppressing uniques in OccNames]
    strip_th_unique ('[' : c : _) | isAlphaNum c = []
    strip_th_unique (c : cs) = c : strip_th_unique cs
    strip_th_unique []       = []

{-
Note [Suppressing uniques in OccNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a hack to de-wobblify the OccNames that contain uniques from
Template Haskell that have been turned into a string in the OccName.
See Note [Unique OccNames from Template Haskell] in Convert.hs

************************************************************************
*                                                                      *
\subsection{Construction}
*                                                                      *
************************************************************************
-}

mkOccName :: NameSpace -> String -> OccName
mkOccName occ_sp str = OccName occ_sp (mkFastString str)

mkOccNameFS :: NameSpace -> FastString -> OccName
mkOccNameFS occ_sp fs = OccName occ_sp fs

mkVarOcc :: String -> OccName
mkVarOcc s = mkOccName varName s

mkVarOccFS :: FastString -> OccName
mkVarOccFS fs = mkOccNameFS varName fs

mkDataOcc :: String -> OccName
mkDataOcc = mkOccName dataName

mkDataOccFS :: FastString -> OccName
mkDataOccFS = mkOccNameFS dataName

mkTyVarOcc :: String -> OccName
mkTyVarOcc = mkOccName tvName

mkTyVarOccFS :: FastString -> OccName
mkTyVarOccFS fs = mkOccNameFS tvName fs

mkTcOcc :: String -> OccName
mkTcOcc = mkOccName tcName

mkTcOccFS :: FastString -> OccName
mkTcOccFS = mkOccNameFS tcName

mkClsOcc :: String -> OccName
mkClsOcc = mkOccName clsName

mkClsOccFS :: FastString -> OccName
mkClsOccFS = mkOccNameFS clsName

-- demoteOccName lowers the Namespace of OccName.
-- see Note [Demotion]
demoteOccName :: OccName -> Maybe OccName
demoteOccName (OccName space name) = do
  space' <- demoteNameSpace space
  return $ OccName space' name

-- Name spaces are related if there is a chance to mean the one when one writes
-- the other, i.e. variables <-> data constructors and type variables <-> type constructors
nameSpacesRelated :: NameSpace -> NameSpace -> Bool
nameSpacesRelated ns1 ns2 = ns1 == ns2 || otherNameSpace ns1 == ns2

otherNameSpace :: NameSpace -> NameSpace
otherNameSpace VarName = DataName
otherNameSpace DataName = VarName
otherNameSpace TvName = TcClsName
otherNameSpace TcClsName = TvName



{- | Other names in the compiler add additional information to an OccName.
This class provides a consistent way to access the underlying OccName. -}
class HasOccName name where
  occName :: name -> OccName

{-
************************************************************************
*                                                                      *
                Environments
*                                                                      *
************************************************************************

OccEnvs are used mainly for the envts in ModIfaces.

Note [The Unique of an OccName]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
They are efficient, because FastStrings have unique Int# keys.  We assume
this key is less than 2^24, and indeed FastStrings are allocated keys
sequentially starting at 0.

So we can make a Unique using
        mkUnique ns key  :: Unique
where 'ns' is a Char representing the name space.  This in turn makes it
easy to build an OccEnv.
-}

instance Uniquable OccName where
      -- See Note [The Unique of an OccName]
  getUnique (OccName VarName   fs) = mkVarOccUnique  fs
  getUnique (OccName DataName  fs) = mkDataOccUnique fs
  getUnique (OccName TvName    fs) = mkTvOccUnique   fs
  getUnique (OccName TcClsName fs) = mkTcOccUnique   fs

newtype OccEnv a = A (UniqFM a)
  deriving Data

emptyOccEnv :: OccEnv a
unitOccEnv  :: OccName -> a -> OccEnv a
extendOccEnv :: OccEnv a -> OccName -> a -> OccEnv a
extendOccEnvList :: OccEnv a -> [(OccName, a)] -> OccEnv a
lookupOccEnv :: OccEnv a -> OccName -> Maybe a
mkOccEnv     :: [(OccName,a)] -> OccEnv a
mkOccEnv_C   :: (a -> a -> a) -> [(OccName,a)] -> OccEnv a
elemOccEnv   :: OccName -> OccEnv a -> Bool
foldOccEnv   :: (a -> b -> b) -> b -> OccEnv a -> b
occEnvElts   :: OccEnv a -> [a]
extendOccEnv_C :: (a->a->a) -> OccEnv a -> OccName -> a -> OccEnv a
extendOccEnv_Acc :: (a->b->b) -> (a->b) -> OccEnv b -> OccName -> a -> OccEnv b
plusOccEnv     :: OccEnv a -> OccEnv a -> OccEnv a
plusOccEnv_C   :: (a->a->a) -> OccEnv a -> OccEnv a -> OccEnv a
mapOccEnv      :: (a->b) -> OccEnv a -> OccEnv b
delFromOccEnv      :: OccEnv a -> OccName -> OccEnv a
delListFromOccEnv :: OccEnv a -> [OccName] -> OccEnv a
filterOccEnv       :: (elt -> Bool) -> OccEnv elt -> OccEnv elt
alterOccEnv        :: (Maybe elt -> Maybe elt) -> OccEnv elt -> OccName -> OccEnv elt

emptyOccEnv      = A emptyUFM
unitOccEnv x y = A $ unitUFM x y
extendOccEnv (A x) y z = A $ addToUFM x y z
extendOccEnvList (A x) l = A $ addListToUFM x l
lookupOccEnv (A x) y = lookupUFM x y
mkOccEnv     l    = A $ listToUFM l
elemOccEnv x (A y)       = elemUFM x y
foldOccEnv a b (A c)     = foldUFM a b c
occEnvElts (A x)         = eltsUFM x
plusOccEnv (A x) (A y)   = A $ plusUFM x y
plusOccEnv_C f (A x) (A y)       = A $ plusUFM_C f x y
extendOccEnv_C f (A x) y z   = A $ addToUFM_C f x y z
extendOccEnv_Acc f g (A x) y z   = A $ addToUFM_Acc f g x y z
mapOccEnv f (A x)        = A $ mapUFM f x
mkOccEnv_C comb l = A $ addListToUFM_C comb emptyUFM l
delFromOccEnv (A x) y    = A $ delFromUFM x y
delListFromOccEnv (A x) y  = A $ delListFromUFM x y
filterOccEnv x (A y)       = A $ filterUFM x y
alterOccEnv fn (A y) k     = A $ alterUFM fn y k

instance Outputable a => Outputable (OccEnv a) where
    ppr x = pprOccEnv ppr x

pprOccEnv :: (a -> SDoc) -> OccEnv a -> SDoc
pprOccEnv ppr_elt (A env) = pprUniqFM ppr_elt env

type OccSet = UniqSet OccName

emptyOccSet       :: OccSet
unitOccSet        :: OccName -> OccSet
mkOccSet          :: [OccName] -> OccSet
extendOccSet      :: OccSet -> OccName -> OccSet
extendOccSetList  :: OccSet -> [OccName] -> OccSet
unionOccSets      :: OccSet -> OccSet -> OccSet
unionManyOccSets  :: [OccSet] -> OccSet
minusOccSet       :: OccSet -> OccSet -> OccSet
elemOccSet        :: OccName -> OccSet -> Bool
isEmptyOccSet     :: OccSet -> Bool
intersectOccSet   :: OccSet -> OccSet -> OccSet
disjointOccSet    :: OccSet -> OccSet -> Bool
intersectsOccSet  :: OccSet -> OccSet -> Bool
filterOccSet      :: (OccName -> Bool) -> OccSet -> OccSet

emptyOccSet       = emptyUniqSet
unitOccSet        = unitUniqSet
mkOccSet          = mkUniqSet
extendOccSet      = addOneToUniqSet
extendOccSetList  = addListToUniqSet
unionOccSets      = unionUniqSets
unionManyOccSets  = unionManyUniqSets
minusOccSet       = minusUniqSet
elemOccSet        = elementOfUniqSet
isEmptyOccSet     = isEmptyUniqSet
intersectOccSet   = intersectUniqSets
disjointOccSet    = disjointUniqSets
intersectsOccSet s1 s2 = not (s1 `disjointOccSet` s2)
filterOccSet      = filterUniqSet

{-
************************************************************************
*                                                                      *
\subsection{Predicates and taking them apart}
*                                                                      *
************************************************************************
-}

occNameString :: OccName -> String
occNameString (OccName _ s) = unpackFS s

setOccNameSpace :: NameSpace -> OccName -> OccName
setOccNameSpace sp (OccName _ occ) = OccName sp occ

isVarOcc, isTvOcc, isTcOcc, isDataOcc :: OccName -> Bool

isVarOcc (OccName VarName _) = True
isVarOcc _                   = False

isTvOcc (OccName TvName _) = True
isTvOcc _                  = False

isTcOcc (OccName TcClsName _) = True
isTcOcc _                     = False

-- | /Value/ 'OccNames's are those that are either in
-- the variable or data constructor namespaces
isValOcc :: OccName -> Bool
isValOcc (OccName VarName  _) = True
isValOcc (OccName DataName _) = True
isValOcc _                    = False

isDataOcc (OccName DataName _) = True
isDataOcc _                    = False

-- | Test if the 'OccName' is a data constructor that starts with
-- a symbol (e.g. @:@, or @[]@)
isDataSymOcc :: OccName -> Bool
isDataSymOcc (OccName DataName s) = isLexConSym s
isDataSymOcc _                    = False
-- Pretty inefficient!

-- | Test if the 'OccName' is that for any operator (whether
-- it is a data constructor or variable or whatever)
isSymOcc :: OccName -> Bool
isSymOcc (OccName DataName s)  = isLexConSym s
isSymOcc (OccName TcClsName s) = isLexSym s
isSymOcc (OccName VarName s)   = isLexSym s
isSymOcc (OccName TvName s)    = isLexSym s
-- Pretty inefficient!

parenSymOcc :: OccName -> SDoc -> SDoc
-- ^ Wrap parens around an operator
parenSymOcc occ doc | isSymOcc occ = parens doc
                    | otherwise    = doc

startsWithUnderscore :: OccName -> Bool
-- ^ Haskell 98 encourages compilers to suppress warnings about unused
-- names in a pattern if they start with @_@: this implements that test
startsWithUnderscore occ = headFS (occNameFS occ) == '_'

{-
************************************************************************
*                                                                      *
\subsection{Making system names}
*                                                                      *
************************************************************************

Here's our convention for splitting up the interface file name space:

   d...         dictionary identifiers
                (local variables, so no name-clash worries)

All of these other OccNames contain a mixture of alphabetic
and symbolic characters, and hence cannot possibly clash with
a user-written type or function name

   $f...        Dict-fun identifiers (from inst decls)
   $dmop        Default method for 'op'
   $pnC         n'th superclass selector for class C
   $wf          Worker for function 'f'
   $sf..        Specialised version of f
   D:C          Data constructor for dictionary for class C
   NTCo:T       Coercion connecting newtype T with its representation type
   TFCo:R       Coercion connecting a data family to its representation type R

In encoded form these appear as Zdfxxx etc

        :...            keywords (export:, letrec: etc.)
--- I THINK THIS IS WRONG!

This knowledge is encoded in the following functions.

@mk_deriv@ generates an @OccName@ from the prefix and a string.
NB: The string must already be encoded!
-}

-- | Build an 'OccName' derived from another 'OccName'.
--
-- Note that the pieces of the name are passed in as a @[FastString]@ so that
-- the whole name can be constructed with a single 'concatFS', minimizing
-- unnecessary intermediate allocations.
mk_deriv :: NameSpace
         -> FastString      -- ^ A prefix which distinguishes one sort of
                            -- derived name from another
         -> [FastString]    -- ^ The name we are deriving from in pieces which
                            -- will be concatenated.
         -> OccName
mk_deriv occ_sp sys_prefix str =
    mkOccNameFS occ_sp (concatFS $ sys_prefix : str)

isDerivedOccName :: OccName -> Bool
-- ^ Test for definitions internally generated by GHC.  This predicate
-- is used to suppress printing of internal definitions in some debug prints
isDerivedOccName occ =
   case occNameString occ of
     '$':c:_ | isAlphaNum c -> True   -- E.g.  $wfoo
     c:':':_ | isAlphaNum c -> True   -- E.g.  N:blah   newtype coercions
     _other                 -> False

isDefaultMethodOcc :: OccName -> Bool
isDefaultMethodOcc occ =
   case occNameString occ of
     '$':'d':'m':_ -> True
     _ -> False

-- | Is an 'OccName' one of a Typeable @TyCon@ or @Module@ binding?
-- This is needed as these bindings are renamed differently.
-- See Note [Grand plan for Typeable] in TcTypeable.
isTypeableBindOcc :: OccName -> Bool
isTypeableBindOcc occ =
   case occNameString occ of
     '$':'t':'c':_ -> True  -- mkTyConRepOcc
     '$':'t':'r':_ -> True  -- Module binding
     _ -> False

mkDataConWrapperOcc, mkWorkerOcc,
        mkMatcherOcc, mkBuilderOcc,
        mkDefaultMethodOcc,
        mkClassDataConOcc, mkDictOcc,
        mkIPOcc, mkSpecOcc, mkForeignExportOcc, mkRepEqOcc,
        mkGenR, mkGen1R,
        mkDataConWorkerOcc, mkNewTyCoOcc,
        mkInstTyCoOcc, mkEqPredCoOcc, mkClassOpAuxOcc,
        mkCon2TagOcc, mkTag2ConOcc, mkMaxTagOcc,
        mkTyConRepOcc
   :: OccName -> OccName

-- These derived variables have a prefix that no Haskell value could have
mkDataConWrapperOcc = mk_simple_deriv varName  "$W"
mkWorkerOcc         = mk_simple_deriv varName  "$w"
mkMatcherOcc        = mk_simple_deriv varName  "$m"
mkBuilderOcc        = mk_simple_deriv varName  "$b"
mkDefaultMethodOcc  = mk_simple_deriv varName  "$dm"
mkClassOpAuxOcc     = mk_simple_deriv varName  "$c"
mkDictOcc           = mk_simple_deriv varName  "$d"
mkIPOcc             = mk_simple_deriv varName  "$i"
mkSpecOcc           = mk_simple_deriv varName  "$s"
mkForeignExportOcc  = mk_simple_deriv varName  "$f"
mkRepEqOcc          = mk_simple_deriv tvName   "$r"   -- In RULES involving Coercible
mkClassDataConOcc   = mk_simple_deriv dataName "C:"     -- Data con for a class
mkNewTyCoOcc        = mk_simple_deriv tcName   "N:"   -- Coercion for newtypes
mkInstTyCoOcc       = mk_simple_deriv tcName   "D:"   -- Coercion for type functions
mkEqPredCoOcc       = mk_simple_deriv tcName   "$co"

-- Used in derived instances
mkCon2TagOcc        = mk_simple_deriv varName  "$con2tag_"
mkTag2ConOcc        = mk_simple_deriv varName  "$tag2con_"
mkMaxTagOcc         = mk_simple_deriv varName  "$maxtag_"

-- TyConRepName stuff; see Note [Grand plan for Typeable] in TcTypeable
mkTyConRepOcc occ = mk_simple_deriv varName prefix occ
  where
    prefix | isDataOcc occ = "$tc'"
           | otherwise     = "$tc"

-- Generic deriving mechanism
mkGenR   = mk_simple_deriv tcName "Rep_"
mkGen1R  = mk_simple_deriv tcName "Rep1_"

-- Overloaded record field selectors
mkRecFldSelOcc :: String -> OccName
mkRecFldSelOcc s = mk_deriv varName "$sel" [fsLit s]

mk_simple_deriv :: NameSpace -> FastString -> OccName -> OccName
mk_simple_deriv sp px occ = mk_deriv sp px [occNameFS occ]

-- Data constructor workers are made by setting the name space
-- of the data constructor OccName (which should be a DataName)
-- to VarName
mkDataConWorkerOcc datacon_occ = setOccNameSpace varName datacon_occ

mkSuperDictAuxOcc :: Int -> OccName -> OccName
mkSuperDictAuxOcc index cls_tc_occ
  = mk_deriv varName "$cp" [fsLit $ show index, occNameFS cls_tc_occ]

mkSuperDictSelOcc :: Int        -- ^ Index of superclass, e.g. 3
                  -> OccName    -- ^ Class, e.g. @Ord@
                  -> OccName    -- ^ Derived 'Occname', e.g. @$p3Ord@
mkSuperDictSelOcc index cls_tc_occ
  = mk_deriv varName "$p" [fsLit $ show index, occNameFS cls_tc_occ]

mkLocalOcc :: Unique            -- ^ Unique to combine with the 'OccName'
           -> OccName           -- ^ Local name, e.g. @sat@
           -> OccName           -- ^ Nice unique version, e.g. @$L23sat@
mkLocalOcc uniq occ
   = mk_deriv varName "$L" [fsLit $ show uniq, occNameFS occ]
        -- The Unique might print with characters
        -- that need encoding (e.g. 'z'!)

-- | Derive a name for the representation type constructor of a
-- @data@\/@newtype@ instance.
mkInstTyTcOcc :: String                 -- ^ Family name, e.g. @Map@
              -> OccSet                 -- ^ avoid these Occs
              -> OccName                -- ^ @R:Map@
mkInstTyTcOcc str = chooseUniqueOcc tcName ('R' : ':' : str)

mkDFunOcc :: String             -- ^ Typically the class and type glommed together e.g. @OrdMaybe@.
                                -- Only used in debug mode, for extra clarity
          -> Bool               -- ^ Is this a hs-boot instance DFun?
          -> OccSet             -- ^ avoid these Occs
          -> OccName            -- ^ E.g. @$f3OrdMaybe@

-- In hs-boot files we make dict funs like $fx7ClsTy, which get bound to the real
-- thing when we compile the mother module. Reason: we don't know exactly
-- what the  mother module will call it.

mkDFunOcc info_str is_boot set
  = chooseUniqueOcc VarName (prefix ++ info_str) set
  where
    prefix | is_boot   = "$fx"
           | otherwise = "$f"

mkDataTOcc, mkDataCOcc
  :: OccName            -- ^ TyCon or data con string
  -> OccSet             -- ^ avoid these Occs
  -> OccName            -- ^ E.g. @$f3OrdMaybe@
-- data T = MkT ... deriving( Data ) needs definitions for
--      $tT   :: Data.Generics.Basics.DataType
--      $cMkT :: Data.Generics.Basics.Constr
mkDataTOcc occ = chooseUniqueOcc VarName ("$t" ++ occNameString occ)
mkDataCOcc occ = chooseUniqueOcc VarName ("$c" ++ occNameString occ)

{-
Sometimes we need to pick an OccName that has not already been used,
given a set of in-use OccNames.
-}

chooseUniqueOcc :: NameSpace -> String -> OccSet -> OccName
chooseUniqueOcc ns str set = loop (mkOccName ns str) (0::Int)
  where
  loop occ n
   | occ `elemOccSet` set = loop (mkOccName ns (str ++ show n)) (n+1)
   | otherwise            = occ

{-
We used to add a '$m' to indicate a method, but that gives rise to bad
error messages from the type checker when we print the function name or pattern
of an instance-decl binding.  Why? Because the binding is zapped
to use the method name in place of the selector name.
(See TcClassDcl.tcMethodBind)

The way it is now, -ddump-xx output may look confusing, but
you can always say -dppr-debug to get the uniques.

However, we *do* have to zap the first character to be lower case,
because overloaded constructors (blarg) generate methods too.
And convert to VarName space

e.g. a call to constructor MkFoo where
        data (Ord a) => Foo a = MkFoo a

If this is necessary, we do it by prefixing '$m'.  These
guys never show up in error messages.  What a hack.
-}

mkMethodOcc :: OccName -> OccName
mkMethodOcc occ@(OccName VarName _) = occ
mkMethodOcc occ                     = mk_simple_deriv varName "$m" occ

{-
************************************************************************
*                                                                      *
\subsection{Tidying them up}
*                                                                      *
************************************************************************

Before we print chunks of code we like to rename it so that
we don't have to print lots of silly uniques in it.  But we mustn't
accidentally introduce name clashes!  So the idea is that we leave the
OccName alone unless it accidentally clashes with one that is already
in scope; if so, we tack on '1' at the end and try again, then '2', and
so on till we find a unique one.

There's a wrinkle for operators.  Consider '>>='.  We can't use '>>=1'
because that isn't a single lexeme.  So we encode it to 'lle' and *then*
tack on the '1', if necessary.

Note [TidyOccEnv]
~~~~~~~~~~~~~~~~~
type TidyOccEnv = UniqFM Int

* Domain = The OccName's FastString. These FastStrings are "taken";
           make sure that we don't re-use

* Int, n = A plausible starting point for new guesses
           There is no guarantee that "FSn" is available;
           you must look that up in the TidyOccEnv.  But
           it's a good place to start looking.

* When looking for a renaming for "foo2" we strip off the "2" and start
  with "foo".  Otherwise if we tidy twice we get silly names like foo23.

  However, if it started with digits at the end, we always make a name
  with digits at the end, rather than shortening "foo2" to just "foo",
  even if "foo" is unused.  Reasons:
     - Plain "foo" might be used later
     - We use trailing digits to subtly indicate a unification variable
       in typechecker error message; see TypeRep.tidyTyVarBndr

We have to take care though! Consider a machine-generated module (#10370)
  module Foo where
     a1 = e1
     a2 = e2
     ...
     a2000 = e2000
Then "a1", "a2" etc are all marked taken.  But now if we come across "a7" again,
we have to do a linear search to find a free one, "a2001".  That might just be
acceptable once.  But if we now come across "a8" again, we don't want to repeat
that search.

So we use the TidyOccEnv mapping for "a" (not "a7" or "a8") as our base for
starting the search; and we make sure to update the starting point for "a"
after we allocate a new one.


Note [Tidying multiple names at once]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider

    > :t (id,id,id)

Every id contributes a type variable to the type signature, and all of them are
"a". If we tidy them one by one, we get

    (id,id,id) :: (a2 -> a2, a1 -> a1, a -> a)

which is a bit unfortunate, as it unfairly renames only two of them. What we
would like to see is

    (id,id,id) :: (a3 -> a3, a2 -> a2, a1 -> a1)

To achieve this, the function avoidClashesOccEnv can be used to prepare the
TidyEnv, by “blocking” every name that occurs twice in the map. This way, none
of the "a"s will get the privilege of keeping this name, and all of them will
get a suitable number by tidyOccName.

This prepared TidyEnv can then be used with tidyOccName. See tidyTyCoVarBndrs
for an example where this is used.

This is #12382.

-}

type TidyOccEnv = UniqFM Int    -- The in-scope OccNames
  -- See Note [TidyOccEnv]

emptyTidyOccEnv :: TidyOccEnv
emptyTidyOccEnv = emptyUFM

initTidyOccEnv :: [OccName] -> TidyOccEnv       -- Initialise with names to avoid!
initTidyOccEnv = foldl' add emptyUFM
  where
    add env (OccName _ fs) = addToUFM env fs 1

delTidyOccEnvList :: TidyOccEnv -> [FastString] -> TidyOccEnv
delTidyOccEnvList = delListFromUFM

-- see Note [Tidying multiple names at once]
avoidClashesOccEnv :: TidyOccEnv -> [OccName] -> TidyOccEnv
avoidClashesOccEnv env occs = go env emptyUFM occs
  where
    go env _        [] = env
    go env seenOnce ((OccName _ fs):occs)
      | fs `elemUFM` env      = go env seenOnce                  occs
      | fs `elemUFM` seenOnce = go (addToUFM env fs 1) seenOnce  occs
      | otherwise             = go env (addToUFM seenOnce fs ()) occs

tidyOccName :: TidyOccEnv -> OccName -> (TidyOccEnv, OccName)
tidyOccName env occ@(OccName occ_sp fs)
  | not (fs `elemUFM` env)
  = -- Desired OccName is free, so use it,
    -- and record in 'env' that it's no longer available
    (addToUFM env fs 1, occ)

  | otherwise
  = case lookupUFM env base1 of
       Nothing -> (addToUFM env base1 2, OccName occ_sp base1)
       Just n  -> find 1 n
  where
    base :: String  -- Drop trailing digits (see Note [TidyOccEnv])
    base  = dropWhileEndLE isDigit (unpackFS fs)
    base1 = mkFastString (base ++ "1")

    find !k !n
      = case lookupUFM env new_fs of
          Just {} -> find (k+1 :: Int) (n+k)
                       -- By using n+k, the n argument to find goes
                       --    1, add 1, add 2, add 3, etc which
                       -- moves at quadratic speed through a dense patch

          Nothing -> (new_env, OccName occ_sp new_fs)
       where
         new_fs = mkFastString (base ++ show n)
         new_env = addToUFM (addToUFM env new_fs 1) base1 (n+1)
                     -- Update:  base1,  so that next time we'll start where we left off
                     --          new_fs, so that we know it is taken
                     -- If they are the same (n==1), the former wins
                     -- See Note [TidyOccEnv]


{-
************************************************************************
*                                                                      *
                Binary instance
    Here rather than in GHC.Iface.Binary because OccName is abstract
*                                                                      *
************************************************************************
-}

instance Binary NameSpace where
    put_ bh VarName = do
            putByte bh 0
    put_ bh DataName = do
            putByte bh 1
    put_ bh TvName = do
            putByte bh 2
    put_ bh TcClsName = do
            putByte bh 3
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return VarName
              1 -> do return DataName
              2 -> do return TvName
              _ -> do return TcClsName

instance Binary OccName where
    put_ bh (OccName aa ab) = do
            put_ bh aa
            put_ bh ab
    get bh = do
          aa <- get bh
          ab <- get bh
          return (OccName aa ab)
