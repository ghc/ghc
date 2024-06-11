{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'GHC.Types.Name.Occurrence.OccName' represents names as strings with just a little more information:
--   the \"namespace\" that the name came from, e.g. the namespace of value, type constructors or
--   data constructors
--
-- * 'GHC.Types.Name.Reader.RdrName': see "GHC.Types.Name.Reader#name_types"
--
-- * 'GHC.Types.Name.Name': see "GHC.Types.Name#name_types"
--
-- * 'GHC.Types.Id.Id': see "GHC.Types.Id#name_types"
--
-- * 'GHC.Types.Var.Var': see "GHC.Types.Var#name_types"

module GHC.Types.Name.Occurrence (
        -- * The 'NameSpace' type
        NameSpace, -- Abstract

        -- ** Construction
        -- $real_vs_source_data_constructors
        tcName, clsName, tcClsName, dataName, varName, fieldName,
        tvName, srcDataName,

        -- ** Pretty Printing
        pprNameSpace, pprNonVarNameSpace, pprNameSpaceBrief,

        -- * The 'OccName' type
        OccName,        -- Abstract, instance of Outputable
        pprOccName, occNameMangledFS,

        -- ** Construction
        mkOccName, mkOccNameFS,
        mkVarOcc, mkVarOccFS,
        mkRecFieldOcc, mkRecFieldOccFS,
        mkDataOcc, mkDataOccFS,
        mkTyVarOcc, mkTyVarOccFS,
        mkTcOcc, mkTcOccFS,
        mkClsOcc, mkClsOccFS,
        mkDFunOcc,
        setOccNameSpace,
        demoteOccName,
        demoteOccTvName,
        promoteOccName,
        varToRecFieldOcc,
        recFieldToVarOcc,
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
        mkTyConRepOcc,

        -- ** Deconstruction
        occNameFS, occNameString, occNameSpace,

        isVarOcc, isTvOcc, isTcOcc, isDataOcc, isDataSymOcc, isSymOcc, isValOcc,
        isFieldOcc, fieldOcc_maybe,
        parenSymOcc, startsWithUnderscore, isUnderscore,

        isTcClsNameSpace, isTvNameSpace, isDataConNameSpace, isVarNameSpace, isValNameSpace,
        isFieldNameSpace, isTermVarOrFieldNameSpace,

        -- * The 'OccEnv' type
        OccEnv, emptyOccEnv, unitOccEnv, extendOccEnv,
        mapOccEnv, strictMapOccEnv,
        mapMaybeOccEnv,
        lookupOccEnv, lookupOccEnv_AllNameSpaces,
        lookupOccEnv_WithFields, lookupFieldsOccEnv,
        mkOccEnv, mkOccEnv_C, extendOccEnvList, elemOccEnv,
        nonDetOccEnvElts, nonDetFoldOccEnv,
        plusOccEnv, plusOccEnv_C,
        extendOccEnv_Acc, filterOccEnv, delListFromOccEnv, delFromOccEnv,
        alterOccEnv, minusOccEnv, minusOccEnv_C, minusOccEnv_C_Ns,
        pprOccEnv, forceOccEnv,
        intersectOccEnv_C,

        -- * The 'OccSet' type
        OccSet, emptyOccSet, unitOccSet, mkOccSet, extendOccSet,
        extendOccSetList,
        unionOccSets, unionManyOccSets, elemOccSet,
        isEmptyOccSet,

        -- * Dealing with main
        mainOcc, ppMainFn,

        -- * Tidying up
        TidyOccEnv, emptyTidyOccEnv, initTidyOccEnv, trimTidyOccEnv,
        tidyOccName, avoidClashesOccEnv, delTidyOccEnvList,

        -- FsEnv
        FastStringEnv, emptyFsEnv, lookupFsEnv, extendFsEnv, mkFsEnv
    ) where

import GHC.Prelude

import GHC.Builtin.Uniques
import GHC.Utils.Misc
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Data.FastString
import GHC.Data.FastString.Env
import GHC.Utils.Outputable
import GHC.Utils.Lexeme
import GHC.Utils.Binary
import GHC.Utils.Panic.Plain

import Control.DeepSeq
import Data.Char
import Data.Data
import qualified Data.Semigroup as S
import GHC.Exts( Int(I#), dataToTag# )

{-
************************************************************************
*                                                                      *
\subsection{Name space}
*                                                                      *
************************************************************************
-}

data NameSpace
  -- | Variable name space (including "real" data constructors).
  = VarName
  -- | Record field namespace for the given record.
  | FldName
    { fldParent :: !FastString
      -- ^ The textual name of the parent of the field.
      --
      --   - For a field of a datatype, this is the name of the first constructor
      --     of the datatype (regardless of whether this constructor has this field).
      --   - For a field of a pattern synonym, this is the name of the pattern synonym.
    }
  -- | "Source" data constructor namespace.
  | DataName
  -- | Type variable namespace.
  | TvName
  -- | Type constructor and class namespace.
  | TcClsName
    -- Haskell has type constructors and classes in the same namespace, for now.
   deriving Eq

instance Ord NameSpace where
  compare ns1 ns2 =
    case compare (I# (dataToTag# ns1)) (I# (dataToTag# ns2)) of
      LT -> LT
      GT -> GT
      EQ
        | FldName { fldParent = p1 } <- ns1
        , FldName { fldParent = p2 } <- ns2
        -> lexicalCompareFS p1 p2
        | otherwise
        -> EQ

instance Uniquable NameSpace where
  getUnique (FldName fs) = mkFldNSUnique  fs
  getUnique VarName      = varNSUnique
  getUnique DataName     = dataNSUnique
  getUnique TvName       = tvNSUnique
  getUnique TcClsName    = tcNSUnique

instance NFData NameSpace where
  rnf VarName = ()
  rnf (FldName par) = rnf par
  rnf DataName = ()
  rnf TvName = ()
  rnf TcClsName = ()

{-
Note [Data Constructors]
~~~~~~~~~~~~~~~~~~~~~~~~
see also: Note [Data Constructor Naming] in GHC.Core.DataCon

$real_vs_source_data_constructors
There are two forms of data constructor:

     [Source data constructors] The data constructors mentioned in Haskell source code

     [Real data constructors] The data constructors of the representation type, which may not be the same as the source type

For example:

> data T = T !(Int, Int)

The source datacon has type @(Int, Int) -> T@
The real   datacon has type @Int -> Int -> T@

GHC chooses a representation based on the strictness etc.

Note [Record field namespacing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record fields have a separate namespace from variables, to support
DuplicateRecordFields, e.g. in

  data X = MkX { fld :: Int }
  data Y = MkY { fld :: Bool }

  f x = x { fld = 3 }
  g y = y { fld = False }

we want the two occurrences of "fld" to refer to the field names associated with
the corresponding data type.

The namespace for a record field is as follows:

  - for a data type, it is the textual name of the first constructor of the
    datatype, whether this constructor has this field or not;
  - for a pattern synonym, it is the textual name of the pattern synonym itself.

Record fields are initially parsed as variables, but the renamer resolves their
namespace in GHC.Rename.Names.newRecordFieldLabel, which is called when renaming
record data declarations and record pattern synonym declarations.

To illustrate the namespacing, consider the record field "fld" in the following datatype

  data instance A Int Bool Char
    = MkA1 | MkA2 { fld :: Int } | MkA3 { bar :: Bool, fld :: Int }

Its namespace is `FldName "MkA1"`. This is a convention used throughout GHC
to circumvent the fact that we don't have a way to refer to the type constructor
"A Int Bool Char" in the renamer, as data family instances only get given
'Name's in the typechecker.
-}

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

fieldName :: FastString -> NameSpace
fieldName = FldName

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
isVarNameSpace TvName       = True
isVarNameSpace VarName      = True
isVarNameSpace (FldName {}) = True
isVarNameSpace _            = False

-- | Is this a term variable or field name namespace?
isTermVarOrFieldNameSpace :: NameSpace -> Bool
isTermVarOrFieldNameSpace VarName      = True
isTermVarOrFieldNameSpace (FldName {}) = True
isTermVarOrFieldNameSpace _            = False

isValNameSpace :: NameSpace -> Bool
isValNameSpace DataName     = True
isValNameSpace VarName      = True
isValNameSpace (FldName {}) = True
isValNameSpace _            = False

isFieldNameSpace :: NameSpace -> Bool
isFieldNameSpace (FldName {}) = True
isFieldNameSpace _            = False

pprNameSpace :: NameSpace -> SDoc
pprNameSpace DataName    = text "data constructor"
pprNameSpace VarName     = text "variable"
pprNameSpace TvName      = text "type variable"
pprNameSpace TcClsName   = text "type constructor or class"
pprNameSpace (FldName p) = text "record field of" <+> ftext p

pprNonVarNameSpace :: NameSpace -> SDoc
pprNonVarNameSpace VarName = empty
pprNonVarNameSpace ns = pprNameSpace ns

pprNameSpaceBrief :: NameSpace -> SDoc
pprNameSpaceBrief DataName     = char 'd'
pprNameSpaceBrief VarName      = char 'v'
pprNameSpaceBrief TvName       = text "tv"
pprNameSpaceBrief TcClsName    = text "tc"
pprNameSpaceBrief (FldName {}) = text "fld"

-- demoteNameSpace lowers the NameSpace if possible.  We can not know
-- in advance, since a TvName can appear in an HsTyVar.
-- See Note [Demotion] in GHC.Rename.Env.
demoteNameSpace :: NameSpace -> Maybe NameSpace
demoteNameSpace VarName = Nothing
demoteNameSpace DataName = Nothing
demoteNameSpace TvName = Nothing
demoteNameSpace TcClsName = Just DataName
demoteNameSpace (FldName {}) = Nothing

-- demoteTvNameSpace lowers the NameSpace of a type variable.
-- See Note [Demotion] in GHC.Rename.Env.
demoteTvNameSpace :: NameSpace -> Maybe NameSpace
demoteTvNameSpace TvName = Just VarName
demoteTvNameSpace VarName = Nothing
demoteTvNameSpace DataName = Nothing
demoteTvNameSpace TcClsName = Nothing
demoteTvNameSpace (FldName {}) = Nothing

-- promoteNameSpace promotes the NameSpace as follows.
-- See Note [Promotion] in GHC.Rename.Env.
promoteNameSpace :: NameSpace -> Maybe NameSpace
promoteNameSpace DataName = Just TcClsName
promoteNameSpace VarName = Just TvName
promoteNameSpace TcClsName = Nothing
promoteNameSpace TvName = Nothing
promoteNameSpace (FldName {}) = Nothing

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
    compare (OccName sp1 s1) (OccName sp2 s2) =
      lexicalCompareFS s1 s2 S.<> compare sp1 sp2

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

pprOccName :: IsLine doc => OccName -> doc
pprOccName (OccName sp occ)
  = docWithStyle (ztext (zEncodeFS occ))
    (\_ -> ftext occ <> whenPprDebug (braces (pprNameSpaceBrief sp)))
{-# SPECIALIZE pprOccName :: OccName -> SDoc #-}
{-# SPECIALIZE pprOccName :: OccName -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Mangle field names to avoid duplicate symbols.
--
-- See Note [Mangling OccNames].
occNameMangledFS :: OccName -> FastString
occNameMangledFS (OccName ns fs) =
  case ns of
    -- Fields need to include the constructor, to ensure that we don't define
    -- duplicate symbols when using DuplicateRecordFields.
    FldName con -> concatFS [fsLit "$fld:", con, ":", fs]
    -- Otherwise, we can ignore the namespace, as there is no risk of name
    -- clashes.
    _           -> fs

{- Note [Mangling OccNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating a symbol for a Name, we usually discard the NameSpace entirely
(see GHC.Types.Name.pprName). This is because clashes are usually not possible,
e.g. a variable and a data constructor can't clash because data constructors
start with a capital letter or a colon, while variables never do.

However, record field names, in the presence of DuplicateRecordFields, need this
disambiguation. So, for a record field like

  data A = MkA { foo :: Int }

we generate the symbol $fld:MkA:foo. We use the constructor 'MkA' to disambiguate,
and not the TyCon A as one might naively expect: this is explained in
Note [Record field namespacing].
-}

{-
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

mkRecFieldOcc :: FastString -> String -> OccName
mkRecFieldOcc dc = mkOccName (fieldName dc)

mkRecFieldOccFS :: FastString -> FastString -> OccName
mkRecFieldOccFS dc = mkOccNameFS (fieldName dc)

varToRecFieldOcc :: HasDebugCallStack => FastString -> OccName -> OccName
varToRecFieldOcc dc (OccName ns s) =
  assert makes_sense $ mkRecFieldOccFS dc s
    where
      makes_sense = case ns of
        VarName    -> True
        FldName {} -> True
          -- NB: it's OK to change the parent data constructor,
          -- see e.g. test T23220 in which we construct with TH
          -- a datatype using the fields of a different datatype.
        _          -> False

recFieldToVarOcc :: HasDebugCallStack => OccName -> OccName
recFieldToVarOcc (OccName _ns s) = mkVarOccFS s

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
-- See Note [Demotion] in GHC.Rename.Env.
demoteOccName :: OccName -> Maybe OccName
demoteOccName (OccName space name) = do
  space' <- demoteNameSpace space
  return $ OccName space' name

demoteOccTvName :: OccName -> Maybe OccName
demoteOccTvName (OccName space name) = do
  space' <- demoteTvNameSpace space
  return $ OccName space' name

-- promoteOccName promotes the NameSpace of OccName.
-- See Note [Promotion] in GHC.Rename.Env.
promoteOccName :: OccName -> Maybe OccName
promoteOccName (OccName space name) = do
  promoted_space <- promoteNameSpace space
  let tyop   = isTvNameSpace promoted_space && isLexVarSym name
      space' = if tyop then tcClsName else promoted_space   -- special case for type operators (#24570)
  return $ OccName space' name

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

OccEnvs are used for the GlobalRdrEnv and for the envts in ModIface.

Note [OccEnv]
~~~~~~~~~~~~~
An OccEnv is a map keyed on OccName. Recall that an OccEnv consists of two
components:

  - a namespace,
  - a textual name (in the form of a FastString).

In general, for a given textual name, there is only one appropriate namespace.
However, sometimes we do get an occurrence that belongs to several namespaces:

  - Symbolic identifiers such as (:+) can belong to both the data constructor and
    type constructor/class namespaces.
  - With duplicate record fields, a field name can belong to several different
    namespaces, one for each parent datatype (or pattern synonym).

So we represent an OccEnv as a nested data structure

  FastStringEnv (UniqFM NameSpace a)

in which we can first look up the textual name, and then choose which of the
namespaces are relevant. This supports the two main uses of OccEnvs:

  1. One wants to look up a specific OccName in the environment, at a specific
     namespace. One looks up the textual name, and then the namespace.
  2. One wants to look up something, but isn't sure in advance of the namespace.
     So one looks up the textual name, and then can decide what to do based on
     the returned map of namespaces.

This data structure isn't performance critical in most situations, but some
improvements to its performance that might be worth it are as follows:

  A. Use a tailor-made data structure for a map keyed on NameSpaces.

     Recall that we have:

        data IntMap a = Bin !Int !Int !(IntMap a) !(IntMap a)
                      | Tip !Key a
                      | Nil

     This is already pretty efficient for singletons, but we don't need the
     empty case (as we would simply omit the parent key in the OccEnv instead
     of storing an empty inner map).

  B. Always ensure the inner map (keyed on namespaces) is evaluated, i.e.
     is never a thunk. For this, we would need to use strict operations on
     the outer FastStringEnv (but we'd keep using lazy operations on the inner
     UniqFM).
-}

-- | A map keyed on 'OccName'. See Note [OccEnv].
newtype OccEnv a = MkOccEnv (FastStringEnv (UniqFM NameSpace a))
  deriving Functor

-- | The empty 'OccEnv'.
emptyOccEnv :: OccEnv a
emptyOccEnv = MkOccEnv emptyFsEnv

-- | A singleton 'OccEnv'.
unitOccEnv :: OccName -> a -> OccEnv a
unitOccEnv (OccName ns s) a = MkOccEnv $ unitFsEnv s (unitUFM ns a)

-- | Add a single element to an 'OccEnv'.
extendOccEnv :: OccEnv a -> OccName -> a -> OccEnv a
extendOccEnv (MkOccEnv as) (OccName ns s) a =
  MkOccEnv $ extendFsEnv_C plusUFM as s (unitUFM ns a)

-- | Extend an 'OccEnv' by a list.
--
-- 'OccName's later on in the list override earlier 'OccName's.
extendOccEnvList :: OccEnv a -> [(OccName, a)] -> OccEnv a
extendOccEnvList = foldl' $ \ env (occ, a) -> extendOccEnv env occ a

-- | Look an element up in an 'OccEnv'.
lookupOccEnv :: OccEnv a -> OccName -> Maybe a
lookupOccEnv (MkOccEnv as) (OccName ns s)
  = do { m <- lookupFsEnv as s
       ; lookupUFM m ns }

-- | Lookup an element in an 'OccEnv', ignoring 'NameSpace's entirely.
lookupOccEnv_AllNameSpaces :: OccEnv a -> OccName -> [a]
lookupOccEnv_AllNameSpaces (MkOccEnv as) (OccName _ s)
  = case lookupFsEnv as s of
      Nothing -> []
      Just r  -> nonDetEltsUFM r

-- | Lookup an element in an 'OccEnv', looking in the record field
-- namespace for a variable.
lookupOccEnv_WithFields :: OccEnv a -> OccName -> [a]
lookupOccEnv_WithFields env occ =
  case lookupOccEnv env occ of
      Nothing  -> fieldGREs
      Just gre -> gre : fieldGREs
  where
    fieldGREs
      -- If the 'OccName' is a variable, also look up
      -- in the record field namespaces.
      | isVarOcc occ
      = lookupFieldsOccEnv env (occNameFS occ)
      | otherwise
      = []

-- | Look up all the record fields that match with the given 'FastString'
-- in an 'OccEnv'.
lookupFieldsOccEnv :: OccEnv a -> FastString -> [a]
lookupFieldsOccEnv (MkOccEnv as) fld =
  case lookupFsEnv as fld of
    Nothing   -> []
    Just flds -> nonDetEltsUFM $ filter_flds flds
  -- NB: non-determinism is OK: in practice we will either end up resolving
  -- to a single field or throwing an error.
  where
    filter_flds = filterUFM_Directly (\ uniq _ -> isFldNSUnique uniq)

-- | Create an 'OccEnv' from a list.
--
-- 'OccName's later on in the list override earlier 'OccName's.
mkOccEnv :: [(OccName,a)] -> OccEnv a
mkOccEnv = extendOccEnvList emptyOccEnv

-- | Create an 'OccEnv' from a list, combining different values
-- with the same 'OccName' using the combining function.
mkOccEnv_C :: (a -> a -> a) -- ^ old -> new -> result
           -> [(OccName,a)]
           -> OccEnv a
mkOccEnv_C f elts
  = MkOccEnv $ foldl' g emptyFsEnv elts
    where
      g env (OccName ns s, a) =
        extendFsEnv_C (plusUFM_C $ flip f) env s (unitUFM ns a)

-- | Compute whether there is a value keyed by the given 'OccName'.
elemOccEnv :: OccName -> OccEnv a -> Bool
elemOccEnv (OccName ns s) (MkOccEnv as)
  = case lookupFsEnv as s of
      Nothing -> False
      Just m  -> ns `elemUFM` m

-- | Fold over an 'OccEnv'. Non-deterministic, unless the folding function
-- is commutative (i.e. @a1 `f` ( a2 `f` b ) == a2 `f` ( a1 `f` b )@ for all @a1@, @a2@, @b@).
nonDetFoldOccEnv :: (a -> b -> b) -> b -> OccEnv a -> b
nonDetFoldOccEnv f b0 (MkOccEnv as) =
  nonDetFoldFsEnv (flip $ nonDetFoldUFM f) b0 as

-- | Obtain the elements of an 'OccEnv'.
--
-- The resulting order is non-deterministic.
nonDetOccEnvElts :: OccEnv a -> [a]
nonDetOccEnvElts = nonDetFoldOccEnv (:) []

-- | Union of two 'OccEnv's, right-biased.
plusOccEnv :: OccEnv a -> OccEnv a -> OccEnv a
plusOccEnv (MkOccEnv env1) (MkOccEnv env2)
  = MkOccEnv $ plusFsEnv_C plusUFM env1 env2

-- | Union of two 'OccEnv's with a combining function.
plusOccEnv_C :: (a->a->a) -> OccEnv a -> OccEnv a -> OccEnv a
plusOccEnv_C f (MkOccEnv env1) (MkOccEnv env2)
  = MkOccEnv $ plusFsEnv_C (plusUFM_C f) env1 env2

-- | Map over an 'OccEnv' ('Functor' instance).
mapOccEnv :: (a->b) -> OccEnv a -> OccEnv b
mapOccEnv = fmap

-- | 'mapMaybe' for b 'OccEnv'.
mapMaybeOccEnv :: (a -> Maybe b) -> OccEnv a -> OccEnv b
mapMaybeOccEnv f (MkOccEnv env)
  = MkOccEnv $ mapMaybeUFM g env
    where
      g as =
        case mapMaybeUFM f as of
          m' | isNullUFM m' -> Nothing
             | otherwise    -> Just m'

-- | Add a single element to an 'OccEnv', using a different function whether
-- the 'OccName' already exists or not.
extendOccEnv_Acc :: forall a b
                 .  (a->b->b)    -- ^ add to existing
                 -> (a->b)       -- ^ new element
                 -> OccEnv b     -- ^ old
                 -> OccName -> a -- ^ new
                 -> OccEnv b
extendOccEnv_Acc f g (MkOccEnv env) (OccName ns s) =
  MkOccEnv . extendFsEnv_Acc f' g' env s
    where
     f' :: a -> UniqFM NameSpace b -> UniqFM NameSpace b
     f' a bs = alterUFM (Just . \ case { Nothing -> g a ; Just b -> f a b }) bs ns
     g' a = unitUFM ns (g a)

-- | Delete one element from an 'OccEnv'.
delFromOccEnv :: forall a. OccEnv a -> OccName -> OccEnv a
delFromOccEnv (MkOccEnv env1) (OccName ns s) =
  MkOccEnv $ alterFsEnv f env1 s
    where
      f :: Maybe (UniqFM NameSpace a) -> Maybe (UniqFM NameSpace a)
      f Nothing = Nothing
      f (Just m) =
        case delFromUFM m ns of
          m' | isNullUFM m' -> Nothing
             | otherwise    -> Just m'

-- | Delete multiple elements from an 'OccEnv'.
delListFromOccEnv :: OccEnv a -> [OccName] -> OccEnv a
delListFromOccEnv = foldl' delFromOccEnv

-- | Filter out all elements in an 'OccEnv' using a predicate.
filterOccEnv :: forall a. (a -> Bool) -> OccEnv a -> OccEnv a
filterOccEnv f (MkOccEnv env) =
  MkOccEnv $ mapMaybeFsEnv g env
    where
      g :: UniqFM NameSpace a -> Maybe (UniqFM NameSpace a)
      g ms =
        case filterUFM f ms of
          m' | isNullUFM m' -> Nothing
             | otherwise    -> Just m'

-- | Alter an 'OccEnv', adding or removing an element at the given key.
alterOccEnv :: forall a. (Maybe a -> Maybe a) -> OccEnv a -> OccName -> OccEnv a
alterOccEnv f (MkOccEnv env) (OccName ns s) =
  MkOccEnv $ alterFsEnv g env s
    where
      g :: Maybe (UniqFM NameSpace a) -> Maybe (UniqFM NameSpace a)
      g Nothing  = fmap (unitUFM ns) (f Nothing)
      g (Just m) =
        case alterUFM f m ns of
          m' | isNullUFM m' -> Nothing
             | otherwise    -> Just m'

intersectOccEnv_C :: (a -> b -> c) -> OccEnv a -> OccEnv b -> OccEnv c
intersectOccEnv_C f (MkOccEnv as) (MkOccEnv bs)
  = MkOccEnv $ intersectUFM_C (intersectUFM_C f) as bs

-- | Remove elements of the first 'OccEnv' that appear in the second 'OccEnv'.
minusOccEnv :: OccEnv a -> OccEnv b -> OccEnv a
minusOccEnv = minusOccEnv_C_Ns minusUFM

-- | Alters (replaces or removes) those elements of the first 'OccEnv' that are
-- mentioned in the second 'OccEnv'.
--
-- Same idea as 'Data.Map.differenceWith'.
minusOccEnv_C :: (a -> b -> Maybe a)
              -> OccEnv a -> OccEnv b -> OccEnv a
minusOccEnv_C f = minusOccEnv_C_Ns (minusUFM_C f)

minusOccEnv_C_Ns :: forall a b
                 .  (UniqFM NameSpace a -> UniqFM NameSpace b -> UniqFM NameSpace a)
                 -> OccEnv a -> OccEnv b -> OccEnv a
minusOccEnv_C_Ns f (MkOccEnv as) (MkOccEnv bs) =
  MkOccEnv $ minusUFM_C g as bs
    where
      g :: UniqFM NameSpace a -> UniqFM NameSpace b -> Maybe (UniqFM NameSpace a)
      g as bs =
        let m = f as bs
        in if isNullUFM m
           then Nothing
           else Just m

instance Outputable a => Outputable (OccEnv a) where
    ppr x = pprOccEnv ppr x

pprOccEnv :: (a -> SDoc) -> OccEnv a -> SDoc
pprOccEnv ppr_elt (MkOccEnv env)
    = brackets $ fsep $ punctuate comma $
    [ ppr uq <+> text ":->" <+> ppr_elt elt
    | (uq, elts) <- nonDetUFMToList env
    , elt <- nonDetEltsUFM elts ]

instance NFData a => NFData (OccEnv a) where
  rnf = forceOccEnv rnf

-- | Map over an 'OccEnv' strictly.
strictMapOccEnv :: (a -> b) -> OccEnv a -> OccEnv b
strictMapOccEnv f (MkOccEnv as) =
  MkOccEnv $ strictMapFsEnv (strictMapUFM f) as

-- | Force an 'OccEnv' with the provided function.
forceOccEnv :: (a -> ()) -> OccEnv a -> ()
forceOccEnv nf (MkOccEnv fs) = seqEltsUFM (seqEltsUFM nf) fs

--------------------------------------------------------------------------------

newtype OccSet = OccSet (FastStringEnv (UniqSet NameSpace))

emptyOccSet       :: OccSet
unitOccSet        :: OccName -> OccSet
mkOccSet          :: [OccName] -> OccSet
extendOccSet      :: OccSet -> OccName -> OccSet
extendOccSetList  :: OccSet -> [OccName] -> OccSet
unionOccSets      :: OccSet -> OccSet -> OccSet
unionManyOccSets  :: [OccSet] -> OccSet
elemOccSet        :: OccName -> OccSet -> Bool
isEmptyOccSet     :: OccSet -> Bool

emptyOccSet       = OccSet emptyFsEnv
unitOccSet (OccName ns s) = OccSet $ unitFsEnv s (unitUniqSet ns)
mkOccSet          = extendOccSetList emptyOccSet
extendOccSet      (OccSet occs) (OccName ns s) = OccSet $ extendFsEnv occs s (unitUniqSet ns)
extendOccSetList  = foldl' extendOccSet
unionOccSets      (OccSet xs) (OccSet ys) = OccSet $ plusFsEnv_C unionUniqSets xs ys
unionManyOccSets  = foldl' unionOccSets emptyOccSet
elemOccSet (OccName ns s) (OccSet occs) = maybe False (elementOfUniqSet ns) $ lookupFsEnv occs s
isEmptyOccSet     (OccSet occs) = isNullUFM occs

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

isVarOcc, isTvOcc, isTcOcc, isDataOcc, isFieldOcc :: OccName -> Bool

isVarOcc (OccName VarName _) = True
isVarOcc _                   = False

isTvOcc (OccName TvName _) = True
isTvOcc _                  = False

isTcOcc (OccName TcClsName _) = True
isTcOcc _                     = False

isFieldOcc (OccName (FldName {}) _) = True
isFieldOcc _                        = False

fieldOcc_maybe :: OccName -> Maybe FastString
fieldOcc_maybe (OccName (FldName con) _) = Just con
fieldOcc_maybe _                         = Nothing

-- | /Value/ 'OccNames's are those that are either in
-- the variable, field name or data constructor namespaces
isValOcc :: OccName -> Bool
isValOcc (OccName VarName      _) = True
isValOcc (OccName DataName     _) = True
isValOcc (OccName (FldName {}) _) = True
isValOcc _                        = False

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
isSymOcc (OccName ns s) = case ns of
  DataName   -> isLexConSym s
  TcClsName  -> isLexSym s
  VarName    -> isLexSym s
  TvName     -> isLexSym s
  FldName {} -> isLexSym s
-- Pretty inefficient!

parenSymOcc :: OccName -> SDoc -> SDoc
-- ^ Wrap parens around an operator
parenSymOcc occ doc | isSymOcc occ = parens doc
                    | otherwise    = doc

startsWithUnderscore :: OccName -> Bool
-- ^ Haskell 98 encourages compilers to suppress warnings about unused
-- names in a pattern if they start with @_@: this implements that test
startsWithUnderscore occ = case unpackFS (occNameFS occ) of
  '_':_ -> True
  _     -> False

isUnderscore :: OccName -> Bool
isUnderscore occ = occNameFS occ == fsLit "_"

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
-- See Note [Grand plan for Typeable] in "GHC.Tc.Instance.Typeable".
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
        mkCon2TagOcc, mkTag2ConOcc, mkMaxTagOcc, mkDataTOcc, mkDataCOcc,
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
mkClassDataConOcc   = mk_simple_deriv dataName "C:"   -- Data con for a class
mkNewTyCoOcc        = mk_simple_deriv tcName   "N:"   -- Coercion for newtypes
mkInstTyCoOcc       = mk_simple_deriv tcName   "D:"   -- Coercion for type functions
mkEqPredCoOcc       = mk_simple_deriv tcName   "$co"

-- Used in derived instances for the names of auxiliary bindings.
-- See Note [Auxiliary binders] in GHC.Tc.Deriv.Generate.
mkCon2TagOcc        = mk_simple_deriv varName  "$con2tag_"
mkTag2ConOcc        = mk_simple_deriv varName  "$tag2con_"
mkMaxTagOcc         = mk_simple_deriv varName  "$maxtag_"
mkDataTOcc          = mk_simple_deriv varName  "$t"
mkDataCOcc          = mk_simple_deriv varName  "$c"

-- TyConRepName stuff; see Note [Grand plan for Typeable] in GHC.Tc.Instance.Typeable
mkTyConRepOcc occ = mk_simple_deriv varName prefix occ
  where
    prefix | isDataOcc occ = "$tc'"
           | otherwise     = "$tc"

-- Generic deriving mechanism
mkGenR   = mk_simple_deriv tcName "Rep_"
mkGen1R  = mk_simple_deriv tcName "Rep1_"

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
(See GHC.Tc.TyCl.Class.tcMethodBind)

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
type TidyOccEnv = UniqFM FastString Int

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
get a suitable number by tidyOccName.  Thus

   avoidNameClashesOccEnv ["a" :-> 7] ["b", "a", "c", "b", "a"]
     = ["a" :-> 7, "b" :-> 1]

Here
* "a" is already the TidyOccEnv, and so is unaffected
* "b" occurs twice, so is blocked by adding "b" :-> 1
* "c" occurs only once, and so is not affected.

This prepared TidyEnv can then be used with tidyOccName. See tidyTyCoVarBndrs
for an example where this is used.

This is #12382.

-}

type TidyOccEnv = UniqFM FastString Int    -- The in-scope OccNames
  -- See Note [TidyOccEnv]

emptyTidyOccEnv :: TidyOccEnv
emptyTidyOccEnv = emptyUFM

initTidyOccEnv :: [OccName] -> TidyOccEnv       -- Initialise with names to avoid!
initTidyOccEnv = foldl' add emptyUFM
  where
    add env (OccName _ fs) = addToUFM env fs 1

delTidyOccEnvList :: TidyOccEnv -> [OccName] -> TidyOccEnv
delTidyOccEnvList env occs = env `delListFromUFM` map occNameFS occs

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
      = case elemUFM new_fs env of
          True -> find (k+1 :: Int) (n+k)
                       -- By using n+k, the n argument to find goes
                       --    1, add 1, add 2, add 3, etc which
                       -- moves at quadratic speed through a dense patch

          False -> (new_env, OccName occ_sp new_fs)
       where
         new_fs = mkFastString (base ++ show n)
         new_env = addToUFM (addToUFM env new_fs 1) base1 (n+1)
                     -- Update:  base1,  so that next time we'll start where we left off
                     --          new_fs, so that we know it is taken
                     -- If they are the same (n==1), the former wins
                     -- See Note [TidyOccEnv]

trimTidyOccEnv :: TidyOccEnv -> [OccName] -> TidyOccEnv
-- Restrict the env to just the [OccName]
trimTidyOccEnv env vs
  = foldl' add emptyUFM vs
  where
    add :: TidyOccEnv -> OccName -> TidyOccEnv
    add so_far (OccName _ fs)
      = case lookupUFM env fs of
          Just n  -> addToUFM so_far fs n
          Nothing -> so_far

{-
************************************************************************
*                                                                      *
                            Utilies for "main"
*                                                                      *
************************************************************************
-}

mainOcc :: OccName
mainOcc = mkVarOccFS (fsLit "main")

ppMainFn :: OccName -> SDoc
ppMainFn main_occ
  | main_occ == mainOcc
  = text "IO action" <+> quotes (ppr main_occ)
  | otherwise
  = text "main IO action" <+> quotes (ppr main_occ)

{-
************************************************************************
*                                                                      *
                Binary instance
    Here rather than in GHC.Iface.Binary because OccName is abstract
*                                                                      *
************************************************************************
-}

instance Binary NameSpace where
    put_ bh VarName =
            putByte bh 0
    put_ bh DataName =
            putByte bh 1
    put_ bh TvName =
            putByte bh 2
    put_ bh TcClsName =
            putByte bh 3
    put_ bh (FldName parent) = do
            putByte bh 4
            put_ bh parent
    get bh = do
            h <- getByte bh
            case h of
              0 -> return VarName
              1 -> return DataName
              2 -> return TvName
              3 -> return TcClsName
              _ -> do
                parent <- get bh
                return $ FldName { fldParent = parent }

instance Binary OccName where
    put_ bh (OccName aa ab) = do
            put_ bh aa
            put_ bh ab
    get bh = do
          aa <- get bh
          ab <- get bh
          return (OccName aa ab)
