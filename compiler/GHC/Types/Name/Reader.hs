{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'GHC.Types.Name.Occurrence.OccName': see "GHC.Types.Name.Occurrence#name_types"
--
-- * 'GHC.Types.Name.Reader.RdrName' is the type of names that come directly from the parser. They
--   have not yet had their scoping and binding resolved by the renamer and can be
--   thought of to a first approximation as an 'GHC.Types.Name.Occurrence.OccName' with an optional module
--   qualifier
--
-- * 'GHC.Types.Name.Name': see "GHC.Types.Name#name_types"
--
-- * 'GHC.Types.Id.Id': see "GHC.Types.Id#name_types"
--
-- * 'GHC.Types.Var.Var': see "GHC.Types.Var#name_types"

module GHC.Types.Name.Reader (
        -- * The main type
        RdrName(..),    -- Constructors exported only to GHC.Iface.Binary

        -- ** Construction
        mkRdrUnqual, mkRdrQual,
        mkUnqual, mkVarUnqual, mkQual, mkOrig,
        nameRdrName, getRdrName,

        -- ** Destruction
        rdrNameOcc, rdrNameSpace, demoteRdrName, demoteRdrNameTv, promoteRdrName,
        isRdrDataCon, isRdrTyVar, isRdrTc, isQual, isQual_maybe, isUnqual,
        isOrig, isOrig_maybe, isExact, isExact_maybe, isSrcRdrName,

        -- * Local mapping of 'RdrName' to 'Name.Name'
        LocalRdrEnv, emptyLocalRdrEnv, extendLocalRdrEnv, extendLocalRdrEnvList,
        lookupLocalRdrEnv, lookupLocalRdrOcc,
        elemLocalRdrEnv, inLocalRdrEnvScope,
        localRdrEnvElts, minusLocalRdrEnv, minusLocalRdrEnvList,

        -- * Global mapping of 'RdrName' to 'GlobalRdrElt's
        GlobalRdrEnvX, GlobalRdrEnv, IfGlobalRdrEnv,
        emptyGlobalRdrEnv, mkGlobalRdrEnv, plusGlobalRdrEnv,
        extendGlobalRdrEnv, greOccName,
        pprGlobalRdrEnv, globalRdrEnvElts, globalRdrEnvLocal,

        -- ** Looking up 'GlobalRdrElt's
        FieldsOrSelectors(..), filterFieldGREs, allowGRE,

        LookupGRE(..), lookupGRE,
        WhichGREs(.., AllRelevantGREs, RelevantGREsFOS),
        greIsRelevant,
        LookupChild(..),

        lookupGRE_Name,
        lookupGRE_FieldLabel,
        getGRE_NameQualifier_maybes,
        transformGREs, pickGREs, pickGREsModExp,

        -- * GlobalRdrElts
        availFromGRE,
        greRdrNames, greSrcSpan, greQualModName,
        gresToAvailInfo,
        greDefinitionModule, greDefinitionSrcSpan,
        greFieldLabel_maybe,

        -- ** Global 'RdrName' mapping elements: 'GlobalRdrElt', 'Provenance', 'ImportSpec'
        GlobalRdrEltX(..), GlobalRdrElt, IfGlobalRdrElt, FieldGlobalRdrElt,
        greName, greNameSpace, greParent, greInfo,
        plusGRE, insertGRE,
        forceGlobalRdrEnv, hydrateGlobalRdrEnv,
        isLocalGRE, isImportedGRE, isRecFldGRE,
        fieldGREInfo,
        isDuplicateRecFldGRE, isNoFieldSelectorGRE, isFieldSelectorGRE,
        unQualOK, qualSpecOK, unQualSpecOK,
        pprNameProvenance,
        mkGRE, mkExactGRE, mkLocalGRE, mkLocalVanillaGRE, mkLocalTyConGRE,
        mkLocalConLikeGRE, mkLocalFieldGREs,
        gresToNameSet,

        -- ** Shadowing
        greClashesWith, shadowNames,

        -- ** Information attached to a 'GlobalRdrElt'
        ConLikeName(..),
        GREInfo(..), RecFieldInfo(..),
        plusGREInfo,
        recFieldConLike_maybe, recFieldInfo_maybe,
        fieldGRE_maybe, fieldGRELabel,

        -- ** Parent information
        Parent(..), greParent_maybe,
        mkParent, availParent,
        ImportSpec(..), ImpDeclSpec(..), ImpItemSpec(..),
        importSpecLoc, importSpecModule, isExplicitItem, bestImport,

        -- * Utils
        opIsAt
  ) where

import GHC.Prelude

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.Maybe

import GHC.Types.Avail
import GHC.Types.Basic
import GHC.Types.GREInfo
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.Name.Env
    ( NameEnv, nonDetNameEnvElts, emptyNameEnv, extendNameEnv_Acc )
import GHC.Types.Name.Set
import GHC.Types.PkgQual
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Builtin.Uniques ( isFldNSUnique )

import GHC.Unit.Module

import GHC.Utils.Misc as Utils
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.DeepSeq
import Control.Monad ( guard )
import Data.Data
import Data.List ( sort )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Semigroup as S
import System.IO.Unsafe ( unsafePerformIO )

{-
************************************************************************
*                                                                      *
\subsection{The main data type}
*                                                                      *
************************************************************************
-}

-- | Reader Name
--
-- Do not use the data constructors of RdrName directly: prefer the family
-- of functions that creates them, such as 'mkRdrUnqual'
--
-- - Note: A Located RdrName will only have API Annotations if it is a
--         compound one,
--   e.g.
--
-- > `bar`
-- > ( ~ )
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnType',
--           'GHC.Parser.Annotation.AnnOpen'  @'('@ or @'['@ or @'[:'@,
--           'GHC.Parser.Annotation.AnnClose' @')'@ or @']'@ or @':]'@,,
--           'GHC.Parser.Annotation.AnnBackquote' @'`'@,
--           'GHC.Parser.Annotation.AnnVal'
--           'GHC.Parser.Annotation.AnnTilde',

-- For details on above see Note [exact print annotations] in "GHC.Parser.Annotation"
data RdrName
  = Unqual OccName
        -- ^ Unqualified  name
        --
        -- Used for ordinary, unqualified occurrences, e.g. @x@, @y@ or @Foo@.
        -- Create such a 'RdrName' with 'mkRdrUnqual'

  | Qual ModuleName OccName
        -- ^ Qualified name
        --
        -- A qualified name written by the user in
        -- /source/ code.  The module isn't necessarily
        -- the module where the thing is defined;
        -- just the one from which it is imported.
        -- Examples are @Bar.x@, @Bar.y@ or @Bar.Foo@.
        -- Create such a 'RdrName' with 'mkRdrQual'

  | Orig Module OccName
        -- ^ Original name
        --
        -- An original name; the module is the /defining/ module.
        -- This is used when GHC generates code that will be fed
        -- into the renamer (e.g. from deriving clauses), but where
        -- we want to say \"Use Prelude.map dammit\". One of these
        -- can be created with 'mkOrig'

  | Exact Name
        -- ^ Exact name
        --
        -- We know exactly the 'Name'. This is used:
        --
        --  (1) When the parser parses built-in syntax like @[]@
        --      and @(,)@, but wants a 'RdrName' from it
        --
        --  (2) By Template Haskell, when TH has generated a unique name
        --
        -- Such a 'RdrName' can be created by using 'getRdrName' on a 'Name'
  deriving Data

{-
************************************************************************
*                                                                      *
\subsection{Simple functions}
*                                                                      *
************************************************************************
-}

instance HasOccName RdrName where
  occName = rdrNameOcc

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (Qual _ occ) = occ
rdrNameOcc (Unqual occ) = occ
rdrNameOcc (Orig _ occ) = occ
rdrNameOcc (Exact name) = nameOccName name

rdrNameSpace :: RdrName -> NameSpace
rdrNameSpace = occNameSpace . rdrNameOcc

-- demoteRdrName lowers the NameSpace of RdrName.
-- See Note [Demotion] in GHC.Rename.Env
demoteRdrName :: RdrName -> Maybe RdrName
demoteRdrName (Unqual occ) = fmap Unqual (demoteOccName occ)
demoteRdrName (Qual m occ) = fmap (Qual m) (demoteOccName occ)
demoteRdrName (Orig _ _) = Nothing
demoteRdrName (Exact _) = Nothing

demoteRdrNameTv :: RdrName -> Maybe RdrName
demoteRdrNameTv (Unqual occ) = fmap Unqual (demoteOccTvName occ)
demoteRdrNameTv (Qual m occ) = fmap (Qual m) (demoteOccTvName occ)
demoteRdrNameTv (Orig _ _) = Nothing
demoteRdrNameTv (Exact _) = Nothing

-- promoteRdrName promotes the NameSpace of RdrName.
-- See Note [Promotion] in GHC.Rename.Env.
promoteRdrName :: RdrName -> Maybe RdrName
promoteRdrName (Unqual occ) = fmap Unqual (promoteOccName occ)
promoteRdrName (Qual m occ) = fmap (Qual m) (promoteOccName occ)
promoteRdrName (Orig _ _) = Nothing
promoteRdrName (Exact _)  = Nothing

        -- These two are the basic constructors
mkRdrUnqual :: OccName -> RdrName
mkRdrUnqual occ = Unqual occ

mkRdrQual :: ModuleName -> OccName -> RdrName
mkRdrQual mod occ = Qual mod occ

mkOrig :: Module -> OccName -> RdrName
mkOrig mod occ = Orig mod occ

---------------
        -- These two are used when parsing source files
        -- They do encode the module and occurrence names
mkUnqual :: NameSpace -> FastString -> RdrName
mkUnqual sp n = Unqual (mkOccNameFS sp n)

mkVarUnqual :: FastString -> RdrName
mkVarUnqual n = Unqual (mkVarOccFS n)

-- | Make a qualified 'RdrName' in the given namespace and where the 'ModuleName' and
-- the 'OccName' are taken from the first and second elements of the tuple respectively
mkQual :: NameSpace -> (FastString, FastString) -> RdrName
mkQual sp (m, n) = Qual (mkModuleNameFS m) (mkOccNameFS sp n)

getRdrName :: NamedThing thing => thing -> RdrName
getRdrName name = nameRdrName (getName name)

nameRdrName :: Name -> RdrName
nameRdrName name = Exact name
-- Keep the Name even for Internal names, so that the
-- unique is still there for debug printing, particularly
-- of Types (which are converted to IfaceTypes before printing)

nukeExact :: Name -> RdrName
nukeExact n
  | isExternalName n = Orig (nameModule n) (nameOccName n)
  | otherwise        = Unqual (nameOccName n)

isRdrDataCon :: RdrName -> Bool
isRdrTyVar   :: RdrName -> Bool
isRdrTc      :: RdrName -> Bool

isRdrDataCon rn = isDataOcc (rdrNameOcc rn)
isRdrTyVar   rn = isTvOcc   (rdrNameOcc rn)
isRdrTc      rn = isTcOcc   (rdrNameOcc rn)

isSrcRdrName :: RdrName -> Bool
isSrcRdrName (Unqual _) = True
isSrcRdrName (Qual _ _) = True
isSrcRdrName _          = False

isUnqual :: RdrName -> Bool
isUnqual (Unqual _) = True
isUnqual _          = False

isQual :: RdrName -> Bool
isQual (Qual _ _) = True
isQual _          = False

isQual_maybe :: RdrName -> Maybe (ModuleName, OccName)
isQual_maybe (Qual m n) = Just (m,n)
isQual_maybe _          = Nothing

isOrig :: RdrName -> Bool
isOrig (Orig _ _) = True
isOrig _          = False

isOrig_maybe :: RdrName -> Maybe (Module, OccName)
isOrig_maybe (Orig m n) = Just (m,n)
isOrig_maybe _          = Nothing

isExact :: RdrName -> Bool
isExact (Exact _) = True
isExact _         = False

isExact_maybe :: RdrName -> Maybe Name
isExact_maybe (Exact n) = Just n
isExact_maybe _         = Nothing

{-
************************************************************************
*                                                                      *
\subsection{Instances}
*                                                                      *
************************************************************************
-}

instance Outputable RdrName where
    ppr (Exact name)   = ppr name
    ppr (Unqual occ)   = ppr occ
    ppr (Qual mod occ) = ppr mod <> dot <> ppr occ
    ppr (Orig mod occ) = getPprStyle (\sty -> pprModulePrefix sty mod occ <> ppr occ)

instance OutputableBndr RdrName where
    pprBndr _ n
        | isTvOcc (rdrNameOcc n) = char '@' <> ppr n
        | otherwise              = ppr n

    pprInfixOcc  rdr = pprInfixVar  (isSymOcc (rdrNameOcc rdr)) (ppr rdr)
    pprPrefixOcc rdr
      | Just name <- isExact_maybe rdr = pprPrefixName name
             -- pprPrefixName has some special cases, so
             -- we delegate to them rather than reproduce them
      | otherwise = pprPrefixVar (isSymOcc (rdrNameOcc rdr)) (ppr rdr)

instance Eq RdrName where
    (Exact n1)    == (Exact n2)    = n1==n2
        -- Convert exact to orig
    (Exact n1)    == r2@(Orig _ _) = nukeExact n1 == r2
    r1@(Orig _ _) == (Exact n2)    = r1 == nukeExact n2

    (Orig m1 o1)  == (Orig m2 o2)  = m1==m2 && o1==o2
    (Qual m1 o1)  == (Qual m2 o2)  = m1==m2 && o1==o2
    (Unqual o1)   == (Unqual o2)   = o1==o2
    _             == _             = False

instance Ord RdrName where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }

        -- Exact < Unqual < Qual < Orig
        -- [Note: Apr 2004] We used to use nukeExact to convert Exact to Orig
        --      before comparing so that Prelude.map == the exact Prelude.map, but
        --      that meant that we reported duplicates when renaming bindings
        --      generated by Template Haskell; e.g
        --      do { n1 <- newName "foo"; n2 <- newName "foo";
        --           <decl involving n1,n2> }
        --      I think we can do without this conversion
    compare (Exact n1) (Exact n2) = n1 `compare` n2
    compare (Exact _)  _          = LT

    compare (Unqual _)   (Exact _)    = GT
    compare (Unqual o1)  (Unqual  o2) = o1 `compare` o2
    compare (Unqual _)   _            = LT

    compare (Qual _ _)   (Exact _)    = GT
    compare (Qual _ _)   (Unqual _)   = GT
    compare (Qual m1 o1) (Qual m2 o2) = compare o1 o2 S.<> compare m1 m2
    compare (Qual _ _)   (Orig _ _)   = LT

    compare (Orig m1 o1) (Orig m2 o2) = compare o1 o2 S.<> compare m1 m2
    compare (Orig _ _)   _            = GT

{-
************************************************************************
*                                                                      *
                        LocalRdrEnv
*                                                                      *
************************************************************************
-}

{- Note [LocalRdrEnv]
~~~~~~~~~~~~~~~~~~~~~
The LocalRdrEnv is used to store local bindings (let, where, lambda, case).

* It is keyed by OccName, because we never use it for qualified names.

* It maps the OccName to a Name.  That Name is almost always an
  Internal Name, but (hackily) it can be External too for top-level
  pattern bindings.  See Note [bindLocalNames for an External name]
  in GHC.Rename.Pat

* We keep the current mapping (lre_env), *and* the set of all Names in
  scope (lre_in_scope).  Reason: see Note [Splicing Exact names] in
  GHC.Rename.Env.
-}

-- | Local Reader Environment
-- See Note [LocalRdrEnv]
data LocalRdrEnv = LRE { lre_env      :: OccEnv Name
                       , lre_in_scope :: NameSet }

instance Outputable LocalRdrEnv where
  ppr (LRE {lre_env = env, lre_in_scope = ns})
    = hang (text "LocalRdrEnv {")
         2 (vcat [ text "env =" <+> pprOccEnv ppr_elt env
                 , text "in_scope ="
                    <+> pprUFM (getUniqSet ns) (braces . pprWithCommas ppr)
                 ] <+> char '}')
    where
      ppr_elt name = parens (ppr (nameOccName name)) <+> ppr name
                     -- So we can see if the keys line up correctly

emptyLocalRdrEnv :: LocalRdrEnv
emptyLocalRdrEnv = LRE { lre_env = emptyOccEnv
                       , lre_in_scope = emptyNameSet }

extendLocalRdrEnv :: LocalRdrEnv -> Name -> LocalRdrEnv
-- See Note [LocalRdrEnv]
extendLocalRdrEnv lre@(LRE { lre_env = env, lre_in_scope = ns }) name
  = lre { lre_env      = extendOccEnv env (nameOccName name) name
        , lre_in_scope = extendNameSet ns name }

extendLocalRdrEnvList :: LocalRdrEnv -> [Name] -> LocalRdrEnv
-- See Note [LocalRdrEnv]
extendLocalRdrEnvList lre@(LRE { lre_env = env, lre_in_scope = ns }) names
  = lre { lre_env = extendOccEnvList env [(nameOccName n, n) | n <- names]
        , lre_in_scope = extendNameSetList ns names }

lookupLocalRdrEnv :: LocalRdrEnv -> RdrName -> Maybe Name
lookupLocalRdrEnv (LRE { lre_env = env, lre_in_scope = ns }) rdr
  | Unqual occ <- rdr
  = lookupOccEnv env occ

  -- See Note [Local bindings with Exact Names]
  | Exact name <- rdr
  , name `elemNameSet` ns
  = Just name

  | otherwise
  = Nothing

lookupLocalRdrOcc :: LocalRdrEnv -> OccName -> Maybe Name
lookupLocalRdrOcc (LRE { lre_env = env }) occ = lookupOccEnv env occ

elemLocalRdrEnv :: RdrName -> LocalRdrEnv -> Bool
elemLocalRdrEnv rdr_name (LRE { lre_env = env, lre_in_scope = ns })
  = case rdr_name of
      Unqual occ -> occ  `elemOccEnv` env
      Exact name -> name `elemNameSet` ns  -- See Note [Local bindings with Exact Names]
      Qual {} -> False
      Orig {} -> False

localRdrEnvElts :: LocalRdrEnv -> [Name]
localRdrEnvElts (LRE { lre_env = env }) = nonDetOccEnvElts env

inLocalRdrEnvScope :: Name -> LocalRdrEnv -> Bool
-- This is the point of the NameSet
inLocalRdrEnvScope name (LRE { lre_in_scope = ns }) = name `elemNameSet` ns

minusLocalRdrEnv :: LocalRdrEnv -> OccEnv a -> LocalRdrEnv
minusLocalRdrEnv lre@(LRE { lre_env = env }) occs
  = lre { lre_env = minusOccEnv env occs }

minusLocalRdrEnvList :: LocalRdrEnv -> [OccName] -> LocalRdrEnv
minusLocalRdrEnvList lre@(LRE { lre_env = env }) occs
  = lre { lre_env = delListFromOccEnv env occs }

{-
Note [Local bindings with Exact Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With Template Haskell we can make local bindings that have Exact Names.
Computing shadowing etc may use elemLocalRdrEnv (at least it certainly
does so in GHC.Rename.HsType.bindHsQTyVars), so for an Exact Name we must consult
the in-scope-name-set.


************************************************************************
*                                                                      *
                        GlobalRdrEnv
*                                                                      *
************************************************************************
-}

-- | Global Reader Environment
type GlobalRdrEnv = GlobalRdrEnvX GREInfo
-- ^ Keyed by 'OccName'; when looking up a qualified name
-- we look up the 'OccName' part, and then check the 'Provenance'
-- to see if the appropriate qualification is valid.  This
-- saves routinely doubling the size of the env by adding both
-- qualified and unqualified names to the domain.
--
-- The list in the codomain is required because there may be name clashes
-- These only get reported on lookup, not on construction
--
-- INVARIANT 1: All the members of the list have distinct
--              'gre_name' fields; that is, no duplicate Names
--
-- INVARIANT 2: Imported provenance => Name is an ExternalName
--              However LocalDefs can have an InternalName.  This
--              happens only when type-checking a [d| ... |] Template
--              Haskell quotation; see this note in GHC.Rename.Names
--              Note [Top-level Names in Template Haskell decl quotes]
--
-- INVARIANT 3: If the GlobalRdrEnv maps [occ -> gre], then
--                 greOccName gre = occ

-- | A 'GlobalRdrEnv' in which the 'GlobalRdrElt's don't have any 'GREInfo'
-- attached to them. This is useful to avoid space leaks, see Note [IfGlobalRdrEnv].
type IfGlobalRdrEnv = GlobalRdrEnvX ()

-- | Parametrises 'GlobalRdrEnv' over the presence or absence of 'GREInfo'.
--
-- See Note [IfGlobalRdrEnv].
type GlobalRdrEnvX info = OccEnv [GlobalRdrEltX info]

-- | Global Reader Element
--
-- Something in scope in the renamer; usually a member of the 'GlobalRdrEnv'.
-- See Note [GlobalRdrElt provenance].

type GlobalRdrElt   = GlobalRdrEltX GREInfo

-- | A 'GlobalRdrElt' in which we stripped out the 'GREInfo' field,
-- in order to avoid space leaks.
--
-- See Note [IfGlobalRdrEnv].
type IfGlobalRdrElt = GlobalRdrEltX ()

-- | Global Reader Element
--
-- Something in scope in the renamer; usually a member of the 'GlobalRdrEnv'.
-- See Note [GlobalRdrElt provenance].
--
-- Why do we parametrise over the 'gre_info' field? See Note [IfGlobalRdrEnv].
data GlobalRdrEltX info
  = GRE { gre_name :: !Name
        , gre_par  :: !Parent            -- ^ See Note [Parents]
        , gre_lcl  :: !Bool              -- ^ True <=> the thing was defined locally
        , gre_imp  :: !(Bag ImportSpec)  -- ^ In scope through these imports
  -- See Note [GlobalRdrElt provenance] for the relation between gre_lcl and gre_imp.

        , gre_info :: info
            -- ^ Information the renamer knows about this particular 'Name'.
            --
            -- Careful about forcing this field! Forcing it can trigger
            -- the loading of interface files.
            --
            -- Note [Retrieving the GREInfo from interfaces] in GHC.Types.GREInfo.
    } deriving (Data)

instance NFData a => NFData (GlobalRdrEltX a) where
  rnf (GRE name par _ imp info) = rnf name `seq` rnf par `seq` rnf imp `seq` rnf info


{- Note [IfGlobalRdrEnv]
~~~~~~~~~~~~~~~~~~~~~~~~
Information pertinent to the renamer about a 'Name' is stored in the fields of
'GlobalRdrElt'. The 'gre_info' field, described in Note [GREInfo] in GHC.Types.GREInfo,
is a bit special: as Note [Retrieving the GREInfo from interfaces] in GHC.Types.GREInfo
describes, for imported 'Name's it is usually obtained by a look up in a type environment,
and forcing can cause the interface file for the module defining the 'Name' to be
loaded. As described in Note [Forcing GREInfo] in GHC.Types.GREInfo, keeping it
a thunk can cause space leaks, while forcing it can cause extra work to be done.
So it's best to discard it when we don't need it, for example when we are about
to store it in a 'ModIface'.

We thus parametrise 'GlobalRdrElt' (and 'GlobalRdrEnv') over the presence or
absence of the 'GREInfo' field.

  - When we are about to stash the 'GlobalRdrElt' in a long-lived data structure,
    e.g. a 'ModIface', we force it by setting all the 'GREInfo' fields to '()'.
    See 'forceGlobalRdrEnv'.
  - To go back the other way, we use 'hydrateGlobalRdrEnv', which sets the
    'gre_info' fields back to lazy lookups.

This parametrisation also helps ensure that we don't accidentally force the
GREInfo field (which can cause unnecessary loading of interface files).
In particular, the 'lookupGRE' function is statically guaranteed to not consult
the 'GREInfo' field when using 'SameNameSpace', which is important
as we sometimes need to use this function with an 'IfaceGlobalRdrEnv' in which
the 'GREInfo' fields have been stripped.
-}

-- | A 'FieldGlobalRdrElt' is a 'GlobalRdrElt'
-- in which the 'gre_info' field is 'IAmRecField'.
type FieldGlobalRdrElt = GlobalRdrElt

greName :: GlobalRdrEltX info -> Name
greName = gre_name

greNameSpace :: GlobalRdrEltX info -> NameSpace
greNameSpace = nameNameSpace . greName

greParent :: GlobalRdrEltX info -> Parent
greParent = gre_par

greInfo :: GlobalRdrElt -> GREInfo
greInfo = gre_info

-- | See Note [Parents]
data Parent = NoParent
            | ParentIs  { par_is :: !Name }
            deriving (Eq, Data)

instance Outputable Parent where
   ppr NoParent        = empty
   ppr (ParentIs n)    = text "parent:" <> ppr n

instance NFData Parent where
  rnf NoParent = ()
  rnf (ParentIs n) = rnf n

plusParent :: Parent -> Parent -> Parent
-- See Note [Combining parents]
plusParent p1@(ParentIs _)    p2 = hasParent p1 p2
plusParent p1 p2@(ParentIs _)    = hasParent p2 p1
plusParent NoParent NoParent     = NoParent

hasParent :: Parent -> Parent -> Parent
#if defined(DEBUG)
hasParent p NoParent = p
hasParent p p'
  | p /= p' = pprPanic "hasParent" (ppr p <+> ppr p')  -- Parents should agree
#endif
hasParent p _  = p


{- Note [GlobalRdrElt provenance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The gre_lcl and gre_imp fields of a GlobalRdrElt describe its "provenance",
i.e. how the Name came to be in scope.  It can be in scope in one of the following
three ways:

  A. The Name was locally bound, in the current module.
     gre_lcl = True

     The renamer adds this Name to the GlobalRdrEnv after renaming the binding.
     See the calls to "extendGlobalRdrEnvRn" in GHC.Rename.Module.rnSrcDecls.

  B. The Name was imported
     gre_imp = Just imps <=> brought into scope by the imports "imps"

     The renamer adds this Name to the GlobalRdrEnv after processing the imports.
     See GHC.Rename.Names.filterImports and GHC.Tc.Module.tcRnImports.

  C. We followed an exact reference (i.e. an Exact or Orig RdrName)
     gre_lcl = False, gre_imp = Nothing

     In this case, we directly fetch a Name and its GREInfo from direct reference.
     We don't add it to the GlobalRdrEnv. See "GHC.Rename.Env.lookupExactOrOrig".

It is just about possible to have *both* gre_lcl = True and gre_imp = Just imps.
This can happen with module loops: a Name is defined locally in A, and also
brought into scope by importing a module that SOURCE-imported A.

Example (#7672):

 A.hs-boot   module A where
               data T

 B.hs        module B(Decl.T) where
               import {-# SOURCE #-} qualified A as Decl

 A.hs        module A where
               import qualified B
               data T = Z | S B.T

In A.hs, 'T' is locally bound, *and* imported as B.T.


Note [Parents]
~~~~~~~~~~~~~~~~~
The children of a Name are the things that are abbreviated by the ".." notation
in export lists.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Parent           Children
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data T           Data constructors
                   Record-field ids

  data family T    Data constructors and record-field ids
                   of all visible data instances of T

  class C          Class operations
                   Associated type constructors

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Constructor      Meaning
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  NoParent         Not bundled with a type constructor.
  ParentIs n       Bundled with the type constructor corresponding to n.

Pattern synonym constructors (and their record fields, if any) are unusual:
their gre_par is NoParent in the module in which they are defined.  However, a
pattern synonym can be bundled with a type constructor on export, in which case
whenever the pattern synonym is imported the gre_par will be ParentIs.

Thus the gre_name and gre_par fields are independent, because a normal datatype
introduces FieldGlobalRdrElts using ParentIs, but a record pattern synonym can
introduce FieldGlobalRdrElts that use NoParent. (In the past we represented
fields using an additional constructor of the Parent type, which could not
adequately represent this situation.) See also
Note [Representing pattern synonym fields in AvailInfo] in GHC.Types.Avail.

Note [Combining parents]
~~~~~~~~~~~~~~~~~~~~~~~~
With an associated type we might have
   module M where
     class C a where
       data T a
       op :: T a -> a
     instance C Int where
       data T Int = TInt
     instance C Bool where
       data T Bool = TBool

Then:   C is the parent of T
        T is the parent of TInt and TBool
So: in an export list
    C(..) is short for C( op, T )
    T(..) is short for T( TInt, TBool )

Module M exports everything, so its exports will be
   AvailTC C [C,T,op]
   AvailTC T [T,TInt,TBool]
On import we convert to GlobalRdrElt and then combine
those.  For T that will mean we have
  one GRE with Parent C
  one GRE with NoParent
That's why plusParent picks the "best" case.
-}

mkGRE :: (Name -> Maybe ImportSpec) -> GREInfo -> Parent -> Name -> GlobalRdrElt
mkGRE prov_fn info par n =
  case prov_fn n of
      -- Nothing => bound locally
      -- Just is => imported from 'is'
    Nothing -> GRE { gre_name = n, gre_par = par
                   , gre_lcl = True, gre_imp = emptyBag
                   , gre_info = info }
    Just is -> GRE { gre_name = n, gre_par = par
                   , gre_lcl = False, gre_imp = unitBag is
                   , gre_info = info }

mkExactGRE :: Name -> GREInfo -> GlobalRdrElt
mkExactGRE nm info =
  GRE { gre_name = nm, gre_par = NoParent
      , gre_lcl = False, gre_imp = emptyBag
      , gre_info = info }

mkLocalGRE :: GREInfo -> Parent -> Name -> GlobalRdrElt
mkLocalGRE = mkGRE (const Nothing)

mkLocalVanillaGRE :: Parent -> Name -> GlobalRdrElt
mkLocalVanillaGRE = mkLocalGRE Vanilla

-- | Create a local 'GlobalRdrElt' for a 'TyCon'.
mkLocalTyConGRE :: TyConFlavour Name
              -> Name
              -> GlobalRdrElt
mkLocalTyConGRE flav nm = mkLocalGRE (IAmTyCon flav) par nm
  where
    par = case tyConFlavourAssoc_maybe flav of
      Nothing -> NoParent
      Just p  -> ParentIs p

mkLocalConLikeGRE :: Parent -> (ConLikeName, ConInfo) -> GlobalRdrElt
mkLocalConLikeGRE p (con_nm, con_info) =
  mkLocalGRE (IAmConLike con_info) p (conLikeName_Name con_nm )

mkLocalFieldGREs :: Parent -> [(ConLikeName, ConInfo)] -> [GlobalRdrElt]
mkLocalFieldGREs p cons =
  [ mkLocalGRE (IAmRecField fld_info) p fld_nm
  | (S.Arg fld_nm fl, fl_cons) <- flds
  , let fld_info = RecFieldInfo { recFieldLabel = fl
                                , recFieldCons  = fl_cons } ]
  where
    -- We are given a map taking a constructor to its fields, but we want
    -- a map taking a field to the constructors which have it.
    -- We thus need to convert [(Con, [Field])] into [(Field, [Con])].
    flds = Map.toList
         $ Map.fromListWith unionUniqSets
         [ (S.Arg (flSelector fl) fl, unitUniqSet con)
         | (con, con_info) <- cons
         , ConHasRecordFields fls <- [con_info]
         , fl <- NE.toList fls ]

instance HasOccName (GlobalRdrEltX info) where
  occName = greOccName

greOccName :: GlobalRdrEltX info -> OccName
greOccName ( GRE { gre_name = nm } ) = nameOccName nm

-- | The SrcSpan of the name pointed to by the GRE.
greDefinitionSrcSpan :: GlobalRdrEltX info -> SrcSpan
greDefinitionSrcSpan = nameSrcSpan . greName

-- | The module in which the name pointed to by the GRE is defined.
greDefinitionModule :: GlobalRdrEltX info -> Maybe Module
greDefinitionModule = nameModule_maybe . greName

greQualModName :: Outputable info => GlobalRdrEltX info -> ModuleName
-- Get a suitable module qualifier for the GRE
-- (used in mkPrintUnqualified)
-- Precondition: the gre_name is always External
greQualModName gre@(GRE { gre_lcl = lcl, gre_imp = iss })
 | lcl, Just mod <- greDefinitionModule gre = moduleName mod
 | Just is <- headMaybe iss                 = is_as (is_decl is)
 | otherwise                                = pprPanic "greQualModName" (ppr gre)

greRdrNames :: GlobalRdrEltX info -> [RdrName]
greRdrNames gre@GRE{ gre_lcl = lcl, gre_imp = iss }
  = bagToList $ (if lcl then unitBag unqual else emptyBag) `unionBags` concatMapBag do_spec (mapBag is_decl iss)
  where
    occ    = greOccName gre
    unqual = Unqual occ
    do_spec decl_spec
        | is_qual decl_spec = unitBag qual
        | otherwise         = listToBag [unqual,qual]
        where qual = Qual (is_as decl_spec) occ

-- the SrcSpan that pprNameProvenance prints out depends on whether
-- the Name is defined locally or not: for a local definition the
-- definition site is used, otherwise the location of the import
-- declaration.  We want to sort the export locations in
-- exportClashErr by this SrcSpan, we need to extract it:
greSrcSpan :: Outputable info => GlobalRdrEltX info -> SrcSpan
greSrcSpan gre@(GRE { gre_lcl = lcl, gre_imp = iss } )
  | lcl           = greDefinitionSrcSpan gre
  | Just is <- headMaybe iss = is_dloc (is_decl is)
  | otherwise     = pprPanic "greSrcSpan" (ppr gre)

mkParent :: Name -> AvailInfo -> Parent
mkParent _ (Avail _)                 = NoParent
mkParent n (AvailTC m _) | n == m    = NoParent
                         | otherwise = ParentIs m

availParent :: AvailInfo -> Parent
availParent (AvailTC m _) = ParentIs m
availParent (Avail {})    = NoParent


greParent_maybe :: GlobalRdrEltX info -> Maybe Name
greParent_maybe gre = case gre_par gre of
                        NoParent      -> Nothing
                        ParentIs n    -> Just n

gresToNameSet :: [GlobalRdrEltX info] -> NameSet
gresToNameSet gres = foldr add emptyNameSet gres
  where add gre set = extendNameSet set (greName gre)

-- | Takes a list of distinct GREs and folds them
-- into AvailInfos. This is more efficient than mapping each individual
-- GRE to an AvailInfo and then folding using `plusAvail`, but needs the
-- uniqueness assumption.
gresToAvailInfo :: forall info. [GlobalRdrEltX info] -> [AvailInfo]
gresToAvailInfo gres
  = nonDetNameEnvElts avail_env
  where
    avail_env :: NameEnv AvailInfo -- Keyed by the parent
    (avail_env, _) = foldl' add (emptyNameEnv, emptyNameSet) gres

    add :: (NameEnv AvailInfo, NameSet)
        -> GlobalRdrEltX info
        -> (NameEnv AvailInfo, NameSet)
    add (env, done) gre
      | name `elemNameSet` done
      = (env, done)  -- Don't insert twice into the AvailInfo
      | otherwise
      = ( extendNameEnv_Acc comb availFromGRE env key gre
        , done `extendNameSet` name )
      where
        name = greName gre
        key = case greParent_maybe gre of
                 Just parent -> parent
                 Nothing     -> greName gre

        -- We want to insert the child `k` into a list of children but
        -- need to maintain the invariant that the parent is first.
        --
        -- We also use the invariant that `k` is not already in `ns`.
        insertChildIntoChildren :: Name -> [Name] -> Name -> [Name]
        insertChildIntoChildren _ [] k = [k]
        insertChildIntoChildren p (n:ns) k
          | p == k    = k:n:ns
          | otherwise = n:k:ns

        comb :: GlobalRdrEltX info -> AvailInfo -> AvailInfo
        comb _   (Avail n) = Avail n -- Duplicated name, should not happen
        comb gre (AvailTC m ns)
          = case gre_par gre of
              NoParent    -> AvailTC m (greName gre:ns) -- Not sure this ever happens
              ParentIs {} -> AvailTC m (insertChildIntoChildren m ns (greName gre))

availFromGRE :: GlobalRdrEltX info -> AvailInfo
availFromGRE (GRE { gre_name = child, gre_par = parent })
  = case parent of
      ParentIs p
        -> AvailTC p [child]
      NoParent
        | isTyConName child -- NB: don't force the GREInfo field unnecessarily.
        -> AvailTC child [child]
        | otherwise
        -> Avail child

emptyGlobalRdrEnv :: GlobalRdrEnvX info
emptyGlobalRdrEnv = emptyOccEnv

globalRdrEnvElts :: GlobalRdrEnvX info -> [GlobalRdrEltX info]
globalRdrEnvElts env = nonDetFoldOccEnv (++) [] env

globalRdrEnvLocal :: GlobalRdrEnvX info -> GlobalRdrEnvX info
globalRdrEnvLocal = mapOccEnv (filter isLocalGRE)

-- | Drop all 'GREInfo' fields in a 'GlobalRdrEnv' in order to
-- avoid space leaks.
-- See Note [Forcing GREInfo] in GHC.Types.GREInfo.
forceGlobalRdrEnv :: GlobalRdrEnvX info -> IfGlobalRdrEnv
forceGlobalRdrEnv rdrs =
  strictMapOccEnv (strictMap (\ gre -> gre { gre_info = ()})) rdrs

-- | Hydrate a previously dehydrated 'GlobalRdrEnv',
-- by (lazily!) looking up the 'GREInfo' using the provided function.
--
-- See Note [Forcing GREInfo] in GHC.Types.GREInfo.
hydrateGlobalRdrEnv :: forall info noInfo
                    .  (Name -> IO info)
                    -> GlobalRdrEnvX noInfo -> GlobalRdrEnvX info
hydrateGlobalRdrEnv f = mapOccEnv (fmap g)
  where
    g gre = gre { gre_info = unsafePerformIO $ f (greName gre) }
    -- NB: use unsafePerformIO to delay the lookup until it is forced.
    -- See also 'GHC.Rename.Env.lookupGREInfo'.

instance Outputable info => Outputable (GlobalRdrEltX info) where
  ppr gre = hang (ppr (greName gre) <+> ppr (gre_par gre) <+> ppr (gre_info gre))
               2 (pprNameProvenance gre)

pprGlobalRdrEnv :: Bool -> GlobalRdrEnv -> SDoc
pprGlobalRdrEnv locals_only env
  = vcat [ text "GlobalRdrEnv" <+> ppWhen locals_only (text "(locals only)")
             <+> lbrace
         , nest 2 (vcat [ pp (remove_locals gre_list) | gre_list <- nonDetOccEnvElts env ]
             <+> rbrace) ]
  where
    remove_locals gres | locals_only = filter isLocalGRE gres
                       | otherwise   = gres
    pp []   = empty
    pp gres@(gre:_) = hang (ppr occ <> colon)
                         2 (vcat (map ppr gres))
      where
        occ = nameOccName (greName gre)

{-
Note [NoFieldSelectors]
~~~~~~~~~~~~~~~~~~~~~~~
The NoFieldSelectors extension allows record fields to be defined without
bringing the corresponding selector functions into scope.  However, such fields
may still be used in contexts such as record construction, pattern matching or
update. This requires us to distinguish contexts in which selectors are required
from those in which any field may be used.  For example:

  {-# LANGUAGE NoFieldSelectors #-}
  module M (T(foo), foo) where  -- T(foo) refers to the field,
                                -- unadorned foo to the value binding
    data T = MkT { foo :: Int }
    foo = ()

    bar = foo -- refers to the value binding, field ignored

  module N where
    import M (T(..))
    baz = MkT { foo = 3 } -- refers to the field
    oops = foo -- an error: the field is in scope but the value binding is not

Each 'FieldLabel' indicates (in the 'flHasFieldSelector' field) whether the
FieldSelectors extension was enabled in the defining module.  This allows them
to be filtered out by 'filterFieldGREs'.

Even when NoFieldSelectors is in use, we still generate selector functions
internally. For example, the expression
   getField @"foo" t
or (with dot-notation)
   t.foo
extracts the `foo` field of t::T, and hence needs the selector function
(see Note [HasField instances] in GHC.Tc.Instance.Class).

In many of the name lookup functions in this module we pass a FieldsOrSelectors
value, indicating what we are looking for:

 * WantNormal: fields are in scope only if they have an accompanying selector
   function, e.g. we are looking up a variable in an expression
   (lookupExprOccRn).

 * WantBoth: any name or field will do, regardless of whether the selector
   function is available, e.g. record updates (lookupRecUpdFields) with
   NoDisambiguateRecordFields.

 * WantField: any field will do, regardless of whether the selector function is
   available, but ignoring any non-field names, e.g. record updates
   (lookupRecUpdFields with DisambiguateRecordFields.

-----------------------------------------------------------------------------------
  Context                                  FieldsOrSelectors
-----------------------------------------------------------------------------------
  Record construction/pattern match        WantField, but unless DisambiguateRecordFields
  e.g. MkT { foo = 3 }                     is in effect, also look up using WantBoth
  Record update, e.g. e { foo = 3 }        to report when a non-field clashes with a field.

  :info in GHCi                            WantBoth

  Variable occurrence in expression        WantNormal
  Type variable, data constructor
  Pretty much everything else
-----------------------------------------------------------------------------------
-}

fieldGRE_maybe :: GlobalRdrElt -> Maybe FieldGlobalRdrElt
fieldGRE_maybe gre = do
  guard (isRecFldGRE gre)
  return gre

fieldGRELabel :: HasDebugCallStack => FieldGlobalRdrElt -> FieldLabel
fieldGRELabel = recFieldLabel . fieldGREInfo

fieldGREInfo :: HasDebugCallStack => FieldGlobalRdrElt -> RecFieldInfo
fieldGREInfo gre
  = assertPpr (isRecFldGRE gre) (ppr gre) $
    case greInfo gre of
      IAmRecField info -> info
      info -> pprPanic "fieldGREInfo" $
        vcat [ text "gre_name:" <+> ppr (greName gre)
             , text "info:" <+> ppr info ]

recFieldConLike_maybe :: HasDebugCallStack => GlobalRdrElt -> Maybe ConInfo
recFieldConLike_maybe gre =
  case greInfo gre of
    IAmConLike info -> Just info
    _               -> Nothing

recFieldInfo_maybe :: HasDebugCallStack => GlobalRdrElt -> Maybe RecFieldInfo
recFieldInfo_maybe gre =
  case greInfo gre of
    IAmRecField info -> assertPpr (isRecFldGRE gre) (ppr gre) $ Just info
    _                -> Nothing

-- | When looking up GREs, we may or may not want to include fields that were
-- defined in modules with @NoFieldSelectors@ enabled.  See Note
-- [NoFieldSelectors].
data FieldsOrSelectors
    = WantNormal -- ^ Include normal names, and fields with selectors, but
                 -- ignore fields without selectors.
    | WantBoth   -- ^ Include normal names and all fields (regardless of whether
                 -- they have selectors).
    | WantField  -- ^ Include only fields, with or without selectors, ignoring
                 -- any non-fields in scope.
  deriving (Eq, Show)

filterFieldGREs :: FieldsOrSelectors -> [GlobalRdrElt] -> [GlobalRdrElt]
filterFieldGREs WantBoth = id
filterFieldGREs fos = filter (allowGRE fos)

allowGRE :: FieldsOrSelectors -> GlobalRdrElt -> Bool
allowGRE WantBoth   _
  = True
allowGRE WantNormal gre
  -- NB: we only need to consult the GREInfo for record field GREs,
  -- to check whether they define field selectors.
  -- By checking 'isRecFldGRE' first, which only consults the NameSpace,
  -- we avoid forcing the GREInfo for things that aren't record fields.
  | isRecFldGRE gre
  = flHasFieldSelector (fieldGRELabel gre) == FieldSelectors
  | otherwise
  = True
allowGRE WantField gre
  = isRecFldGRE gre

-- | What should we look up in a 'GlobalRdrEnv'? Should we only look up
-- names with the exact same 'OccName', or do we allow different 'NameSpace's?
--
-- Depending on the answer, we might need more or less information from the
-- 'GlobalRdrEnv', e.g. if we want to include matching record fields we need
-- to know if the corresponding record fields define field selectors, for which
-- we need to consult the 'GREInfo'. This is why this datatype is a GADT.
--
-- See Note [IfGlobalRdrEnv].
data LookupGRE info where
  -- | Look for this specific 'OccName', with the exact same 'NameSpace',
  -- in the 'GlobalRdrEnv'.
  LookupOccName :: OccName -- ^ the 'OccName' to look up
                -> WhichGREs info
                    -- ^ information about other relevant 'NameSpace's
                -> LookupGRE info

  -- | Look up the 'OccName' of this 'RdrName' in the 'GlobalRdrEnv',
  -- filtering out those whose qualification matches that of the 'RdrName'.
  --
  -- Lookup returns an empty result for 'Exact' or 'Orig' 'RdrName's.
  LookupRdrName :: RdrName -- ^ the 'RdrName' to look up
                -> WhichGREs info
                    -- ^ information about other relevant 'NameSpace's
                -> LookupGRE info

  -- | Look for 'GRE's with the same unique as the given 'Name'
  -- in the 'GlobalRdrEnv'.
  LookupExactName
    :: { lookupExactName :: Name
          -- ^ the 'Name' to look up
       , lookInAllNameSpaces :: Bool
          -- ^ whether to look in *all* 'NameSpace's, or just
          -- in the 'NameSpace' of the 'Name'
          -- See Note [Template Haskell ambiguity]
       }
    -> LookupGRE info

  -- | Look up children 'GlobalRdrElt's with a given 'Parent'.
  LookupChildren
    :: OccName  -- ^ the 'OccName' to look up
    -> LookupChild
         -- ^ information to decide which 'GlobalRdrElt's
         -- are valid children after looking up
    -> LookupGRE info

-- | How should we look up in a 'GlobalRdrEnv'?
-- Which 'NameSpace's are considered relevant for a given lookup?
data WhichGREs info where
  -- | Only consider 'GlobalRdrElt's with the exact 'NameSpace' we look up.
  SameNameSpace :: WhichGREs info
  -- | Allow 'GlobalRdrElt's with different 'NameSpace's, e.g. allow looking up
  -- record fields from the variable 'NameSpace', or looking up a 'TyCon' from
  -- the data constructor 'NameSpace'.
  RelevantGREs
    :: { includeFieldSelectors :: !FieldsOrSelectors
        -- ^ how should we handle looking up variables?
        --
        --   - should we include record fields defined with @-XNoFieldSelectors@?
        --   - should we include non-fields?
        --
        -- See Note [NoFieldSelectors].
       , lookupVariablesForFields :: !Bool
          -- ^ when looking up a record field, should we also look up plain variables?
       , lookupTyConsAsWell :: !Bool
          -- ^ when looking up a variable, field or data constructor, should we
          -- also try the type constructor 'NameSpace'?
       }
    -> WhichGREs GREInfo

instance Outputable (WhichGREs info) where
  ppr SameNameSpace = text "SameNameSpace"
  ppr (RelevantGREs { includeFieldSelectors = sel
                    , lookupVariablesForFields = vars
                    , lookupTyConsAsWell = tcs_too })
    = braces $ hsep
       [ text "RelevantGREs"
       , text (show sel)
       , if vars then text "[vars]" else empty
       , if tcs_too then text "[tcs]" else empty ]

-- | Look up as many possibly relevant 'GlobalRdrElt's as possible.
pattern AllRelevantGREs :: WhichGREs GREInfo
pattern AllRelevantGREs =
  RelevantGREs { includeFieldSelectors = WantBoth
               , lookupVariablesForFields = True
               , lookupTyConsAsWell = True }

-- | Look up relevant GREs, taking into account the interaction between the
-- variable and field 'NameSpace's as determined by the 'FieldsOrSelector'
-- argument.
pattern RelevantGREsFOS :: FieldsOrSelectors -> WhichGREs GREInfo
pattern RelevantGREsFOS fos <- RelevantGREs { includeFieldSelectors = fos }
  where
    RelevantGREsFOS fos =
      RelevantGREs { includeFieldSelectors = fos
                   , lookupVariablesForFields = fos == WantBoth
                   , lookupTyConsAsWell = False }

data LookupChild
  = LookupChild
  { wantedParent :: Name
     -- ^ the parent we are looking up children of
  , lookupDataConFirst :: Bool
     -- ^ for type constructors, should we look in the data constructor
     -- namespace first?
  , prioritiseParent :: Bool
    -- ^ should we prioritise getting the right 'Parent'?
    --
    --  - @True@: prioritise getting the right 'Parent'
    --  - @False@: prioritise getting the right 'NameSpace'
    --
    -- See Note [childGREPriority].
  }

instance Outputable LookupChild where
  ppr (LookupChild { wantedParent = par
                   , lookupDataConFirst = dc
                   , prioritiseParent = prio_parent })
    = braces $ hsep
        [ text "LookupChild"
        , braces (text "parent:" <+> ppr par)
        , if dc then text "[dc_first]" else empty
        , if prio_parent then text "[prio_parent]" else empty
        ]

-- | After looking up something with the given 'NameSpace', is the resulting
-- 'GlobalRdrElt' we have obtained relevant, according to the 'RelevantGREs'
-- specification of which 'NameSpace's are relevant?
greIsRelevant :: WhichGREs GREInfo -- ^ specification of which 'GlobalRdrElt's to consider relevant
              -> NameSpace    -- ^ the 'NameSpace' of the thing we are looking up
              -> GlobalRdrElt -- ^ the 'GlobalRdrElt' we have looked up, in a
                              -- potentially different 'NameSpace' than we wanted
              -> Bool
greIsRelevant which_gres ns gre
  | ns == other_ns
  = True
  | otherwise
  = case which_gres of
      SameNameSpace -> False
      RelevantGREs { includeFieldSelectors = fos
                   , lookupVariablesForFields = vars_for_flds
                   , lookupTyConsAsWell = tycons_too }
        | ns == varName
        -> (isFieldNameSpace other_ns && allowGRE fos gre) || tc_too
        | isFieldNameSpace ns
        -> vars_for_flds &&
          (  other_ns == varName
          || (isFieldNameSpace other_ns && allowGRE fos gre)
          || tc_too )
        | isDataConNameSpace ns
        -> tc_too
        | otherwise
        -> False
        where
          tc_too = tycons_too && isTcClsNameSpace other_ns
  where
    other_ns = greNameSpace gre

{- Note [childGREPriority]
~~~~~~~~~~~~~~~~~~~~~~~~~~
There are currently two places in the compiler where we look up GlobalRdrElts
which have a given Parent. These are the two calls to lookupSubBndrOcc_helper:

  A. Looking up children in an export item, e.g.

       module M ( T(MkT, D) ) where { data T = MkT; data D = D }

  B. Looking up binders in a class or instance declaration, e.g.
     the operator +++ in the fixity declaration:

       class C a where { type (+++) :: a -> a ->; infixl 6 +++ }
       (+++) :: Int -> Int -> Int; (+++) = (+)

In these two situations, there are two competing metrics for finding the "best"
'GlobalRdrElt' that a particular 'OccName' resolves to:

  - does the resolved 'GlobalRdrElt' have the correct parent?
  - does the resolved 'GlobalRdrElt' have the same 'NameSpace' as the 'OccName'?

(A) and (B) have competing requirements.

For the example of (A) above, we know that the child 'D' of 'T' must live
in the data namespace, so we look up the OccName 'OccName DataName "D"' and
prioritise the lookup results based on the 'NameSpace'.
This means we get an error message of the form:

  The type constructor 'T' is not the parent of the data constructor 'D'.

as opposed to the rather unhelpful and confusing:

  The type constructor 'T' is not the parent of the type constructor 'D'.

See test case T11970.

For the example of (B) above, the fixity declaration for +++ lies inside the
class, so we should prioritise looking up 'GlobalRdrElt's whose parent is 'C'.
Not doing so led to #23664.
-}

-- | Scoring priority function for looking up children 'GlobalRdrElt'.
--
-- We score by 'Parent' and 'NameSpace', with higher priorities having lower
-- numbers. Which lexicographic order we use ('Parent' or 'NameSpace' first)
-- is determined by the first argument; see Note [childGREPriority].
childGREPriority :: LookupChild -- ^ what kind of child do we want,
                                -- e.g. what should its parent be?
                 -> NameSpace   -- ^ what 'NameSpace' are we originally looking in?
                 -> GlobalRdrEltX info
                                -- ^ the result of looking up; it might be in a different
                                -- 'NameSpace', which is used to determine the score
                                -- (in the first component)
                 -> Maybe (Int, Int)
childGREPriority (LookupChild { wantedParent = wanted_parent
                              , lookupDataConFirst = try_dc_first
                              , prioritiseParent = par_first })
  ns gre =
    case child_ns_prio $ greNameSpace gre of
      Nothing -> Nothing
      Just ns_prio ->
        let par_prio = parent_prio $ greParent gre
        in Just $ if par_first
                  then (par_prio, ns_prio)
                  else (ns_prio, par_prio)
          -- See Note [childGREPriority].

  where
      -- Pick out the possible 'NameSpace's in order of priority.
      child_ns_prio :: (NameSpace -> Maybe Int)
      child_ns_prio other_ns
        | other_ns == ns
        = Just 0
        | isTermVarOrFieldNameSpace ns
        , isTermVarOrFieldNameSpace other_ns
        = Just 0
        | isValNameSpace varName
        , other_ns == tcName
        -- When looking up children, we sometimes want a value name
        -- to resolve to a type constructor.
        -- For example, for an infix declaration "infixr 3 +!" or "infix 2 `Fun`"
        -- inside a class declaration, we want to account for the possibility
        -- that the identifier refers to an associated type (type constructor
        -- NameSpace), when otherwise "+!" would be in the term-level variable
        -- NameSpace, and "Fun" would be in the term-level data constructor
        -- NameSpace.  See tests T10816, T23664, T24037.
        = Just 1
        | ns == tcName
        , other_ns == dataName
        , try_dc_first -- try data namespace before type/class namespace?
        = Just (-1)
        | otherwise
        = Nothing

      parent_prio :: Parent -> Int
      parent_prio (ParentIs other_parent)
        | other_parent == wanted_parent = 0
        | otherwise                     = 1
      parent_prio NoParent              = 0

-- | Look something up in the Global Reader Environment.
--
-- The 'LookupGRE' argument specifies what to look up, and in particular
-- whether there should there be any lee-way if the 'NameSpace's don't
-- exactly match.
lookupGRE :: GlobalRdrEnvX info -> LookupGRE info -> [GlobalRdrEltX info]
lookupGRE env = \case
  LookupOccName occ which_gres ->
    case which_gres of
      SameNameSpace ->
        concat $ lookupOccEnv env occ
      rel@(RelevantGREs{}) ->
        filter (greIsRelevant rel (occNameSpace occ)) $
          concat $ lookupOccEnv_AllNameSpaces env occ
  LookupRdrName rdr rel ->
    pickGREs rdr $ lookupGRE env (LookupOccName (rdrNameOcc rdr) rel)
  LookupExactName { lookupExactName = nm
                  , lookInAllNameSpaces = all_ns } ->
      [ gre | gre <- lkup, greName gre == nm ]
    where
      occ = nameOccName nm
      lkup | all_ns    = concat $ lookupOccEnv_AllNameSpaces env occ
           | otherwise = fromMaybe [] $ lookupOccEnv env occ
  LookupChildren occ which_child ->
    let ns = occNameSpace occ
        all_gres = concat $ lookupOccEnv_AllNameSpaces env occ
    in highestPriorityGREs (childGREPriority which_child ns) all_gres

-- | Collect the 'GlobalRdrElt's with the highest priority according
-- to the given function (lower value <=> higher priority).
--
-- This allows us to first look in e.g. the data 'NameSpace', and then fall back
-- to the type/class 'NameSpace'.
highestPriorityGREs :: forall gre prio
                    .  Ord prio
                    => (gre -> Maybe prio)
                      -- ^ priority function
                      -- lower value <=> higher priority
                    -> [gre] -> [gre]
highestPriorityGREs priority gres =
  take_highest_prio $ NE.group $ sort
    [ S.Arg prio gre
    | gre <- gres
    , prio <- maybeToList $ priority gre ]
  where
    take_highest_prio :: [NE.NonEmpty (S.Arg prio gre)] -> [gre]
    take_highest_prio [] = []
    take_highest_prio (fs:_) = map (\ (S.Arg _ gre) -> gre) $ NE.toList fs
{-# INLINEABLE highestPriorityGREs #-}

-- | Look for precisely this 'Name' in the environment,
-- in the __same 'NameSpace'__ as the 'Name'.
--
-- This tests whether it is in scope, ignoring anything
-- else that might be in scope which doesn't have the same 'Unique'.
lookupGRE_Name :: Outputable info => GlobalRdrEnvX info -> Name -> Maybe (GlobalRdrEltX info)
lookupGRE_Name env name =
  case lookupGRE env (LookupExactName { lookupExactName = name
                                      , lookInAllNameSpaces = False }) of
      []    -> Nothing
      [gre] -> Just gre
      gres  -> pprPanic "lookupGRE_Name"
                        (ppr name $$ ppr (nameOccName name) $$ ppr gres)
               -- See INVARIANT 1 on GlobalRdrEnv

-- | Look for a particular record field selector in the environment.
lookupGRE_FieldLabel :: GlobalRdrEnv -> FieldLabel -> Maybe FieldGlobalRdrElt
lookupGRE_FieldLabel env fl =
  case lookupGRE_Name env (flSelector fl) of
    Nothing -> Nothing
    Just gre ->
      assertPpr (isRecFldGRE gre)
        (vcat [ text "lookupGre_FieldLabel:" <+> ppr fl ]) $
        Just gre

getGRE_NameQualifier_maybes :: GlobalRdrEnv -> Name -> [Maybe [ModuleName]]
-- Returns all the qualifiers by which 'x' is in scope
-- Nothing means "the unqualified version is in scope"
-- [] means the thing is not in scope at all
getGRE_NameQualifier_maybes env name
  = case lookupGRE_Name env name of
      Just gre -> [qualifier_maybe gre]
      Nothing  -> []
  where
    qualifier_maybe (GRE { gre_lcl = lcl, gre_imp = iss })
      | lcl       = Nothing
      | otherwise = Just $ map (is_as . is_decl) (bagToList iss)

-- | Is this 'GlobalRdrElt' defined locally?
isLocalGRE :: GlobalRdrEltX info -> Bool
isLocalGRE (GRE { gre_lcl = lcl }) = lcl

-- | Is this 'GlobalRdrElt' imported?
--
-- Not just the negation of 'isLocalGRE', because it might be an Exact or
-- Orig name reference. See Note [GlobalRdrElt provenance].
isImportedGRE :: GlobalRdrEltX info -> Bool
isImportedGRE (GRE { gre_imp = imps }) = not $ isEmptyBag imps

-- | Is this a record field GRE?
--
-- Important: does /not/ consult the 'GreInfo' field.
isRecFldGRE :: GlobalRdrEltX info -> Bool
isRecFldGRE (GRE { gre_name = nm }) = isFieldName nm

isDuplicateRecFldGRE :: GlobalRdrElt -> Bool
-- ^ Is this a record field defined with DuplicateRecordFields?
isDuplicateRecFldGRE =
    maybe False ((DuplicateRecordFields ==) . flHasDuplicateRecordFields) . greFieldLabel_maybe

isNoFieldSelectorGRE :: GlobalRdrElt -> Bool
-- ^ Is this a record field defined with NoFieldSelectors?
-- (See Note [NoFieldSelectors] in GHC.Rename.Env)
isNoFieldSelectorGRE =
    maybe False ((NoFieldSelectors ==) . flHasFieldSelector) . greFieldLabel_maybe

isFieldSelectorGRE :: GlobalRdrElt -> Bool
-- ^ Is this a record field defined with FieldSelectors?
-- (See Note [NoFieldSelectors] in GHC.Rename.Env)
isFieldSelectorGRE =
    maybe False ((FieldSelectors ==) . flHasFieldSelector) . greFieldLabel_maybe

greFieldLabel_maybe :: GlobalRdrElt -> Maybe FieldLabel
-- ^ Returns the field label of this GRE, if it has one
greFieldLabel_maybe = fmap fieldGRELabel . fieldGRE_maybe

unQualOK :: GlobalRdrEltX info -> Bool
-- ^ Test if an unqualified version of this thing would be in scope
unQualOK (GRE {gre_lcl = lcl, gre_imp = iss })
  | lcl = True
  | otherwise = any unQualSpecOK iss

{- Note [GRE filtering]
~~~~~~~~~~~~~~~~~~~~~~~
(pickGREs rdr gres) takes a list of GREs which have the same OccName
as 'rdr', say "x".  It does two things:

(a) filters the GREs to a subset that are in scope
    * Qualified,   as 'M.x'  if want_qual    is Qual M _
    * Unqualified, as 'x'    if want_unqual  is Unqual _

(b) for that subset, filter the provenance field (gre_lcl and gre_imp)
    to ones that brought it into scope qualified or unqualified resp.

Example:
      module A ( f ) where
      import qualified Foo( f )
      import Baz( f )
      f = undefined

Let's suppose that Foo.f and Baz.f are the same entity really, but the local
'f' is different, so there will be two GREs matching "f":
   gre1:  gre_lcl = True,  gre_imp = []
   gre2:  gre_lcl = False, gre_imp = [ imported from Foo, imported from Bar ]

The use of "f" in the export list is ambiguous because it's in scope
from the local def and the import Baz(f); but *not* the import qualified Foo.
pickGREs returns two GRE
   gre1:   gre_lcl = True,  gre_imp = []
   gre2:   gre_lcl = False, gre_imp = [ imported from Bar ]

Now the "ambiguous occurrence" message can correctly report how the
ambiguity arises.
-}

pickGREs :: RdrName -> [GlobalRdrEltX info] -> [GlobalRdrEltX info]
-- ^ Takes a list of GREs which have the right OccName 'x'
-- Pick those GREs that are in scope
--    * Qualified,   as 'M.x'  if want_qual    is Qual M _
--    * Unqualified, as 'x'    if want_unqual  is Unqual _
--
-- Return each such GRE, with its ImportSpecs filtered, to reflect
-- how it is in scope qualified or unqualified respectively.
-- See Note [GRE filtering]
pickGREs (Unqual {})  gres = mapMaybe pickUnqualGRE     gres
pickGREs (Qual mod _) gres = mapMaybe (pickQualGRE mod) gres
pickGREs _            _    = []  -- I don't think this actually happens

pickUnqualGRE :: GlobalRdrEltX info -> Maybe (GlobalRdrEltX info)
pickUnqualGRE gre@(GRE { gre_lcl = lcl, gre_imp = iss })
  | not lcl, null iss' = Nothing
  | otherwise          = Just (gre { gre_imp = iss' })
  where
    iss' = filterBag unQualSpecOK iss

pickQualGRE :: ModuleName -> GlobalRdrEltX info -> Maybe (GlobalRdrEltX info)
pickQualGRE mod gre@(GRE { gre_lcl = lcl, gre_imp = iss })
  | not lcl', null iss' = Nothing
  | otherwise           = Just (gre { gre_lcl = lcl', gre_imp = iss' })
  where
    iss' = filterBag (qualSpecOK mod) iss
    lcl' = lcl && name_is_from mod

    name_is_from :: ModuleName -> Bool
    name_is_from mod = case greDefinitionModule gre of
                         Just n_mod -> moduleName n_mod == mod
                         Nothing    -> False

pickGREsModExp :: ModuleName -> [GlobalRdrEltX info] -> [(GlobalRdrEltX info,GlobalRdrEltX info)]
-- ^ Pick GREs that are in scope *both* qualified *and* unqualified
-- Return each GRE that is, as a pair
--    (qual_gre, unqual_gre)
-- These two GREs are the original GRE with imports filtered to express how
-- it is in scope qualified an unqualified respectively
--
-- Used only for the 'module M' item in export list;
--   see 'GHC.Tc.Gen.Export.exports_from_avail'
pickGREsModExp mod gres = mapMaybe (pickBothGRE mod) gres

-- | isBuiltInSyntax filter out names for built-in syntax They
-- just clutter up the environment (esp tuples), and the
-- parser will generate Exact RdrNames for them, so the
-- cluttered envt is no use.  Really, it's only useful for
-- GHC.Base and GHC.Tuple.
pickBothGRE :: ModuleName -> GlobalRdrEltX info -> Maybe (GlobalRdrEltX info, GlobalRdrEltX info)
pickBothGRE mod gre
  | isBuiltInSyntax (greName gre)
  = Nothing
  | Just gre1 <- pickQualGRE mod gre
  , Just gre2 <- pickUnqualGRE   gre
  = Just (gre1, gre2)
  | otherwise
  = Nothing

-- Building GlobalRdrEnvs

plusGlobalRdrEnv :: GlobalRdrEnv -> GlobalRdrEnv -> GlobalRdrEnv
plusGlobalRdrEnv env1 env2 = plusOccEnv_C (foldr insertGRE) env1 env2

mkGlobalRdrEnv :: [GlobalRdrElt] -> GlobalRdrEnv
mkGlobalRdrEnv gres
  = foldr add emptyGlobalRdrEnv gres
  where
    add gre env = extendOccEnv_Acc insertGRE Utils.singleton env
                                   (greOccName gre)
                                   gre

insertGRE :: GlobalRdrElt -> [GlobalRdrElt] -> [GlobalRdrElt]
insertGRE new_g [] = [new_g]
insertGRE new_g (old_g : old_gs)
        | greName new_g == greName old_g
        = new_g `plusGRE` old_g : old_gs
        | otherwise
        = old_g : insertGRE new_g old_gs

plusGRE :: GlobalRdrElt -> GlobalRdrElt -> GlobalRdrElt
-- Used when the gre_name fields match
plusGRE g1 g2
  = GRE { gre_name = gre_name g1
        , gre_lcl  = gre_lcl g1 || gre_lcl g2
        , gre_imp  = gre_imp g1 `unionBags` gre_imp g2
        , gre_par  = gre_par g1 `plusParent` gre_par g2
        , gre_info = gre_info g1 `plusGREInfo` gre_info g2 }

transformGREs :: (GlobalRdrElt -> GlobalRdrElt)
              -> [OccName]
              -> GlobalRdrEnv -> GlobalRdrEnv
-- ^ Apply a transformation function to the GREs for these OccNames
transformGREs trans_gre occs rdr_env
  = foldr trans rdr_env occs
  where
    trans occ env
      = case lookupOccEnv env occ of
           Just gres -> extendOccEnv env occ (map trans_gre gres)
           Nothing   -> env

extendGlobalRdrEnv :: GlobalRdrEnv -> GlobalRdrElt -> GlobalRdrEnv
extendGlobalRdrEnv env gre
  = extendOccEnv_Acc insertGRE Utils.singleton env
                     (greOccName gre) gre

{- Note [GlobalRdrEnv shadowing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before adding new names to the GlobalRdrEnv we nuke some existing entries;
this is "shadowing".  The actual work is done by GHC.Types.Name.Reader.shadowNames.
Suppose

   env' = shadowNames env { f } `extendGlobalRdrEnv` { M.f }

Then:
   * Looking up (Unqual f) in env' should succeed, returning M.f,
     even if env contains existing unqualified bindings for f.
     They are shadowed

   * Looking up (Qual M.f) in env' should succeed, returning M.f

   * Looking up (Qual X.f) in env', where X /= M, should be the same as
     looking up (Qual X.f) in env.

     That is, shadowNames does /not/ delete earlier qualified bindings

There are two reasons for shadowing:

* The GHCi REPL

  - Ids bought into scope on the command line (eg let x = True) have
    External Names, like Ghci4.x.  We want a new binding for 'x' (say)
    to override the existing binding for 'x'.  Example:

           ghci> :load M    -- Brings `x` and `M.x` into scope
           ghci> x
           ghci> "Hello"
           ghci> M.x
           ghci> "hello"
           ghci> let x = True  -- Shadows `x`
           ghci> x             -- The locally bound `x`
                               -- NOT an ambiguous reference
           ghci> True
           ghci> M.x           -- M.x is still in scope!
           ghci> "Hello"

    So when we add `x = True` we must not delete the `M.x` from the
    `GlobalRdrEnv`; rather we just want to make it "qualified only";
    hence the `set_qual` in `shadowNames`.  See also Note
    [Interactively-bound Ids in GHCi] in GHC.Runtime.Context

  - Data types also have External Names, like Ghci4.T; but we still want
    'T' to mean the newly-declared 'T', not an old one.

* Nested Template Haskell declaration brackets
  See Note [Top-level Names in Template Haskell decl quotes] in GHC.Rename.Names

  Consider a TH decl quote:
      module M where
        f x = h [d| f = ...f...M.f... |]
  We must shadow the outer unqualified binding of 'f', else we'll get
  a complaint when extending the GlobalRdrEnv, saying that there are
  two bindings for 'f'.  There are several tricky points:

    - This shadowing applies even if the binding for 'f' is in a
      where-clause, and hence is in the *local* RdrEnv not the *global*
      RdrEnv.  This is done in lcl_env_TH in extendGlobalRdrEnvRn.

    - The External Name M.f from the enclosing module must certainly
      still be available.  So we don't nuke it entirely; we just make
      it seem like qualified import.

    - We only shadow *External* names (which come from the main module),
      or from earlier GHCi commands. Do not shadow *Internal* names
      because in the bracket
          [d| class C a where f :: a
              f = 4 |]
      rnSrcDecls will first call extendGlobalRdrEnvRn with C[f] from the
      class decl, and *separately* extend the envt with the value binding.
      At that stage, the class op 'f' will have an Internal name.

Wrinkle [Shadowing namespaces]

  In the following GHCi session:

    > data A = MkA { foo :: Int }
    > foo = False
    > bar = foo

  We expect the variable 'foo' to shadow the record field 'foo', even though
  they are in separate namespaces, so that the occurrence of 'foo' in the body
  of 'bar' is not ambiguous.

-}

shadowNames :: Bool -- ^ discard names that are only available qualified?
            -> GlobalRdrEnv -> GlobalRdrEnv -> GlobalRdrEnv
-- Remove certain old GREs that share the same OccName as this new Name.
-- See Note [GlobalRdrEnv shadowing] for details
shadowNames drop_only_qualified env new_gres = minusOccEnv_C_Ns do_shadowing env new_gres
  where

    do_shadowing :: UniqFM NameSpace [GlobalRdrElt]
                 -> UniqFM NameSpace [GlobalRdrElt]
                 -> UniqFM NameSpace [GlobalRdrElt]
    do_shadowing olds news =
      -- Start off by accumulating all 'NameSpace's shadowed
      -- by the entire collection of new GREs.
      let shadowed_gres :: ShadowedGREs
          shadowed_gres =
            nonDetFoldUFM (\ gres shads -> foldMap greShadowedNameSpaces gres S.<> shads)
              mempty news

      -- Then shadow the old 'GlobalRdrElt's, now that we know which 'NameSpace's
      -- should be shadowed.
          shadow_list :: Unique -> [GlobalRdrElt] -> Maybe [GlobalRdrElt]
          shadow_list old_ns old_gres =
            case namespace_is_shadowed old_ns shadowed_gres of
              IsNotShadowed -> Just old_gres
              IsShadowed    -> guard_nonEmpty $ mapMaybe shadow old_gres
              IsShadowedIfFieldSelector ->
                guard_nonEmpty $
                mapMaybe (\ old_gre -> if isFieldSelectorGRE old_gre then shadow old_gre else Just old_gre)
                  old_gres

      -- Now do all of the shadowing in a single go. This avoids traversing
      -- the old GlobalRdrEnv multiple times over.
      in mapMaybeWithKeyUFM shadow_list olds

    guard_nonEmpty :: [a] -> Maybe [a]
    guard_nonEmpty xs | null xs   = Nothing
                      | otherwise = Just xs

    -- Shadow a single GRE, by either qualifying it or removing it entirely.
    shadow :: GlobalRdrElt-> Maybe GlobalRdrElt
    shadow old_gre@(GRE { gre_lcl = lcl, gre_imp = iss }) =
      case greDefinitionModule old_gre of
        Nothing -> Just old_gre   -- Old name is Internal; do not shadow
        Just old_mod
           |  null iss'            -- Nothing remains
           || drop_only_qualified
           -> Nothing

           | otherwise
           -> Just (old_gre { gre_lcl = False, gre_imp = iss' })

           where
             iss' = lcl_imp `unionBags` mapBag set_qual iss
             lcl_imp | lcl       = unitBag $ mk_fake_imp_spec old_gre old_mod
                     | otherwise = emptyBag

    mk_fake_imp_spec old_gre old_mod    -- Urgh!
      = ImpSpec id_spec ImpAll
      where
        old_mod_name = moduleName old_mod
        id_spec      = ImpDeclSpec { is_mod = old_mod
                                   , is_as = old_mod_name
                                   , is_pkg_qual = NoPkgQual
                                   , is_qual = True
                                   , is_isboot = NotBoot
                                   , is_dloc = greDefinitionSrcSpan old_gre }

    set_qual :: ImportSpec -> ImportSpec
    set_qual is = is { is_decl = (is_decl is) { is_qual = True } }

-- | @greClashesWith new_gre old_gre@ computes whether @new_gre@ clashes
-- with @old_gre@ (assuming they both have the same underlying 'occNameFS').
greClashesWith :: GlobalRdrElt -> (GlobalRdrElt -> Bool)
greClashesWith new_gre old_gre =
  old_gre `greIsShadowed` greShadowedNameSpaces new_gre

-- | Is the given 'GlobalRdrElt' shadowed, as specified by the 'ShadowedNameSpace's?
greIsShadowed :: GlobalRdrElt -> ShadowedGREs -> Bool
greIsShadowed old_gre shadowed =
  case getUnique old_ns `namespace_is_shadowed` shadowed of
    IsShadowed                -> True
    IsNotShadowed             -> False
    IsShadowedIfFieldSelector -> isFieldSelectorGRE old_gre
  where
    old_ns = occNameSpace $ greOccName old_gre


-- | Whether a 'GlobalRdrElt' is definitely shadowed, definitely not shadowed,
-- or conditionally shadowed based on more information beyond the 'NameSpace'.
data IsShadowed
  -- | The GRE is not shadowed.
  = IsNotShadowed
  -- | The GRE is shadowed.
  | IsShadowed
  -- | The GRE is shadowed iff it is a record field GRE
  -- which defines a field selector (i.e. FieldSelectors is enabled in its
  -- defining module).
  | IsShadowedIfFieldSelector

-- | Internal function: is a 'GlobalRdrElt' with the 'NameSpace' with given
-- 'Unique' shadowed by the specified 'ShadowedGREs'?
namespace_is_shadowed :: Unique -> ShadowedGREs -> IsShadowed
namespace_is_shadowed old_ns (ShadowedGREs shadowed_nonflds shadowed_flds)
  | isFldNSUnique old_ns
  = case shadowed_flds of
      ShadowAllFieldGREs -> IsShadowed
      ShadowFieldSelectorsAnd shadowed
        | old_ns `elemUniqSet_Directly` shadowed
        -> IsShadowed
        | otherwise
        -> IsShadowedIfFieldSelector
      ShadowFieldNameSpaces shadowed
        | old_ns `elemUniqSet_Directly` shadowed
        -> IsShadowed
        | otherwise
        -> IsNotShadowed
  | old_ns `elemUniqSet_Directly` shadowed_nonflds
  = IsShadowed
  | otherwise
  = IsNotShadowed

-- | What are all the 'GlobalRdrElt's that are shadowed by this new 'GlobalRdrElt'?
greShadowedNameSpaces :: GlobalRdrElt -> ShadowedGREs
greShadowedNameSpaces gre = ShadowedGREs shadowed_nonflds shadowed_flds
  where
    ns = occNameSpace $ greOccName gre
    !shadowed_nonflds
      | isFieldNameSpace ns
      -- A new record field shadows variables if it defines a field selector.
      = if isFieldSelectorGRE gre
        then unitUniqSet varName
        else emptyUniqSet
      | otherwise
      = unitUniqSet ns
    !shadowed_flds
      | ns == varName
      -- A new variable shadows record fields with field selectors.
      = ShadowFieldSelectorsAnd emptyUniqSet
      | isFieldNameSpace ns
      -- A new record field shadows record fields unless it is a duplicate record field.
      = if isDuplicateRecFldGRE gre
        then ShadowFieldNameSpaces (unitUniqSet ns)
        -- NB: we must still shadow fields with the same constructor name.
        else ShadowAllFieldGREs
      | otherwise
      = ShadowFieldNameSpaces emptyUniqSet

-- | A description of which 'GlobalRdrElt's are shadowed.
data ShadowedGREs
  = ShadowedGREs
    { shadowedNonFieldNameSpaces :: !(UniqSet NameSpace)
      -- ^ These specific non-field 'NameSpace's are shadowed.
    , shadowedFieldGREs :: !ShadowedFieldGREs
      -- ^ These field 'GlobalRdrElt's are shadowed.
    }

-- | A description of which record field 'GlobalRdrElt's are shadowed.
data ShadowedFieldGREs
  -- | All field 'GlobalRdrElt's are shadowed.
  = ShadowAllFieldGREs
  -- | Record field GREs defining field selectors, as well as those
  -- with the explicitly specified field 'NameSpace's, are shadowed.
  | ShadowFieldSelectorsAnd { shadowedFieldNameSpaces :: !(UniqSet NameSpace) }
  -- | These specific field 'NameSpace's are shadowed.
  | ShadowFieldNameSpaces { shadowedFieldNameSpaces :: !(UniqSet NameSpace) }

instance Monoid ShadowedFieldGREs where
  mempty = ShadowFieldNameSpaces { shadowedFieldNameSpaces = emptyUniqSet }

instance Semigroup ShadowedFieldGREs where
  ShadowAllFieldGREs <> _ = ShadowAllFieldGREs
  _ <> ShadowAllFieldGREs = ShadowAllFieldGREs
  ShadowFieldSelectorsAnd ns1 <> ShadowFieldSelectorsAnd ns2 =
    ShadowFieldSelectorsAnd (ns1 S.<> ns2)
  ShadowFieldSelectorsAnd ns1 <> ShadowFieldNameSpaces ns2 =
    ShadowFieldSelectorsAnd (ns1 S.<> ns2)
  ShadowFieldNameSpaces ns1 <> ShadowFieldSelectorsAnd ns2 =
    ShadowFieldSelectorsAnd (ns1 S.<> ns2)
  ShadowFieldNameSpaces ns1 <> ShadowFieldNameSpaces ns2 =
    ShadowFieldNameSpaces (ns1 S.<> ns2)

instance Monoid ShadowedGREs where
  mempty =
    ShadowedGREs
      { shadowedNonFieldNameSpaces = emptyUniqSet
      , shadowedFieldGREs = mempty }

instance Semigroup ShadowedGREs where
  ShadowedGREs nonflds1 flds1 <> ShadowedGREs nonflds2 flds2 =
    ShadowedGREs (nonflds1 S.<> nonflds2) (flds1 S.<> flds2)

{-
************************************************************************
*                                                                      *
                        ImportSpec
*                                                                      *
************************************************************************
-}

-- | Import Specification
--
-- The 'ImportSpec' of something says how it came to be imported
-- It's quite elaborate so that we can give accurate unused-name warnings.
data ImportSpec = ImpSpec { is_decl :: !ImpDeclSpec,
                            is_item :: !ImpItemSpec }
                deriving( Eq, Data )

instance NFData ImportSpec where
  rnf = rwhnf -- All fields are strict, so we don't need to do anything

-- | Import Declaration Specification
--
-- Describes a particular import declaration and is
-- shared among all the 'Provenance's for that decl
data ImpDeclSpec
  = ImpDeclSpec {
        is_mod      :: !Module,     -- ^ Module imported, e.g. @import Muggle@
                                   -- Note the @Muggle@ may well not be
                                   -- the defining module for this thing!

                                   -- TODO: either should be Module, or there
                                   -- should be a Maybe UnitId here too.
        is_as       :: !ModuleName, -- ^ Import alias, e.g. from @as M@ (or @Muggle@ if there is no @as@ clause)
        is_pkg_qual :: !PkgQual,    -- ^ Was this a package import?
        is_qual     :: !Bool,       -- ^ Was this import qualified?
        is_dloc     :: !SrcSpan,    -- ^ The location of the entire import declaration
        is_isboot   :: !IsBootInterface -- ^ Was this a SOURCE import?
    } deriving (Eq, Data)

instance NFData ImpDeclSpec where
  rnf = rwhnf -- Already strict in all fields

-- | Import Item Specification
--
-- Describes import info a particular Name
data ImpItemSpec
  = ImpAll              -- ^ The import had no import list,
                        -- or had a hiding list

  | ImpSome {
        is_explicit :: !Bool,
        is_iloc     :: !SrcSpan  -- Location of the import item
    }   -- ^ The import had an import list.
        -- The 'is_explicit' field is @True@ iff the thing was named
        -- /explicitly/ in the import specs rather
        -- than being imported as part of a "..." group. Consider:
        --
        -- > import C( T(..) )
        --
        -- Here the constructors of @T@ are not named explicitly;
        -- only @T@ is named explicitly.
  deriving (Eq, Data)

bestImport :: NE.NonEmpty ImportSpec -> ImportSpec
-- See Note [Choosing the best import declaration]
bestImport iss = NE.head $ NE.sortBy best iss
  where
    best :: ImportSpec -> ImportSpec -> Ordering
    -- Less means better
    -- Unqualified always wins over qualified; then
    -- import-all wins over import-some; then
    -- earlier declaration wins over later
    best (ImpSpec { is_item = item1, is_decl = d1 })
         (ImpSpec { is_item = item2, is_decl = d2 })
      = (is_qual d1 `compare` is_qual d2) S.<> best_item item1 item2 S.<>
        SrcLoc.leftmost_smallest (is_dloc d1) (is_dloc d2)

    best_item :: ImpItemSpec -> ImpItemSpec -> Ordering
    best_item ImpAll ImpAll = EQ
    best_item ImpAll (ImpSome {}) = LT
    best_item (ImpSome {}) ImpAll = GT
    best_item (ImpSome { is_explicit = e1 })
              (ImpSome { is_explicit = e2 }) = e1 `compare` e2
     -- False < True, so if e1 is explicit and e2 is not, we get GT

{- Note [Choosing the best import declaration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When reporting unused import declarations we use the following rules.
   (see [wiki:commentary/compiler/unused-imports])

Say that an import-item is either
  * an entire import-all decl (eg import Foo), or
  * a particular item in an import list (eg import Foo( ..., x, ...)).
The general idea is that for each /occurrence/ of an imported name, we will
attribute that use to one import-item. Once we have processed all the
occurrences, any import items with no uses attributed to them are unused,
and are warned about. More precisely:

1. For every RdrName in the program text, find its GlobalRdrElt.

2. Then, from the [ImportSpec] (gre_imp) of that GRE, choose one
   the "chosen import-item", and mark it "used". This is done
   by 'bestImport'

3. After processing all the RdrNames, bleat about any
   import-items that are unused.
   This is done in GHC.Rename.Names.warnUnusedImportDecls.

The function 'bestImport' returns the dominant import among the
ImportSpecs it is given, implementing Step 2.  We say import-item A
dominates import-item B if we choose A over B. In general, we try to
choose the import that is most likely to render other imports
unnecessary.  Here is the dominance relationship we choose:

    a) import Foo dominates import qualified Foo.

    b) import Foo dominates import Foo(x).

    c) Otherwise choose the textually first one.

Rationale for (a).  Consider
   import qualified M  -- Import #1
   import M( x )       -- Import #2
   foo = M.x + x

The unqualified 'x' can only come from import #2.  The qualified 'M.x'
could come from either, but bestImport picks import #2, because it is
more likely to be useful in other imports, as indeed it is in this
case (see #5211 for a concrete example).

But the rules are not perfect; consider
   import qualified M  -- Import #1
   import M( x )       -- Import #2
   foo = M.x + M.y

The M.x will use import #2, but M.y can only use import #1.
-}


unQualSpecOK :: ImportSpec -> Bool
-- ^ Is in scope unqualified?
unQualSpecOK is = not (is_qual (is_decl is))

qualSpecOK :: ModuleName -> ImportSpec -> Bool
-- ^ Is in scope qualified with the given module?
qualSpecOK mod is = mod == is_as (is_decl is)

importSpecLoc :: ImportSpec -> SrcSpan
importSpecLoc (ImpSpec decl ImpAll) = is_dloc decl
importSpecLoc (ImpSpec _    item)   = is_iloc item

importSpecModule :: ImportSpec -> ModuleName
importSpecModule = moduleName . is_mod . is_decl

isExplicitItem :: ImpItemSpec -> Bool
isExplicitItem ImpAll                        = False
isExplicitItem (ImpSome {is_explicit = exp}) = exp

pprNameProvenance :: GlobalRdrEltX info -> SDoc
-- ^ Print out one place where the name was define/imported
-- (With -dppr-debug, print them all)
pprNameProvenance (GRE { gre_name = name, gre_lcl = lcl, gre_imp = iss })
  = ifPprDebug (vcat pp_provs)
               (head pp_provs)
  where
    pp_provs = pp_lcl ++ map pp_is (bagToList iss)
    pp_lcl = if lcl then [text "defined at" <+> ppr (nameSrcLoc name)]
                    else []
    pp_is is = sep [ppr is, ppr_defn_site is name]

-- If we know the exact definition point (which we may do with GHCi)
-- then show that too.  But not if it's just "imported from X".
ppr_defn_site :: ImportSpec -> Name -> SDoc
ppr_defn_site imp_spec name
  | same_module && not (isGoodSrcSpan loc)
  = empty              -- Nothing interesting to say
  | otherwise
  = parens $ hang (text "and originally defined" <+> pp_mod)
                2 (pprLoc loc)
  where
    loc = nameSrcSpan name
    defining_mod = assertPpr (isExternalName name) (ppr name) $ nameModule name
    same_module = importSpecModule imp_spec == moduleName defining_mod
    pp_mod | same_module = empty
           | otherwise   = text "in" <+> quotes (ppr defining_mod)


instance Outputable ImportSpec where
   ppr imp_spec
     = text "imported" <+> qual
        <+> text "from" <+> quotes (ppr (importSpecModule imp_spec))
        <+> pprLoc (importSpecLoc imp_spec)
     where
       qual | is_qual (is_decl imp_spec) = text "qualified"
            | otherwise                  = empty

pprLoc :: SrcSpan -> SDoc
pprLoc (RealSrcSpan s _)  = text "at" <+> ppr s
pprLoc (UnhelpfulSpan {}) = empty

-- | Indicate if the given name is the "@" operator
opIsAt :: RdrName -> Bool
opIsAt e = e == mkUnqual varName (fsLit "@")
