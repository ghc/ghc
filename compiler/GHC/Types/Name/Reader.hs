{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

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
        rdrNameOcc, rdrNameSpace, demoteRdrName, promoteRdrName,
        isRdrDataCon, isRdrTyVar, isRdrTc, isQual, isQual_maybe, isUnqual,
        isOrig, isOrig_maybe, isExact, isExact_maybe, isSrcRdrName,

        -- * Local mapping of 'RdrName' to 'Name.Name'
        LocalRdrEnv, emptyLocalRdrEnv, extendLocalRdrEnv, extendLocalRdrEnvList,
        lookupLocalRdrEnv, lookupLocalRdrOcc,
        elemLocalRdrEnv, inLocalRdrEnvScope,
        localRdrEnvElts, delLocalRdrEnvList,

        -- * Global mapping of 'RdrName' to 'GlobalRdrElt's
        GlobalRdrEnv, emptyGlobalRdrEnv, mkGlobalRdrEnv, plusGlobalRdrEnv,
        lookupGlobalRdrEnv, extendGlobalRdrEnv, greOccName, shadowNames,
        pprGlobalRdrEnv, globalRdrEnvElts,
        lookupGRE_RdrName, lookupGRE_Name,
        lookupGRE_GreName, lookupGRE_FieldLabel,
        lookupGRE_Name_OccName,
        getGRE_NameQualifier_maybes,
        transformGREs, pickGREs, pickGREsModExp,

        -- * GlobalRdrElts
        gresFromAvails, gresFromAvail, localGREsFromAvail, availFromGRE,
        greRdrNames, greSrcSpan, greQualModName,
        gresToAvailInfo,
        greDefinitionModule, greDefinitionSrcSpan,
        greMangledName, grePrintableName,

        -- ** Global 'RdrName' mapping elements: 'GlobalRdrElt', 'Provenance', 'ImportSpec'
        GlobalRdrElt(..), isLocalGRE, isRecFldGRE, isOverloadedRecFldGRE, greFieldLabel,
        unQualOK, qualSpecOK, unQualSpecOK,
        pprNameProvenance,
        GreName(..), greNameSrcSpan,
        Parent(..), greParent_maybe,
        ImportSpec(..), ImpDeclSpec(..), ImpItemSpec(..),
        importSpecLoc, importSpecModule, isExplicitItem, bestImport,

        -- * Utils for StarIsType
        starInfo,

        -- * Utils
        opIsAt,
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Unit.Module
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Types.Name.Set
import GHC.Data.Maybe
import GHC.Types.SrcLoc as SrcLoc
import GHC.Data.FastString
import GHC.Types.FieldLabel
import GHC.Utils.Outputable
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Utils.Misc as Utils
import GHC.Utils.Panic
import GHC.Types.Name.Env

import Data.Data
import Data.List( sortBy )

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

-- For details on above see note [Api annotations] in "GHC.Parser.Annotation"
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
    compare (Qual m1 o1) (Qual m2 o2) = (o1 `compare` o2) `thenCmp` (m1 `compare` m2)
    compare (Qual _ _)   (Orig _ _)   = LT

    compare (Orig m1 o1) (Orig m2 o2) = (o1 `compare` o2) `thenCmp` (m1 `compare` m2)
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
      ppr_elt name = parens (ppr (getUnique (nameOccName name))) <+> ppr name
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
localRdrEnvElts (LRE { lre_env = env }) = occEnvElts env

inLocalRdrEnvScope :: Name -> LocalRdrEnv -> Bool
-- This is the point of the NameSet
inLocalRdrEnvScope name (LRE { lre_in_scope = ns }) = name `elemNameSet` ns

delLocalRdrEnvList :: LocalRdrEnv -> [OccName] -> LocalRdrEnv
delLocalRdrEnvList lre@(LRE { lre_env = env }) occs
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
type GlobalRdrEnv = OccEnv [GlobalRdrElt]
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
--
--              NB: greOccName gre is usually the same as
--                  nameOccName (greMangledName gre), but not always in the
--                  case of record selectors; see Note [GreNames]

-- | Global Reader Element
--
-- An element of the 'GlobalRdrEnv'
data GlobalRdrElt
  = GRE { gre_name :: GreName      -- ^ See Note [GreNames]
        , gre_par  :: Parent       -- ^ See Note [Parents]
        , gre_lcl :: Bool          -- ^ True <=> the thing was defined locally
        , gre_imp :: [ImportSpec]  -- ^ In scope through these imports
    } deriving (Data, Eq)
         -- INVARIANT: either gre_lcl = True or gre_imp is non-empty
         -- See Note [GlobalRdrElt provenance]

-- | See Note [Parents]
data Parent = NoParent
            | ParentIs  { par_is :: Name }
            deriving (Eq, Data)

instance Outputable Parent where
   ppr NoParent        = empty
   ppr (ParentIs n)    = text "parent:" <> ppr n

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
i.e. how the Name came to be in scope.  It can be in scope two ways:
  - gre_lcl = True: it is bound in this module
  - gre_imp: a list of all the imports that brought it into scope

It's an INVARIANT that you have one or the other; that is, either
gre_lcl is True, or gre_imp is non-empty.

It is just possible to have *both* if there is a module loop: a Name
is defined locally in A, and also brought into scope by importing a
module that SOURCE-imported A.  Example (#7672):

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
introduces FieldGreNames using ParentIs, but a record pattern synonym can
introduce FieldGreNames that use NoParent. (In the past we represented fields
using an additional constructor of the Parent type, which could not adequately
represent this situation.) See also
Note [Representing pattern synonym fields in AvailInfo] in GHC.Types.Avail.


Note [GreNames]
~~~~~~~~~~~~~~~
A `GlobalRdrElt` has a field `gre_name :: GreName`, which uniquely
identifies what the `GlobalRdrElt` describes.  There are two sorts of
`GreName` (see the data type decl):

* NormalGreName Name: this is used for most entities; the Name
  uniquely identifies it. It is stored in the GlobalRdrEnv under
  the OccName of the Name.

* FieldGreName FieldLabel: is used only for field labels of a
  record. With -XDuplicateRecordFields there may be many field
  labels `x` in scope; e.g.
     data T1 = MkT1 { x :: Int }
     data T2 = MkT2 { x :: Bool }
  Each has a different GlobalRdrElt with a distinct GreName.
  The two fields are uniquely identified by their record selectors,
  which are stored in the FieldLabel, and have mangled names like
  `$sel:x:MkT1`.  See Note [FieldLabel] in GHC.Types.FieldLabel.

  These GREs are stored in the GlobalRdrEnv under the OccName of the
  field (i.e. "x" in both cases above), /not/ the OccName of the mangled
  record selector function.

A GreName, and hence a GRE, has both a "printable" and a "mangled" Name.  These
are identical for normal names, but for record fields compiled with
-XDuplicateRecordFields they will differ. So we have two pairs of functions:

 * greNameMangledName :: GreName -> Name
   greMangledName :: GlobalRdrElt -> Name
   The "mangled" Name is the actual Name of the selector function,
   e.g. $sel:x:MkT1.  This should not be displayed to the user, but is used to
   uniquely identify the field in the renamer, and later in the backend.

 * greNamePrintableName :: GreName -> Name
   grePrintableName :: GlobalRdrElt -> Name
   The "printable" Name is the "manged" Name with its OccName replaced with that
   of the field label.  This is how the field should be output to the user.

Since the right Name to use is context-dependent, we do not define a NamedThing
instance for GREName (or GlobalRdrElt), but instead make the choice explicit.


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

-- | make a 'GlobalRdrEnv' where all the elements point to the same
-- Provenance (useful for "hiding" imports, or imports with no details).
gresFromAvails :: Maybe ImportSpec -> [AvailInfo] -> [GlobalRdrElt]
-- prov = Nothing   => locally bound
--        Just spec => imported as described by spec
gresFromAvails prov avails
  = concatMap (gresFromAvail (const prov)) avails

localGREsFromAvail :: AvailInfo -> [GlobalRdrElt]
-- Turn an Avail into a list of LocalDef GlobalRdrElts
localGREsFromAvail = gresFromAvail (const Nothing)

gresFromAvail :: (Name -> Maybe ImportSpec) -> AvailInfo -> [GlobalRdrElt]
gresFromAvail prov_fn avail
  = map mk_gre (availNonFldNames avail) ++ map mk_fld_gre (availFlds avail)
  where
    mk_gre n
      = case prov_fn n of  -- Nothing => bound locally
                           -- Just is => imported from 'is'
          Nothing -> GRE { gre_name = NormalGreName n, gre_par = mkParent n avail
                         , gre_lcl = True, gre_imp = [] }
          Just is -> GRE { gre_name = NormalGreName n, gre_par = mkParent n avail
                         , gre_lcl = False, gre_imp = [is] }

    mk_fld_gre fl
      = case prov_fn (flSelector fl) of  -- Nothing => bound locally
                           -- Just is => imported from 'is'
          Nothing -> GRE { gre_name = FieldGreName fl, gre_par = availParent avail
                         , gre_lcl = True, gre_imp = [] }
          Just is -> GRE { gre_name = FieldGreName fl, gre_par = availParent avail
                         , gre_lcl = False, gre_imp = [is] }

instance HasOccName GlobalRdrElt where
  occName = greOccName

-- | See Note [GreNames]
greOccName :: GlobalRdrElt -> OccName
greOccName = occName . gre_name

-- | A 'Name' for the GRE for internal use.  Careful: the 'OccName' of this
-- 'Name' is not necessarily the same as the 'greOccName' (see Note [GreNames]).
greMangledName :: GlobalRdrElt -> Name
greMangledName = greNameMangledName . gre_name

-- | A 'Name' for the GRE suitable for output to the user.  Its 'OccName' will
-- be the 'greOccName' (see Note [GreNames]).
grePrintableName :: GlobalRdrElt -> Name
grePrintableName = greNamePrintableName . gre_name

-- | The SrcSpan of the name pointed to by the GRE.
greDefinitionSrcSpan :: GlobalRdrElt -> SrcSpan
greDefinitionSrcSpan = nameSrcSpan . greMangledName

-- | The module in which the name pointed to by the GRE is defined.
greDefinitionModule :: GlobalRdrElt -> Maybe Module
greDefinitionModule = nameModule_maybe . greMangledName

greQualModName :: GlobalRdrElt -> ModuleName
-- Get a suitable module qualifier for the GRE
-- (used in mkPrintUnqualified)
-- Prerecondition: the greMangledName is always External
greQualModName gre@(GRE { gre_lcl = lcl, gre_imp = iss })
 | lcl, Just mod <- greDefinitionModule gre = moduleName mod
 | (is:_) <- iss                            = is_as (is_decl is)
 | otherwise                                = pprPanic "greQualModName" (ppr gre)

greRdrNames :: GlobalRdrElt -> [RdrName]
greRdrNames gre@GRE{ gre_lcl = lcl, gre_imp = iss }
  = (if lcl then [unqual] else []) ++ concatMap do_spec (map is_decl iss)
  where
    occ    = greOccName gre
    unqual = Unqual occ
    do_spec decl_spec
        | is_qual decl_spec = [qual]
        | otherwise         = [unqual,qual]
        where qual = Qual (is_as decl_spec) occ

-- the SrcSpan that pprNameProvenance prints out depends on whether
-- the Name is defined locally or not: for a local definition the
-- definition site is used, otherwise the location of the import
-- declaration.  We want to sort the export locations in
-- exportClashErr by this SrcSpan, we need to extract it:
greSrcSpan :: GlobalRdrElt -> SrcSpan
greSrcSpan gre@(GRE { gre_lcl = lcl, gre_imp = iss } )
  | lcl           = greDefinitionSrcSpan gre
  | (is:_) <- iss = is_dloc (is_decl is)
  | otherwise     = pprPanic "greSrcSpan" (ppr gre)

mkParent :: Name -> AvailInfo -> Parent
mkParent _ (Avail _)                 = NoParent
mkParent n (AvailTC m _) | n == m    = NoParent
                         | otherwise = ParentIs m

availParent :: AvailInfo -> Parent
availParent (AvailTC m _) = ParentIs m
availParent (Avail {})    = NoParent


greParent_maybe :: GlobalRdrElt -> Maybe Name
greParent_maybe gre = case gre_par gre of
                        NoParent      -> Nothing
                        ParentIs n    -> Just n

-- | Takes a list of distinct GREs and folds them
-- into AvailInfos. This is more efficient than mapping each individual
-- GRE to an AvailInfo and the folding using `plusAvail` but needs the
-- uniqueness assumption.
gresToAvailInfo :: [GlobalRdrElt] -> [AvailInfo]
gresToAvailInfo gres
  = nameEnvElts avail_env
  where
    avail_env :: NameEnv AvailInfo -- Keyed by the parent
    (avail_env, _) = foldl' add (emptyNameEnv, emptyNameSet) gres

    add :: (NameEnv AvailInfo, NameSet)
        -> GlobalRdrElt
        -> (NameEnv AvailInfo, NameSet)
    add (env, done) gre
      | name `elemNameSet` done
      = (env, done)  -- Don't insert twice into the AvailInfo
      | otherwise
      = ( extendNameEnv_Acc comb availFromGRE env key gre
        , done `extendNameSet` name )
      where
        name = greMangledName gre
        key = case greParent_maybe gre of
                 Just parent -> parent
                 Nothing     -> greMangledName gre

        -- We want to insert the child `k` into a list of children but
        -- need to maintain the invariant that the parent is first.
        --
        -- We also use the invariant that `k` is not already in `ns`.
        insertChildIntoChildren :: Name -> [GreName] -> GreName -> [GreName]
        insertChildIntoChildren _ [] k = [k]
        insertChildIntoChildren p (n:ns) k
          | NormalGreName p == k = k:n:ns
          | otherwise = n:k:ns

        comb :: GlobalRdrElt -> AvailInfo -> AvailInfo
        comb _   (Avail n) = Avail n -- Duplicated name, should not happen
        comb gre (AvailTC m ns)
          = case gre_par gre of
              NoParent    -> AvailTC m (gre_name gre:ns) -- Not sure this ever happens
              ParentIs {} -> AvailTC m (insertChildIntoChildren m ns (gre_name gre))

availFromGRE :: GlobalRdrElt -> AvailInfo
availFromGRE (GRE { gre_name = child, gre_par = parent })
  = case parent of
      ParentIs p -> AvailTC p [child]
      NoParent | NormalGreName me <- child, isTyConName me -> AvailTC me [child]
               | otherwise -> Avail child

emptyGlobalRdrEnv :: GlobalRdrEnv
emptyGlobalRdrEnv = emptyOccEnv

globalRdrEnvElts :: GlobalRdrEnv -> [GlobalRdrElt]
globalRdrEnvElts env = foldOccEnv (++) [] env

instance Outputable GlobalRdrElt where
  ppr gre = hang (ppr (greMangledName gre) <+> ppr (gre_par gre))
               2 (pprNameProvenance gre)

pprGlobalRdrEnv :: Bool -> GlobalRdrEnv -> SDoc
pprGlobalRdrEnv locals_only env
  = vcat [ text "GlobalRdrEnv" <+> ppWhen locals_only (ptext (sLit "(locals only)"))
             <+> lbrace
         , nest 2 (vcat [ pp (remove_locals gre_list) | gre_list <- occEnvElts env ]
             <+> rbrace) ]
  where
    remove_locals gres | locals_only = filter isLocalGRE gres
                       | otherwise   = gres
    pp []   = empty
    pp gres = hang (ppr occ
                     <+> parens (text "unique" <+> ppr (getUnique occ))
                     <> colon)
                 2 (vcat (map ppr gres))
      where
        occ = nameOccName (greMangledName (head gres))

lookupGlobalRdrEnv :: GlobalRdrEnv -> OccName -> [GlobalRdrElt]
lookupGlobalRdrEnv env occ_name = case lookupOccEnv env occ_name of
                                  Nothing   -> []
                                  Just gres -> gres

lookupGRE_RdrName :: RdrName -> GlobalRdrEnv -> [GlobalRdrElt]
lookupGRE_RdrName rdr_name env
  = case lookupOccEnv env (rdrNameOcc rdr_name) of
    Nothing   -> []
    Just gres -> pickGREs rdr_name gres

lookupGRE_Name :: GlobalRdrEnv -> Name -> Maybe GlobalRdrElt
-- ^ Look for precisely this 'Name' in the environment.  This tests
-- whether it is in scope, ignoring anything else that might be in
-- scope with the same 'OccName'.
lookupGRE_Name env name
  = lookupGRE_Name_OccName env name (nameOccName name)

lookupGRE_GreName :: GlobalRdrEnv -> GreName -> Maybe GlobalRdrElt
-- ^ Look for precisely this 'GreName' in the environment.  This tests
-- whether it is in scope, ignoring anything else that might be in
-- scope with the same 'OccName'.
lookupGRE_GreName env gname
  = lookupGRE_Name_OccName env (greNameMangledName gname) (occName gname)

lookupGRE_FieldLabel :: GlobalRdrEnv -> FieldLabel -> Maybe GlobalRdrElt
-- ^ Look for a particular record field selector in the environment, where the
-- selector name and field label may be different: the GlobalRdrEnv is keyed on
-- the label.  See Note [Parents for record fields] for why this happens.
lookupGRE_FieldLabel env fl
  = lookupGRE_Name_OccName env (flSelector fl) (mkVarOccFS (flLabel fl))

lookupGRE_Name_OccName :: GlobalRdrEnv -> Name -> OccName -> Maybe GlobalRdrElt
-- ^ Look for precisely this 'Name' in the environment, but with an 'OccName'
-- that might differ from that of the 'Name'.  See 'lookupGRE_FieldLabel' and
-- Note [Parents for record fields].
lookupGRE_Name_OccName env name occ
  = case [ gre | gre <- lookupGlobalRdrEnv env occ
               , greMangledName gre == name ] of
      []    -> Nothing
      [gre] -> Just gre
      gres  -> pprPanic "lookupGRE_Name_OccName"
                        (ppr name $$ ppr occ $$ ppr gres)
               -- See INVARIANT 1 on GlobalRdrEnv


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
      | otherwise = Just $ map (is_as . is_decl) iss

isLocalGRE :: GlobalRdrElt -> Bool
isLocalGRE (GRE {gre_lcl = lcl }) = lcl

isRecFldGRE :: GlobalRdrElt -> Bool
isRecFldGRE = isJust . greFieldLabel

isOverloadedRecFldGRE :: GlobalRdrElt -> Bool
-- ^ Is this a record field defined with DuplicateRecordFields?
-- (See Note [Parents for record fields])
isOverloadedRecFldGRE = maybe False flIsOverloaded . greFieldLabel

greFieldLabel :: GlobalRdrElt -> Maybe FieldLabel
-- ^ Returns the field label of this GRE, if it has one
greFieldLabel = greNameFieldLabel . gre_name

unQualOK :: GlobalRdrElt -> Bool
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

pickGREs :: RdrName -> [GlobalRdrElt] -> [GlobalRdrElt]
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

pickUnqualGRE :: GlobalRdrElt -> Maybe GlobalRdrElt
pickUnqualGRE gre@(GRE { gre_lcl = lcl, gre_imp = iss })
  | not lcl, null iss' = Nothing
  | otherwise          = Just (gre { gre_imp = iss' })
  where
    iss' = filter unQualSpecOK iss

pickQualGRE :: ModuleName -> GlobalRdrElt -> Maybe GlobalRdrElt
pickQualGRE mod gre@(GRE { gre_lcl = lcl, gre_imp = iss })
  | not lcl', null iss' = Nothing
  | otherwise           = Just (gre { gre_lcl = lcl', gre_imp = iss' })
  where
    iss' = filter (qualSpecOK mod) iss
    lcl' = lcl && name_is_from mod

    name_is_from :: ModuleName -> Bool
    name_is_from mod = case greDefinitionModule gre of
                         Just n_mod -> moduleName n_mod == mod
                         Nothing    -> False

pickGREsModExp :: ModuleName -> [GlobalRdrElt] -> [(GlobalRdrElt,GlobalRdrElt)]
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
pickBothGRE :: ModuleName -> GlobalRdrElt -> Maybe (GlobalRdrElt, GlobalRdrElt)
pickBothGRE mod gre
  | isBuiltInSyntax (greMangledName gre)   = Nothing
  | Just gre1 <- pickQualGRE mod gre
  , Just gre2 <- pickUnqualGRE   gre = Just (gre1, gre2)
  | otherwise                        = Nothing

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
        | gre_name new_g == gre_name old_g
        = new_g `plusGRE` old_g : old_gs
        | otherwise
        = old_g : insertGRE new_g old_gs

plusGRE :: GlobalRdrElt -> GlobalRdrElt -> GlobalRdrElt
-- Used when the gre_name fields match
plusGRE g1 g2
  = GRE { gre_name = gre_name g1
        , gre_lcl  = gre_lcl g1 || gre_lcl g2
        , gre_imp  = gre_imp g1 ++ gre_imp g2
        , gre_par  = gre_par  g1 `plusParent` gre_par  g2 }

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

shadowNames :: GlobalRdrEnv -> [Name] -> GlobalRdrEnv
shadowNames = foldl' shadowName

{- Note [GlobalRdrEnv shadowing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before adding new names to the GlobalRdrEnv we nuke some existing entries;
this is "shadowing".  The actual work is done by RdrEnv.shadowName.
Suppose
   env' = shadowName env M.f

Then:
   * Looking up (Unqual f) in env' should succeed, returning M.f,
     even if env contains existing unqualified bindings for f.
     They are shadowed

   * Looking up (Qual M.f) in env' should succeed, returning M.f

   * Looking up (Qual X.f) in env', where X /= M, should be the same as
     looking up (Qual X.f) in env.
     That is, shadowName does /not/ delete earlier qualified bindings

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
    hence the `mk_fake-imp_spec` in `shadowName`.  See also Note
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
-}

shadowName :: GlobalRdrEnv -> Name -> GlobalRdrEnv
-- Remove certain old GREs that share the same OccName as this new Name.
-- See Note [GlobalRdrEnv shadowing] for details
shadowName env name
  = alterOccEnv (fmap alter_fn) env (nameOccName name)
  where
    alter_fn :: [GlobalRdrElt] -> [GlobalRdrElt]
    alter_fn gres = mapMaybe (shadow_with name) gres

    shadow_with :: Name -> GlobalRdrElt -> Maybe GlobalRdrElt
    shadow_with new_name
       old_gre@(GRE { gre_lcl = lcl, gre_imp = iss })
       = case greDefinitionModule old_gre of
           Nothing -> Just old_gre   -- Old name is Internal; do not shadow
           Just old_mod
              | Just new_mod <- nameModule_maybe new_name
              , new_mod == old_mod   -- Old name same as new name; shadow completely
              -> Nothing

              | null iss'            -- Nothing remains
              -> Nothing

              | otherwise
              -> Just (old_gre { gre_lcl = False, gre_imp = iss' })

              where
                iss' = lcl_imp ++ mapMaybe (shadow_is new_name) iss
                lcl_imp | lcl       = [mk_fake_imp_spec old_gre old_mod]
                        | otherwise = []

    mk_fake_imp_spec old_gre old_mod    -- Urgh!
      = ImpSpec id_spec ImpAll
      where
        old_mod_name = moduleName old_mod
        id_spec      = ImpDeclSpec { is_mod = old_mod_name
                                   , is_as = old_mod_name
                                   , is_qual = True
                                   , is_dloc = greDefinitionSrcSpan old_gre }

    shadow_is :: Name -> ImportSpec -> Maybe ImportSpec
    shadow_is new_name is@(ImpSpec { is_decl = id_spec })
       | Just new_mod <- nameModule_maybe new_name
       , is_as id_spec == moduleName new_mod
       = Nothing   -- Shadow both qualified and unqualified
       | otherwise -- Shadow unqualified only
       = Just (is { is_decl = id_spec { is_qual = True } })


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
data ImportSpec = ImpSpec { is_decl :: ImpDeclSpec,
                            is_item :: ImpItemSpec }
                deriving( Eq, Data )

-- | Import Declaration Specification
--
-- Describes a particular import declaration and is
-- shared among all the 'Provenance's for that decl
data ImpDeclSpec
  = ImpDeclSpec {
        is_mod      :: ModuleName, -- ^ Module imported, e.g. @import Muggle@
                                   -- Note the @Muggle@ may well not be
                                   -- the defining module for this thing!

                                   -- TODO: either should be Module, or there
                                   -- should be a Maybe UnitId here too.
        is_as       :: ModuleName, -- ^ Import alias, e.g. from @as M@ (or @Muggle@ if there is no @as@ clause)
        is_qual     :: Bool,       -- ^ Was this import qualified?
        is_dloc     :: SrcSpan     -- ^ The location of the entire import declaration
    } deriving (Eq, Data)

-- | Import Item Specification
--
-- Describes import info a particular Name
data ImpItemSpec
  = ImpAll              -- ^ The import had no import list,
                        -- or had a hiding list

  | ImpSome {
        is_explicit :: Bool,
        is_iloc     :: SrcSpan  -- Location of the import item
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

bestImport :: [ImportSpec] -> ImportSpec
-- See Note [Choosing the best import declaration]
bestImport iss
  = case sortBy best iss of
      (is:_) -> is
      []     -> pprPanic "bestImport" (ppr iss)
  where
    best :: ImportSpec -> ImportSpec -> Ordering
    -- Less means better
    -- Unqualified always wins over qualified; then
    -- import-all wins over import-some; then
    -- earlier declaration wins over later
    best (ImpSpec { is_item = item1, is_decl = d1 })
         (ImpSpec { is_item = item2, is_decl = d2 })
      = (is_qual d1 `compare` is_qual d2) `thenCmp`
        (best_item item1 item2)           `thenCmp`
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
importSpecModule is = is_mod (is_decl is)

isExplicitItem :: ImpItemSpec -> Bool
isExplicitItem ImpAll                        = False
isExplicitItem (ImpSome {is_explicit = exp}) = exp

pprNameProvenance :: GlobalRdrElt -> SDoc
-- ^ Print out one place where the name was define/imported
-- (With -dppr-debug, print them all)
pprNameProvenance gre@(GRE { gre_lcl = lcl, gre_imp = iss })
  = ifPprDebug (vcat pp_provs)
               (head pp_provs)
  where
    name = greMangledName gre
    pp_provs = pp_lcl ++ map pp_is iss
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
    defining_mod = ASSERT2( isExternalName name, ppr name ) nameModule name
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

-- | Display info about the treatment of '*' under NoStarIsType.
--
-- With StarIsType, three properties of '*' hold:
--
--   (a) it is not an infix operator
--   (b) it is always in scope
--   (c) it is a synonym for Data.Kind.Type
--
-- However, the user might not know that they are working on a module with
-- NoStarIsType and write code that still assumes (a), (b), and (c), which
-- actually do not hold in that module.
--
-- Violation of (a) shows up in the parser. For instance, in the following
-- examples, we have '*' not applied to enough arguments:
--
--   data A :: *
--   data F :: * -> *
--
-- Violation of (b) or (c) show up in the renamer and the typechecker
-- respectively. For instance:
--
--   type K = Either * Bool
--
-- This will parse differently depending on whether StarIsType is enabled,
-- but it will parse nonetheless. With NoStarIsType it is parsed as a type
-- operator, thus we have ((*) Either Bool). Now there are two cases to
-- consider:
--
--   1. There is no definition of (*) in scope. In this case the renamer will
--      fail to look it up. This is a violation of assumption (b).
--
--   2. There is a definition of the (*) type operator in scope (for example
--      coming from GHC.TypeNats). In this case the user will get a kind
--      mismatch error. This is a violation of assumption (c).
--
-- The user might unknowingly be working on a module with NoStarIsType
-- or use '*' as 'Data.Kind.Type' out of habit. So it is important to give a
-- hint whenever an assumption about '*' is violated. Unfortunately, it is
-- somewhat difficult to deal with (c), so we limit ourselves to (a) and (b).
--
-- 'starInfo' generates an appropriate hint to the user depending on the
-- extensions enabled in the module and the name that triggered the error.
-- That is, if we have NoStarIsType and the error is related to '*' or its
-- Unicode variant, the resulting SDoc will contain a helpful suggestion.
-- Otherwise it is empty.
--
starInfo :: Bool -> RdrName -> SDoc
starInfo star_is_type rdr_name =
  -- One might ask: if can use `sdocOption sdocStarIsType` here, why bother to
  -- take star_is_type as input? Why not refactor?
  --
  -- The reason is that `sdocOption sdocStarIsType` would indicate that
  -- StarIsType is enabled in the module that tries to load the problematic
  -- definition, not in the module that is being loaded.
  --
  -- So if we have 'data T :: *' in a module with NoStarIsType, then the hint
  -- must be displayed even if we load this definition from a module (or GHCi)
  -- with StarIsType enabled!
  --
  if isUnqualStar && not star_is_type
     then text "With NoStarIsType, " <>
          quotes (ppr rdr_name) <>
          text " is treated as a regular type operator. "
        $$
          text "Did you mean to use " <> quotes (text "Type") <>
          text " from Data.Kind instead?"
      else empty
  where
    -- Does rdr_name look like the user might have meant the '*' kind by it?
    -- We focus on unqualified stars specifically, because qualified stars are
    -- treated as type operators even under StarIsType.
    isUnqualStar
      | Unqual occName <- rdr_name
      = let fs = occNameFS occName
        in fs == fsLit "*" || fs == fsLit "★"
      | otherwise = False

-- | Indicate if the given name is the "@" operator
opIsAt :: RdrName -> Bool
opIsAt e = e == mkUnqual varName (fsLit "@")
