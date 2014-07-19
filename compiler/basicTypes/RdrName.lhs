%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName': see "OccName#name_types"
--
-- * 'RdrName.RdrName' is the type of names that come directly from the parser. They
--   have not yet had their scoping and binding resolved by the renamer and can be
--   thought of to a first approximation as an 'OccName.OccName' with an optional module
--   qualifier
--
-- * 'Name.Name': see "Name#name_types"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var': see "Var#name_types"

module RdrName (
        -- * The main type
        RdrName(..),    -- Constructors exported only to BinIface

        -- ** Construction
        mkRdrUnqual, mkRdrQual,
        mkUnqual, mkVarUnqual, mkQual, mkOrig,
        nameRdrName, getRdrName,

        -- ** Destruction
        rdrNameOcc, rdrNameSpace, setRdrNameSpace, demoteRdrName,
        isRdrDataCon, isRdrTyVar, isRdrTc, isQual, isQual_maybe, isUnqual,
        isOrig, isOrig_maybe, isExact, isExact_maybe, isSrcRdrName,

        -- * Local mapping of 'RdrName' to 'Name.Name'
        LocalRdrEnv, emptyLocalRdrEnv, extendLocalRdrEnv, extendLocalRdrEnvList,
        lookupLocalRdrEnv, lookupLocalRdrOcc,
        elemLocalRdrEnv, inLocalRdrEnvScope,
        localRdrEnvElts, delLocalRdrEnvList,

        -- * Global mapping of 'RdrName' to 'GlobalRdrElt's
        GlobalRdrEnv, emptyGlobalRdrEnv, mkGlobalRdrEnv, plusGlobalRdrEnv,
        lookupGlobalRdrEnv, extendGlobalRdrEnv, 
        pprGlobalRdrEnv, globalRdrEnvElts,
        lookupGRE_RdrName, lookupGRE_Name, getGRE_NameQualifier_maybes,
        transformGREs, findLocalDupsRdrEnv, pickGREs,

        -- * GlobalRdrElts
        gresFromAvails, gresFromAvail,

        -- ** Global 'RdrName' mapping elements: 'GlobalRdrElt', 'Provenance', 'ImportSpec'
        GlobalRdrElt(..), isLocalGRE, unQualOK, qualSpecOK, unQualSpecOK,
        Provenance(..), pprNameProvenance,
        Parent(..),
        ImportSpec(..), ImpDeclSpec(..), ImpItemSpec(..),
        importSpecLoc, importSpecModule, isExplicitItem
  ) where

#include "HsVersions.h"

import Module
import Name
import Avail
import NameSet
import Maybes
import SrcLoc
import FastString
import Outputable
import Unique
import Util
import StaticFlags( opt_PprStyle_Debug )

import Data.Data
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The main data type}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Do not use the data constructors of RdrName directly: prefer the family
-- of functions that creates them, such as 'mkRdrUnqual'
data RdrName
  = Unqual OccName
        -- ^ Used for ordinary, unqualified occurrences, e.g. @x@, @y@ or @Foo@.
        -- Create such a 'RdrName' with 'mkRdrUnqual'

  | Qual ModuleName OccName
        -- ^ A qualified name written by the user in
        -- /source/ code.  The module isn't necessarily
        -- the module where the thing is defined;
        -- just the one from which it is imported.
        -- Examples are @Bar.x@, @Bar.y@ or @Bar.Foo@.
        -- Create such a 'RdrName' with 'mkRdrQual'

  | Orig Module OccName
        -- ^ An original name; the module is the /defining/ module.
        -- This is used when GHC generates code that will be fed
        -- into the renamer (e.g. from deriving clauses), but where
        -- we want to say \"Use Prelude.map dammit\". One of these
        -- can be created with 'mkOrig'

  | Exact Name
        -- ^ We know exactly the 'Name'. This is used:
        --
        --  (1) When the parser parses built-in syntax like @[]@
        --      and @(,)@, but wants a 'RdrName' from it
        --
        --  (2) By Template Haskell, when TH has generated a unique name
        --
        -- Such a 'RdrName' can be created by using 'getRdrName' on a 'Name'
  deriving (Data, Typeable)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Simple functions}
%*                                                                      *
%************************************************************************

\begin{code}

instance HasOccName RdrName where
  occName = rdrNameOcc

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (Qual _ occ) = occ
rdrNameOcc (Unqual occ) = occ
rdrNameOcc (Orig _ occ) = occ
rdrNameOcc (Exact name) = nameOccName name

rdrNameSpace :: RdrName -> NameSpace
rdrNameSpace = occNameSpace . rdrNameOcc

setRdrNameSpace :: RdrName -> NameSpace -> RdrName
-- ^ This rather gruesome function is used mainly by the parser.
-- When parsing:
--
-- > data T a = T | T1 Int
--
-- we parse the data constructors as /types/ because of parser ambiguities,
-- so then we need to change the /type constr/ to a /data constr/
--
-- The exact-name case /can/ occur when parsing:
--
-- > data [] a = [] | a : [a]
--
-- For the exact-name case we return an original name.
setRdrNameSpace (Unqual occ) ns = Unqual (setOccNameSpace ns occ)
setRdrNameSpace (Qual m occ) ns = Qual m (setOccNameSpace ns occ)
setRdrNameSpace (Orig m occ) ns = Orig m (setOccNameSpace ns occ)
setRdrNameSpace (Exact n)    ns = ASSERT( isExternalName n )
                                  Orig (nameModule n)
                                       (setOccNameSpace ns (nameOccName n))

-- demoteRdrName lowers the NameSpace of RdrName.
-- see Note [Demotion] in OccName
demoteRdrName :: RdrName -> Maybe RdrName
demoteRdrName (Unqual occ) = fmap Unqual (demoteOccName occ)
demoteRdrName (Qual m occ) = fmap (Qual m) (demoteOccName occ)
demoteRdrName (Orig _ _) = panic "demoteRdrName"
demoteRdrName (Exact _) = panic "demoteRdrName"
\end{code}

\begin{code}
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
\end{code}

\begin{code}
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
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Instances}
%*                                                                      *
%************************************************************************

\begin{code}
instance Outputable RdrName where
    ppr (Exact name)   = ppr name
    ppr (Unqual occ)   = ppr occ
    ppr (Qual mod occ) = ppr mod <> dot <> ppr occ
    ppr (Orig mod occ) = getPprStyle (\sty -> pprModulePrefix sty mod occ <> ppr occ)

instance OutputableBndr RdrName where
    pprBndr _ n
        | isTvOcc (rdrNameOcc n) = char '@' <+> ppr n
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
\end{code}

%************************************************************************
%*                                                                      *
                        LocalRdrEnv
%*                                                                      *
%************************************************************************

\begin{code}
-- | This environment is used to store local bindings (@let@, @where@, lambda, @case@).
-- It is keyed by OccName, because we never use it for qualified names
-- We keep the current mapping, *and* the set of all Names in scope
-- Reason: see Note [Splicing Exact Names] in RnEnv
data LocalRdrEnv = LRE { lre_env      :: OccEnv Name
                       , lre_in_scope :: NameSet }

instance Outputable LocalRdrEnv where
  ppr (LRE {lre_env = env, lre_in_scope = ns})
    = hang (ptext (sLit "LocalRdrEnv {"))
         2 (vcat [ ptext (sLit "env =") <+> pprOccEnv ppr_elt env
                 , ptext (sLit "in_scope =") <+> braces (pprWithCommas ppr (nameSetToList ns))
                 ] <+> char '}')
    where
      ppr_elt name = parens (ppr (getUnique (nameOccName name))) <+> ppr name
                     -- So we can see if the keys line up correctly

emptyLocalRdrEnv :: LocalRdrEnv
emptyLocalRdrEnv = LRE { lre_env = emptyOccEnv, lre_in_scope = emptyNameSet }

extendLocalRdrEnv :: LocalRdrEnv -> Name -> LocalRdrEnv
-- The Name should be a non-top-level thing
extendLocalRdrEnv (LRE { lre_env = env, lre_in_scope = ns }) name
  = WARN( isExternalName name, ppr name )
    LRE { lre_env      = extendOccEnv env (nameOccName name) name
        , lre_in_scope = addOneToNameSet ns name }

extendLocalRdrEnvList :: LocalRdrEnv -> [Name] -> LocalRdrEnv
extendLocalRdrEnvList (LRE { lre_env = env, lre_in_scope = ns }) names
  = WARN( any isExternalName names, ppr names )
    LRE { lre_env = extendOccEnvList env [(nameOccName n, n) | n <- names]
        , lre_in_scope = addListToNameSet ns names }

lookupLocalRdrEnv :: LocalRdrEnv -> RdrName -> Maybe Name
lookupLocalRdrEnv (LRE { lre_env = env }) (Unqual occ) = lookupOccEnv env occ
lookupLocalRdrEnv _                       _            = Nothing

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
delLocalRdrEnvList (LRE { lre_env = env, lre_in_scope = ns }) occs 
  = LRE { lre_env = delListFromOccEnv env occs
        , lre_in_scope = ns }
\end{code}

Note [Local bindings with Exact Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With Template Haskell we can make local bindings that have Exact Names.
Computing shadowing etc may use elemLocalRdrEnv (at least it certainly
does so in RnTpes.bindHsTyVars), so for an Exact Name we must consult
the in-scope-name-set.


%************************************************************************
%*                                                                      *
                        GlobalRdrEnv
%*                                                                      *
%************************************************************************

\begin{code}
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
-- INVARIANT: All the members of the list have distinct
--            'gre_name' fields; that is, no duplicate Names
--
-- INVARIANT: Imported provenance => Name is an ExternalName
--            However LocalDefs can have an InternalName.  This
--            happens only when type-checking a [d| ... |] Template
--            Haskell quotation; see this note in RnNames
--            Note [Top-level Names in Template Haskell decl quotes]

-- | An element of the 'GlobalRdrEnv'
data GlobalRdrElt
  = GRE { gre_name :: Name,
          gre_par  :: Parent,
          gre_prov :: Provenance        -- ^ Why it's in scope
    }

-- | The children of a Name are the things that are abbreviated by the ".."
--   notation in export lists.  See Note [Parents]
data Parent = NoParent | ParentIs Name
              deriving (Eq)

instance Outputable Parent where
   ppr NoParent     = empty
   ppr (ParentIs n) = ptext (sLit "parent:") <> ppr n

plusParent :: Parent -> Parent -> Parent
-- See Note [Combining parents]
plusParent (ParentIs n) p2 = hasParent n p2
plusParent p1 (ParentIs n) = hasParent n p1
plusParent _ _ = NoParent

hasParent :: Name -> Parent -> Parent
#ifdef DEBUG
hasParent n (ParentIs n')
  | n /= n' = pprPanic "hasParent" (ppr n <+> ppr n')  -- Parents should agree
#endif
hasParent n _  = ParentIs n
\end{code}

Note [Parents]
~~~~~~~~~~~~~~~~~
  Parent           Children
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data T           Data constructors
                   Record-field ids

  data family T    Data constructors and record-field ids
                   of all visible data instances of T

  class C          Class operations
                   Associated type constructors

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
On import we convert to GlobalRdrElt and the combine
those.  For T that will mean we have
  one GRE with Parent C
  one GRE with NoParent
That's why plusParent picks the "best" case.


\begin{code}
-- | make a 'GlobalRdrEnv' where all the elements point to the same
-- Provenance (useful for "hiding" imports, or imports with
-- no details).
gresFromAvails :: Provenance -> [AvailInfo] -> [GlobalRdrElt]
gresFromAvails prov avails
  = concatMap (gresFromAvail (const prov)) avails

gresFromAvail :: (Name -> Provenance) -> AvailInfo -> [GlobalRdrElt]
gresFromAvail prov_fn avail
  = [ GRE {gre_name = n,
           gre_par = mkParent n avail,
           gre_prov = prov_fn n}
    | n <- availNames avail ]
  where

mkParent :: Name -> AvailInfo -> Parent
mkParent _ (Avail _)                 = NoParent
mkParent n (AvailTC m _) | n == m    = NoParent
                         | otherwise = ParentIs m

emptyGlobalRdrEnv :: GlobalRdrEnv
emptyGlobalRdrEnv = emptyOccEnv

globalRdrEnvElts :: GlobalRdrEnv -> [GlobalRdrElt]
globalRdrEnvElts env = foldOccEnv (++) [] env

instance Outputable GlobalRdrElt where
  ppr gre = hang (ppr (gre_name gre) <+> ppr (gre_par gre))
               2 (pprNameProvenance gre)

pprGlobalRdrEnv :: Bool -> GlobalRdrEnv -> SDoc
pprGlobalRdrEnv locals_only env
  = vcat [ ptext (sLit "GlobalRdrEnv") <+> ppWhen locals_only (ptext (sLit "(locals only)")) 
             <+> lbrace
         , nest 2 (vcat [ pp (remove_locals gre_list) | gre_list <- occEnvElts env ] 
             <+> rbrace) ]
  where
    remove_locals gres | locals_only = filter isLocalGRE gres
                       | otherwise   = gres
    pp []   = empty
    pp gres = hang (ppr occ
                     <+> parens (ptext (sLit "unique") <+> ppr (getUnique occ))
                     <> colon)
                 2 (vcat (map ppr gres))
      where
        occ = nameOccName (gre_name (head gres))

lookupGlobalRdrEnv :: GlobalRdrEnv -> OccName -> [GlobalRdrElt]
lookupGlobalRdrEnv env occ_name = case lookupOccEnv env occ_name of
                                  Nothing   -> []
                                  Just gres -> gres

lookupGRE_RdrName :: RdrName -> GlobalRdrEnv -> [GlobalRdrElt]
lookupGRE_RdrName rdr_name env
  = case lookupOccEnv env (rdrNameOcc rdr_name) of
    Nothing   -> []
    Just gres -> pickGREs rdr_name gres

lookupGRE_Name :: GlobalRdrEnv -> Name -> [GlobalRdrElt]
lookupGRE_Name env name
  = [ gre | gre <- lookupGlobalRdrEnv env (nameOccName name),
            gre_name gre == name ]

getGRE_NameQualifier_maybes :: GlobalRdrEnv -> Name -> [Maybe [ModuleName]]
-- Returns all the qualifiers by which 'x' is in scope
-- Nothing means "the unqualified version is in scope"
-- [] means the thing is not in scope at all
getGRE_NameQualifier_maybes env
  = map (qualifier_maybe . gre_prov) . lookupGRE_Name env
  where
    qualifier_maybe LocalDef       = Nothing
    qualifier_maybe (Imported iss) = Just $ map (is_as . is_decl) iss

isLocalGRE :: GlobalRdrElt -> Bool
isLocalGRE (GRE {gre_prov = LocalDef}) = True
isLocalGRE _                           = False

unQualOK :: GlobalRdrElt -> Bool
-- ^ Test if an unqualifed version of this thing would be in scope
unQualOK (GRE {gre_prov = LocalDef})    = True
unQualOK (GRE {gre_prov = Imported is}) = any unQualSpecOK is

pickGREs :: RdrName -> [GlobalRdrElt] -> [GlobalRdrElt]
-- ^ Take a list of GREs which have the right OccName
-- Pick those GREs that are suitable for this RdrName
-- And for those, keep only only the Provenances that are suitable
-- Only used for Qual and Unqual, not Orig or Exact
--
-- Consider:
--
-- @
--       module A ( f ) where
--       import qualified Foo( f )
--       import Baz( f )
--       f = undefined
-- @
--
-- Let's suppose that @Foo.f@ and @Baz.f@ are the same entity really.
-- The export of @f@ is ambiguous because it's in scope from the local def
-- and the import.  The lookup of @Unqual f@ should return a GRE for
-- the locally-defined @f@, and a GRE for the imported @f@, with a /single/
-- provenance, namely the one for @Baz(f)@.
pickGREs rdr_name gres
  | (_ : _ : _) <- candidates  -- This is usually false, so we don't have to
                               -- even look at internal_candidates
  , (gre : _)   <- internal_candidates
  = [gre]  -- For this internal_candidate stuff,
           -- see Note [Template Haskell binders in the GlobalRdrEnv]
           -- If there are multiple Internal candidates, pick the
           -- first one (ie with the (innermost binding)
  | otherwise
  = ASSERT2( isSrcRdrName rdr_name, ppr rdr_name )
    candidates
  where
    candidates = mapMaybe pick gres
    internal_candidates = filter (isInternalName . gre_name) candidates

    rdr_is_unqual = isUnqual rdr_name
    rdr_is_qual   = isQual_maybe rdr_name

    pick :: GlobalRdrElt -> Maybe GlobalRdrElt
    pick gre@(GRE {gre_prov = LocalDef, gre_name = n})  -- Local def
        | rdr_is_unqual                    = Just gre
        | Just (mod,_) <- rdr_is_qual        -- Qualified name
        , Just n_mod <- nameModule_maybe n   -- Binder is External
        , mod == moduleName n_mod          = Just gre
        | otherwise                        = Nothing
    pick gre@(GRE {gre_prov = Imported [is]})   -- Single import (efficiency)
        | rdr_is_unqual,
          not (is_qual (is_decl is))    = Just gre
        | Just (mod,_) <- rdr_is_qual,
          mod == is_as (is_decl is)     = Just gre
        | otherwise                     = Nothing
    pick gre@(GRE {gre_prov = Imported is})     -- Multiple import
        | null filtered_is = Nothing
        | otherwise        = Just (gre {gre_prov = Imported filtered_is})
        where
          filtered_is | rdr_is_unqual
                      = filter (not . is_qual    . is_decl) is
                      | Just (mod,_) <- rdr_is_qual
                      = filter ((== mod) . is_as . is_decl) is
                      | otherwise
                      = []
\end{code}

Building GlobalRdrEnvs

\begin{code}
plusGlobalRdrEnv :: GlobalRdrEnv -> GlobalRdrEnv -> GlobalRdrEnv
plusGlobalRdrEnv env1 env2 = plusOccEnv_C (foldr insertGRE) env1 env2

mkGlobalRdrEnv :: [GlobalRdrElt] -> GlobalRdrEnv
mkGlobalRdrEnv gres
  = foldr add emptyGlobalRdrEnv gres
  where
    add gre env = extendOccEnv_Acc insertGRE singleton env
                                   (nameOccName (gre_name gre))
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
  = GRE { gre_name = gre_name g1,
          gre_prov = gre_prov g1 `plusProv`   gre_prov g2,
          gre_par  = gre_par  g1 `plusParent` gre_par  g2 }

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

extendGlobalRdrEnv :: Bool -> GlobalRdrEnv -> [AvailInfo] -> GlobalRdrEnv
-- Extend with new LocalDef GREs from the AvailInfos.
--
-- If do_shadowing is True, first remove name clashes between the new
-- AvailInfos and the existing GlobalRdrEnv.
-- This is used by the GHCi top-level
--
-- E.g.  Adding a LocalDef "x" when there is an existing GRE for Q.x
--       should remove any unqualified import of Q.x,
--       leaving only the qualified one
--
-- However do *not* remove name clashes between the AvailInfos themselves,
-- so that (say)   data T = A | A
-- will still give a duplicate-binding error.
-- Same thing if there are multiple AvailInfos (don't remove clashes),
-- though I'm not sure this ever happens with do_shadowing=True

extendGlobalRdrEnv do_shadowing env avails
  = foldl add_avail env1 avails
  where
    names = concatMap availNames avails
    env1 | do_shadowing = foldl shadow_name env names
         | otherwise    = env
         -- By doing the removal first, we ensure that the new AvailInfos
         -- don't shadow each other; that would conceal genuine errors
         -- E.g. in GHCi   data T = A | A

    add_avail env avail = foldl (add_name avail) env (availNames avail)

    add_name avail env name
       = extendOccEnv_Acc (:) singleton env occ gre
       where
         occ = nameOccName name
         gre = GRE { gre_name = name
                   , gre_par = mkParent name avail
                   , gre_prov = LocalDef }

shadow_name :: GlobalRdrEnv -> Name -> GlobalRdrEnv
shadow_name env name
  = alterOccEnv (fmap alter_fn) env (nameOccName name)
  where
    alter_fn :: [GlobalRdrElt] -> [GlobalRdrElt]
    alter_fn gres = mapMaybe (shadow_with name) gres

    shadow_with :: Name -> GlobalRdrElt -> Maybe GlobalRdrElt
    shadow_with new_name old_gre@(GRE { gre_name = old_name, gre_prov = LocalDef })
       = case (nameModule_maybe old_name, nameModule_maybe new_name) of
           (Nothing,      _)                                 -> Nothing
           (Just old_mod, Just new_mod) | new_mod == old_mod -> Nothing
           (Just old_mod, _) -> Just (old_gre { gre_prov = Imported [fake_imp_spec] })
              where
                 fake_imp_spec = ImpSpec id_spec ImpAll  -- Urgh!
                 old_mod_name = moduleName old_mod
                 id_spec = ImpDeclSpec { is_mod = old_mod_name
                                       , is_as = old_mod_name
                                       , is_qual = True
                                       , is_dloc = nameSrcSpan old_name }
    shadow_with new_name old_gre@(GRE { gre_prov = Imported imp_specs })
       | null imp_specs' = Nothing
       | otherwise       = Just (old_gre { gre_prov = Imported imp_specs' })
       where
         imp_specs' = mapMaybe (shadow_is new_name) imp_specs

    shadow_is :: Name -> ImportSpec -> Maybe ImportSpec
    shadow_is new_name is@(ImpSpec { is_decl = id_spec })
       | Just new_mod <- nameModule_maybe new_name
       , is_as id_spec == moduleName new_mod
       = Nothing   -- Shadow both qualified and unqualified
       | otherwise -- Shadow unqualified only
       = Just (is { is_decl = id_spec { is_qual = True } })
\end{code}

Note [Template Haskell binders in the GlobalRdrEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For reasons described in Note [Top-level Names in Template Haskell decl quotes]
in RnNames, a GRE with an Internal gre_name (i.e. one generated by a TH decl
quote) should *shadow* a GRE with an External gre_name.  Hence some faffing
around in pickGREs and findLocalDupsRdrEnv

\begin{code}
findLocalDupsRdrEnv :: GlobalRdrEnv -> [Name] -> [[GlobalRdrElt]]
-- ^ For each 'OccName', see if there are multiple local definitions
-- for it; return a list of all such
-- and return a list of the duplicate bindings
findLocalDupsRdrEnv rdr_env occs
  = go rdr_env [] occs
  where
    go _       dups [] = dups
    go rdr_env dups (name:names)
      = case filter (pick name) gres of
          []       -> go rdr_env  dups              names
          [_]      -> go rdr_env  dups              names   -- The common case
          dup_gres -> go rdr_env' (dup_gres : dups) names
      where
        occ      = nameOccName name
        gres     = lookupOccEnv rdr_env occ `orElse` []
        rdr_env' = delFromOccEnv rdr_env occ
            -- The delFromOccEnv avoids repeating the same
            -- complaint twice, when names itself has a duplicate
            -- which is a common case

    -- See Note [Template Haskell binders in the GlobalRdrEnv]
    pick name (GRE { gre_name = n, gre_prov = LocalDef })
      | isInternalName name = isInternalName n
      | otherwise           = True
    pick _ _ = False
\end{code}

%************************************************************************
%*                                                                      *
                        Provenance
%*                                                                      *
%************************************************************************

\begin{code}
-- | The 'Provenance' of something says how it came to be in scope.
-- It's quite elaborate so that we can give accurate unused-name warnings.
data Provenance
  = LocalDef            -- ^ The thing was defined locally
  | Imported
        [ImportSpec]    -- ^ The thing was imported.
                        --
                        -- INVARIANT: the list of 'ImportSpec' is non-empty

data ImportSpec = ImpSpec { is_decl :: ImpDeclSpec,
                            is_item :: ImpItemSpec }
                deriving( Eq, Ord )

-- | Describes a particular import declaration and is
-- shared among all the 'Provenance's for that decl
data ImpDeclSpec
  = ImpDeclSpec {
        is_mod      :: ModuleName, -- ^ Module imported, e.g. @import Muggle@
                                   -- Note the @Muggle@ may well not be
                                   -- the defining module for this thing!

                                   -- TODO: either should be Module, or there
                                   -- should be a Maybe PackageId here too.
        is_as       :: ModuleName, -- ^ Import alias, e.g. from @as M@ (or @Muggle@ if there is no @as@ clause)
        is_qual     :: Bool,       -- ^ Was this import qualified?
        is_dloc     :: SrcSpan     -- ^ The location of the entire import declaration
    }

-- | Describes import info a particular Name
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

-- Note [Comparing provenance]
-- Comparison of provenance is just used for grouping
-- error messages (in RnEnv.warnUnusedBinds)
instance Eq Provenance where
  p1 == p2 = case p1 `compare` p2 of EQ -> True; _ -> False

instance Eq ImpDeclSpec where
  p1 == p2 = case p1 `compare` p2 of EQ -> True; _ -> False

instance Eq ImpItemSpec where
  p1 == p2 = case p1 `compare` p2 of EQ -> True; _ -> False

instance Ord Provenance where
   compare LocalDef      LocalDef        = EQ
   compare LocalDef      (Imported _)    = LT
   compare (Imported _ ) LocalDef        = GT
   compare (Imported is1) (Imported is2) = compare (head is1)
        {- See Note [Comparing provenance] -}      (head is2)

instance Ord ImpDeclSpec where
   compare is1 is2 = (is_mod is1 `compare` is_mod is2) `thenCmp`
                     (is_dloc is1 `compare` is_dloc is2)

instance Ord ImpItemSpec where
   compare is1 is2 = is_iloc is1 `compare` is_iloc is2
\end{code}

\begin{code}
plusProv :: Provenance -> Provenance -> Provenance
-- Choose LocalDef over Imported
-- There is an obscure bug lurking here; in the presence
-- of recursive modules, something can be imported *and* locally
-- defined, and one might refer to it with a qualified name from
-- the import -- but I'm going to ignore that because it makes
-- the isLocalGRE predicate so much nicer this way
plusProv LocalDef        LocalDef        = panic "plusProv"
plusProv LocalDef        _               = LocalDef
plusProv _               LocalDef        = LocalDef
plusProv (Imported is1)  (Imported is2)  = Imported (is1++is2)

pprNameProvenance :: GlobalRdrElt -> SDoc
-- ^ Print out the place where the name was imported
pprNameProvenance (GRE {gre_name = name, gre_prov = LocalDef})
  = ptext (sLit "defined at") <+> ppr (nameSrcLoc name)
pprNameProvenance (GRE {gre_name = name, gre_prov = Imported whys})
  = case whys of
        (why:_) | opt_PprStyle_Debug -> vcat (map pp_why whys)
                | otherwise          -> pp_why why
        [] -> panic "pprNameProvenance"
  where
    pp_why why = sep [ppr why, ppr_defn_site why name]

-- If we know the exact definition point (which we may do with GHCi)
-- then show that too.  But not if it's just "imported from X".
ppr_defn_site :: ImportSpec -> Name -> SDoc
ppr_defn_site imp_spec name
  | same_module && not (isGoodSrcSpan loc)
  = empty              -- Nothing interesting to say
  | otherwise
  = parens $ hang (ptext (sLit "and originally defined") <+> pp_mod)
                2 (pprLoc loc)
  where
    loc = nameSrcSpan name
    defining_mod = nameModule name
    same_module = importSpecModule imp_spec == moduleName defining_mod
    pp_mod | same_module = empty
           | otherwise   = ptext (sLit "in") <+> quotes (ppr defining_mod)


instance Outputable ImportSpec where
   ppr imp_spec
     = ptext (sLit "imported") <+> qual
        <+> ptext (sLit "from") <+> quotes (ppr (importSpecModule imp_spec))
        <+> pprLoc (importSpecLoc imp_spec)
     where
       qual | is_qual (is_decl imp_spec) = ptext (sLit "qualified")
            | otherwise                  = empty

pprLoc :: SrcSpan -> SDoc
pprLoc (RealSrcSpan s)    = ptext (sLit "at") <+> ppr s
pprLoc (UnhelpfulSpan {}) = empty
\end{code}
