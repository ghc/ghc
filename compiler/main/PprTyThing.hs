-----------------------------------------------------------------------------
--
-- Pretty-printing TyThings
--
-- (c) The GHC Team 2005
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
module PprTyThing (
        pprTyThing,
        pprTyThingInContext,
        pprTyThingLoc,
        pprTyThingInContextLoc,
        pprTyThingHdr,
        pprTypeForUser,
        pprFamInst
  ) where

#include "HsVersions.h"

import GhcPrelude

import Type    ( ArgFlag(..), TyThing(..), mkTyVarBinders, pprUserForAll )
import IfaceSyn ( ShowSub(..), ShowHowMuch(..), AltPpr(..)
  , showToHeader, pprIfaceDecl )
import CoAxiom ( coAxiomTyCon )
import HscTypes( tyThingParent_maybe )
import MkIface ( tyThingToIfaceDecl )
import Type ( tidyOpenType )
import FamInstEnv( FamInst(..), FamFlavor(..) )
import Type( Type, pprTypeApp, pprSigmaType )
import Name
import VarEnv( emptyTidyEnv )
import Outputable

-- -----------------------------------------------------------------------------
-- Pretty-printing entities that we get from the GHC API

{- Note [Pretty printing via IfaceSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our general plan for prett-printing
  - Types
  - TyCons
  - Classes
  - Pattern synonyms
  ...etc...

is to convert them to IfaceSyn, and pretty-print that. For example
  - pprType converts a Type to an IfaceType, and pretty prints that.
  - pprTyThing converts the TyThing to an IfaceDecl,
    and pretty prints that.

So IfaceSyn play a dual role:
  - it's the internal version of an interface files
  - it's used for pretty-printing

Why do this?

* A significant reason is that we need to be able
  to pretty-print IfaceSyn (to display Foo.hi), and it was a
  pain to duplicate masses of pretty-printing goop, esp for
  Type and IfaceType.

* When pretty-printing (a type, say), we want to tidy (with
  tidyType) to avoids having (forall a a. blah) where the two
  a's have different uniques.

  Alas, for type constructors, TyCon, tidying does not work well,
  because a TyCon includes DataCons which include Types, which mention
  TyCons. And tidying can't tidy a mutually recursive data structure
  graph, only trees.

* Interface files contains fast-strings, not uniques, so the very same
  tidying must take place when we convert to IfaceDecl. E.g.
  MkIface.tyThingToIfaceDecl which converts a TyThing (i.e. TyCon,
  Class etc) to an IfaceDecl.

  Bottom line: IfaceDecls are already 'tidy', so it's straightforward
  to print them.

* An alternative I once explored was to ensure that TyCons get type
  variables with distinct print-names. That's ok for type variables
  but less easy for kind variables. Processing data type declarations
  is already so complicated that I don't think it's sensible to add
  the extra requirement that it generates only "pretty" types and
  kinds.

Consequences:

- IfaceSyn (and IfaceType) must contain enough information to
  print nicely.  Hence, for example, the IfaceAppArgs type, which
  allows us to suppress invisible kind arguments in types
  (see Note [Suppressing invisible arguments] in IfaceType)

- In a few places we have info that is used only for pretty-printing,
  and is totally ignored when turning IfaceSyn back into TyCons
  etc (in TcIface). For example, IfaceClosedSynFamilyTyCon
  stores a [IfaceAxBranch] that is used only for pretty-printing.

- See Note [Free tyvars in IfaceType] in IfaceType

See #7730, #8776 for details   -}

--------------------
-- | Pretty-prints a 'FamInst' (type/data family instance) with its defining location.
pprFamInst :: FamInst -> SDoc
--  * For data instances we go via pprTyThing of the representational TyCon,
--    because there is already much cleverness associated with printing
--    data type declarations that I don't want to duplicate
--  * For type instances we print directly here; there is no TyCon
--    to give to pprTyThing
--
-- FamInstEnv.pprFamInst does a more quick-and-dirty job for internal purposes

pprFamInst (FamInst { fi_flavor = DataFamilyInst rep_tc })
  = pprTyThingInContextLoc (ATyCon rep_tc)

pprFamInst (FamInst { fi_flavor = SynFamilyInst, fi_axiom = axiom
                    , fi_tvs = tvs, fi_tys = lhs_tys, fi_rhs = rhs })
  = showWithLoc (pprDefinedAt (getName axiom)) $
    hang (text "type instance"
            <+> pprUserForAll (mkTyVarBinders Specified tvs)
                -- See Note [Printing foralls in type family instances]
                -- in IfaceType
            <+> pprTypeApp (coAxiomTyCon axiom) lhs_tys)
       2 (equals <+> ppr rhs)

----------------------------
-- | Pretty-prints a 'TyThing' with its defining location.
pprTyThingLoc :: TyThing -> SDoc
pprTyThingLoc tyThing
  = showWithLoc (pprDefinedAt (getName tyThing))
                (pprTyThing showToHeader tyThing)

-- | Pretty-prints the 'TyThing' header. For functions and data constructors
-- the function is equivalent to 'pprTyThing' but for type constructors
-- and classes it prints only the header part of the declaration.
pprTyThingHdr :: TyThing -> SDoc
pprTyThingHdr = pprTyThing showToHeader

-- | Pretty-prints a 'TyThing' in context: that is, if the entity
-- is a data constructor, record selector, or class method, then
-- the entity's parent declaration is pretty-printed with irrelevant
-- parts omitted.
pprTyThingInContext :: ShowSub -> TyThing -> SDoc
pprTyThingInContext show_sub thing
  = go [] thing
  where
    go ss thing
      = case tyThingParent_maybe thing of
          Just parent ->
            go (getOccName thing : ss) parent
          Nothing ->
            pprTyThing
              (show_sub { ss_how_much = ShowSome ss (AltPpr Nothing) })
              thing

-- | Like 'pprTyThingInContext', but adds the defining location.
pprTyThingInContextLoc :: TyThing -> SDoc
pprTyThingInContextLoc tyThing
  = showWithLoc (pprDefinedAt (getName tyThing))
                (pprTyThingInContext showToHeader tyThing)

-- | Pretty-prints a 'TyThing'.
pprTyThing :: ShowSub -> TyThing -> SDoc
-- We pretty-print 'TyThing' via 'IfaceDecl'
-- See Note [Pretty-printing TyThings]
pprTyThing ss ty_thing
  = pprIfaceDecl ss' (tyThingToIfaceDecl ty_thing)
  where
    ss' = case ss_how_much ss of
      ShowHeader (AltPpr Nothing)  -> ss { ss_how_much = ShowHeader ppr' }
      ShowSome xs (AltPpr Nothing) -> ss { ss_how_much = ShowSome xs ppr' }
      _                   -> ss

    ppr' = AltPpr $ ppr_bndr $ getName ty_thing

    ppr_bndr :: Name -> Maybe (OccName -> SDoc)
    ppr_bndr name
      | isBuiltInSyntax name
         = Nothing
      | otherwise
         = case nameModule_maybe name of
             Just mod -> Just $ \occ -> getPprStyle $ \sty ->
               pprModulePrefix sty mod occ <> ppr occ
             Nothing  -> WARN( True, ppr name ) Nothing
             -- Nothing is unexpected here; TyThings have External names

pprTypeForUser :: Type -> SDoc
-- The type is tidied
pprTypeForUser ty
  = pprSigmaType tidy_ty
  where
    (_, tidy_ty)     = tidyOpenType emptyTidyEnv ty
     -- Often the types/kinds we print in ghci are fully generalised
     -- and have no free variables, but it turns out that we sometimes
     -- print un-generalised kinds (eg when doing :k T), so it's
     -- better to use tidyOpenType here

showWithLoc :: SDoc -> SDoc -> SDoc
showWithLoc loc doc
    = hang doc 2 (char '\t' <> comment <+> loc)
                -- The tab tries to make them line up a bit
  where
    comment = text "--"
