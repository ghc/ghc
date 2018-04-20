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

import Type    ( TyThing(..) )
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

{-  Note [Pretty-printing TyThings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pretty-print a TyThing by converting it to an IfaceDecl,
and pretty-printing that (see ppr_ty_thing below).
Here is why:

* When pretty-printing (a type, say), the idiomatic solution is not to
  "rename type variables on the fly", but rather to "tidy" the type
  (which gives each variable a distinct print-name), and then
  pretty-print it (without renaming). Separate the two
  concerns. Functions like tidyType do this.

* Alas, for type constructors, TyCon, tidying does not work well,
  because a TyCon includes DataCons which include Types, which mention
  TyCons. And tidying can't tidy a mutually recursive data structure
  graph, only trees.

* One alternative would be to ensure that TyCons get type variables
  with distinct print-names. That's ok for type variables but less
  easy for kind variables. Processing data type declarations is
  already so complicated that I don't think it's sensible to add the
  extra requirement that it generates only "pretty" types and kinds.

*  One place the non-pretty names can show up is in GHCi. But another
   is in interface files. Look at MkIface.tyThingToIfaceDecl which
   converts a TyThing (i.e. TyCon, Class etc) to an IfaceDecl. And it
   already does tidying as part of that conversion!  Why? Because
   interface files contains fast-strings, not uniques, so the names
   must at least be distinct.

So if we convert to IfaceDecl, we get a nice tidy IfaceDecl, and can
print that.  Of course, that means that pretty-printing IfaceDecls
must be careful to display nice user-friendly results, but that's ok.

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
                    , fi_tys = lhs_tys, fi_rhs = rhs })
  = showWithLoc (pprDefinedAt (getName axiom)) $
    hang (text "type instance" <+> pprTypeApp (coAxiomTyCon axiom) lhs_tys)
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
