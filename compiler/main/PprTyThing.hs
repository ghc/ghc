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

import TypeRep ( TyThing(..) )
import CoAxiom ( coAxiomTyCon )
import HscTypes( tyThingParent_maybe )
import MkIface ( tyThingToIfaceDecl )
import Type ( tidyOpenType )
import IfaceSyn ( pprIfaceDecl, ShowSub(..), ShowHowMuch(..) )
import FamInstEnv( FamInst( .. ), FamFlavor(..) )
import TcType
import Name
import VarEnv( emptyTidyEnv )
import Outputable
import FastString

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
--  * For data instances we go via pprTyThing of the represntational TyCon,
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
    hang (ptext (sLit "type instance") <+> pprTypeApp (coAxiomTyCon axiom) lhs_tys)
       2 (equals <+> ppr rhs)

----------------------------
-- | Pretty-prints a 'TyThing' with its defining location.
pprTyThingLoc :: TyThing -> SDoc
pprTyThingLoc tyThing
  = showWithLoc (pprDefinedAt (getName tyThing)) (pprTyThing tyThing)

-- | Pretty-prints a 'TyThing'.
pprTyThing :: TyThing -> SDoc
pprTyThing = ppr_ty_thing False []

-- | Pretty-prints the 'TyThing' header. For functions and data constructors
-- the function is equivalent to 'pprTyThing' but for type constructors
-- and classes it prints only the header part of the declaration.
pprTyThingHdr :: TyThing -> SDoc
pprTyThingHdr = ppr_ty_thing True []

-- | Pretty-prints a 'TyThing' in context: that is, if the entity
-- is a data constructor, record selector, or class method, then
-- the entity's parent declaration is pretty-printed with irrelevant
-- parts omitted.
pprTyThingInContext :: TyThing -> SDoc
pprTyThingInContext thing
  = go [] thing
  where
    go ss thing = case tyThingParent_maybe thing of
                    Just parent -> go (getOccName thing : ss) parent
                    Nothing     -> ppr_ty_thing False ss thing

-- | Like 'pprTyThingInContext', but adds the defining location.
pprTyThingInContextLoc :: TyThing -> SDoc
pprTyThingInContextLoc tyThing
  = showWithLoc (pprDefinedAt (getName tyThing))
                (pprTyThingInContext tyThing)

------------------------
ppr_ty_thing :: Bool -> [OccName] -> TyThing -> SDoc
-- We pretty-print 'TyThing' via 'IfaceDecl'
-- See Note [Pretty-pringint TyThings]
ppr_ty_thing hdr_only path ty_thing
  = pprIfaceDecl ss (tyThingToIfaceDecl ty_thing)
  where
    ss = ShowSub { ss_how_much = how_much, ss_ppr_bndr = ppr_bndr }
    how_much | hdr_only  = ShowHeader
             | otherwise = ShowSome path
    name    = getName ty_thing
    ppr_bndr :: OccName -> SDoc
    ppr_bndr | isBuiltInSyntax name
             = ppr
             | otherwise
             = case nameModule_maybe name of
                 Just mod -> \ occ -> getPprStyle $ \sty ->
                                      pprModulePrefix sty mod occ <> ppr occ
                 Nothing  -> WARN( True, ppr name ) ppr
                 -- Nothing is unexpected here; TyThings have External names

pprTypeForUser :: Type -> SDoc
-- We do two things here.
-- a) We tidy the type, regardless
-- b) Swizzle the foralls to the top, so that without
--    -fprint-explicit-foralls we'll suppress all the foralls
-- Prime example: a class op might have type
--      forall a. C a => forall b. Ord b => stuff
-- Then we want to display
--      (C a, Ord b) => stuff
pprTypeForUser ty
  = pprSigmaType (mkSigmaTy tvs ctxt tau)
  where
    (tvs, ctxt, tau) = tcSplitSigmaTy tidy_ty
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
    comment = ptext (sLit "--")
