-----------------------------------------------------------------------------
--
-- Pretty-printing TyThings
--
-- (c) The GHC Team 2005
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

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

--------------------
-- | Pretty-prints a 'FamInst' (type/data family instance) with its defining location.
pprFamInst :: FamInst -> SDoc
--  * For data instances we go via pprTyThing of the represntational TyCon,
--    becuase there is already much cleverness associated with printing
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
-- NOTE: We pretty-print 'TyThing' via 'IfaceDecl' so that we can reuse the
-- 'TyCon' tidying happening in 'tyThingToIfaceDecl'. See #8776 for details.
ppr_ty_thing hdr_only path ty_thing
  = pprIfaceDecl (ShowSub { ss_how_much = how_much, ss_ppr_bndr = ppr_bndr }) if_decl
  where
    how_much | hdr_only  = ShowHeader
             | otherwise = ShowSome path
    if_decl = tyThingToIfaceDecl ty_thing
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
--	forall a. C a => forall b. Ord b => stuff
-- Then we want to display
--	(C a, Ord b) => stuff
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
