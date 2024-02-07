-----------------------------------------------------------------------------
--
-- Pretty-printing TyThings
--
-- (c) The GHC Team 2005
--
-----------------------------------------------------------------------------


module GHC.Types.TyThing.Ppr (
        pprTyThing,
        pprTyThingInContext,
        pprTyThingLoc,
        pprTyThingInContextLoc,
        pprTyThingHdr,
        pprFamInst
  ) where

import GHC.Prelude

import GHC.Types.TyThing ( TyThing(..), tyThingParent_maybe )
import GHC.Types.Name

import GHC.Core.Type    ( ForAllTyFlag(..), mkTyVarBinders )
import GHC.Core.Coercion.Axiom ( coAxiomTyCon )
import GHC.Core.FamInstEnv( FamInst(..), FamFlavor(..) )
import GHC.Core.TyCo.Ppr ( pprUserForAll, pprTypeApp )

import GHC.Iface.Decl   ( tyThingToIfaceDecl )
import GHC.Iface.Syntax ( ShowSub(..), ShowHowMuch(..), AltPpr(..)
                        , showToHeader, pprIfaceDecl )

import GHC.Utils.Outputable

import Data.Maybe ( isJust )

-- -----------------------------------------------------------------------------
-- Pretty-printing entities that we get from the GHC API

{- Note [Pretty printing via Iface syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our general plan for pretty-printing
  - Types
  - TyCons
  - Classes
  - Pattern synonyms
  ...etc...

is to convert them to Iface syntax, and pretty-print that. For example
  - pprType converts a Type to an IfaceType, and pretty prints that.
  - pprTyThing converts the TyThing to an IfaceDecl,
    and pretty prints that.

So Iface syntax plays a dual role:
  - it's the internal version of an interface files
  - it's used for pretty-printing

Why do this?

* A significant reason is that we need to be able
  to pretty-print Iface syntax (to display Foo.hi), and it was a
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
  GHC.Iface.Make.tyThingToIfaceDecl which converts a TyThing (i.e. TyCon,
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

- Iface syntax (and IfaceType) must contain enough information to
  print nicely.  Hence, for example, the IfaceAppArgs type, which
  allows us to suppress invisible kind arguments in types
  (see Note [Suppressing invisible arguments] in GHC.Iface.Type)

- In a few places we have info that is used only for pretty-printing,
  and is totally ignored when turning Iface syntax back into Core
  (in GHC.IfaceToCore). For example, IfaceClosedSynFamilyTyCon
  stores a [IfaceAxBranch] that is used only for pretty-printing.

- See Note [Free TyVars and CoVars in IfaceType] in GHC.Iface.Type

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
                -- in GHC.Iface.Type
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
  = case parents thing of
      -- If there are no parents print everything.
      [] -> print_it Nothing thing
      -- If `thing` has a parent, print the parent and only its child `thing`
      thing':rest -> let subs = map getOccName (thing:rest)
                         filt = (`elem` subs)
                     in print_it (Just filt) thing'
  where
    parents = go
      where
        go thing =
          case tyThingParent_maybe thing of
            Just parent -> parent : go parent
            Nothing     -> []

    print_it :: Maybe (OccName -> Bool) -> TyThing -> SDoc
    print_it mb_filt thing =
      pprTyThing (show_sub { ss_how_much = ShowSome mb_filt (AltPpr Nothing) }) thing

-- | Like 'pprTyThingInContext', but adds the defining location.
pprTyThingInContextLoc :: TyThing -> SDoc
pprTyThingInContextLoc tyThing
  = showWithLoc (pprDefinedAt (getName tyThing))
                (pprTyThingInContext showToHeader tyThing)

-- | Pretty-prints a 'TyThing'.
pprTyThing :: ShowSub -> TyThing -> SDoc
-- We pretty-print 'TyThing' via 'IfaceDecl'
-- See Note [Pretty printing via Iface syntax]
pprTyThing ss ty_thing
  = sdocOption sdocLinearTypes $ \show_linear_types ->
      pprIfaceDecl ss' (tyThingToIfaceDecl show_linear_types ty_thing)
  where
    ss' = case ss_how_much ss of
      ShowHeader (AltPpr Nothing)    -> ss { ss_how_much = ShowHeader ppr' }
      ShowSome filt (AltPpr Nothing) -> ss { ss_how_much = ShowSome filt ppr' }
      _                   -> ss

    ppr' = AltPpr $ ppr_bndr $ getName ty_thing

    ppr_bndr :: Name -> Maybe (OccName -> SDoc)
    ppr_bndr name
      | isBuiltInSyntax name || isJust (namePun_maybe name)
         = Nothing
      | otherwise
         = case nameModule_maybe name of
             Just mod -> Just $ \occ -> getPprStyle $ \sty ->
               pprModulePrefix sty mod occ <> ppr occ
             Nothing  -> warnPprTrace True "pprTyThing" (ppr name) Nothing
             -- Nothing is unexpected here; TyThings have External names

showWithLoc :: SDoc -> SDoc -> SDoc
showWithLoc loc doc
    = hang doc 2 (char '\t' <> comment <+> loc)
                -- The tab tries to make them line up a bit
  where
    comment = text "--"
