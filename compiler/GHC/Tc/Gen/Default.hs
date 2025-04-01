{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}
{-# LANGUAGE TypeFamilies #-}

-- | Typechecking @default@ declarations
module GHC.Tc.Gen.Default ( tcDefaults ) where

import GHC.Prelude

import GHC.Hs
import GHC.Core.Class
import GHC.Core.Type( typeKind )

import GHC.Types.Var( tyVarKind )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.HsType
import GHC.Tc.Zonk.Type
import GHC.Tc.Solver
import GHC.Tc.Validity
import GHC.Tc.Utils.TcType
import GHC.Builtin.Names
import GHC.Types.DefaultEnv ( DefaultEnv, ClassDefaults (..), defaultEnv )
import GHC.Types.SrcLoc
import GHC.Unit.Types (Module, ghcInternalUnit, moduleUnit)
import GHC.Utils.Misc (fstOf3, sndOf3)
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad (void)
import Data.Function (on)
import Data.List.NonEmpty ( NonEmpty (..), groupBy )
import qualified Data.List.NonEmpty as NE

{- Note [Named default declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With the `NamedDefaults` language extension, a `default` declaration can specify type-class
defaulting behaviour for specific classes. For example

      class C a where
        ...
      default C( Int, Bool )  -- The default types for class C

The `default` declaration tells GHC to default unresolved constraints (C a) to (C Int) or
(C Bool), in that order. Of course, if you don't specify a class, thus

    default (Int, Bool)

the default declaration behaves as before, affecting primarily the `Num` class.

Moreover, a module export list can specify a list of classes whose defaults should be
exported.  For example

    module M( C, default C )

would export the above `default` declaration for `C`.

See details at
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0409-exportable-named-default.rst

The moving parts are as follows:

* Language.Haskell.Syntax.Decls.DefaultDecl: A `DefaultDecl` optionally carries
  the specified class.

* Parsing and renaming are entirely straightforward.

* The typechecker maintains a `DefaultEnv` (see GHC.Types.DefaultEnv)
  which maps a class to a `ClassDefaults`.  The `ClassDefaults` for a class
  specifies the defaults for that class, in the current module.

* The `DefaultEnv` of all defaults in scope in a module is kept in the `tcg_default_env`
  field of `TcGblEnv`.

* This field is populated by `GHC.Tc.Gen.Default.tcDefaults` which typechecks
  any local or imported `default` declarations.

* Only a single default declaration can be in effect in any single module for
  any particular class. We issue an error if a single module contains two
  default declarations for the same class, a possible warning if it imports
  them.

  See Note [Disambiguation of multiple default declarations] in GHC.Tc.Module

* There is a _default_ `DefaultEnv` even in absence of any user-declared
  `default` declarations. It is determined by the presence of the
  `ExtendedDefaultRules` and `OverloadedStrings` extensions. If neither of these
  extensions nor user-declared declarations are present, the `DefaultEnv` will
  in effect be `default Num (Integer, Double)` as specified by Haskell Language
  Report.

  See Note [Default class defaults] in GHC.Tc.Utils.Env

* Beside the defaults, the `ExtendedDefaultRules` and `OverloadedStrings`
  extensions also affect the traditional `default` declarations that don't name
  the class. They have no effect on declarations with explicit class name.
  For details of their operation see the corresponding sections of GHC User's Guide:
  - https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#extension-ExtendedDefaultRules
  - https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_strings.html#extension-OverloadedStrings

* The module's `tcg_default_env` is consulted when defaulting unsolved constraints,
  in GHC.Tc.Solver.applyDefaultingRules.
  See Note [How type-class constraints are defaulted] in GHC.Tc.Solver

* Class defaults are imported automatically, like class instances. They are
  tracked separately from `ImportAvails`, and returned separately from them by
  `GHC.Rename.Names.rnImports`.

* Class defaults are exported explicitly, as the example above shows. A module's
  exported defaults are tracked in `tcg_default_exports`, which are then
  transferred to `mg_defaults`, `md_defaults`, and `mi_defaults_`.
  See Note [Default exports] in GHC.Tc.Gen.Export

* Since the class defaults merely help the solver infer the correct types, they
  leave no trace in Haskell Core.
-}

-- See Note [Named default declarations]
tcDefaults :: [LDefaultDecl GhcRn]
           -> TcM DefaultEnv  -- Defaulting types to heave
                              -- into Tc monad for later use
                              -- in Disambig.

tcDefaults []
  = getDeclaredDefaultTys       -- No default declaration, so get the
                                -- default types from the envt;
                                -- i.e. use the current ones
                                -- (the caller will put them back there)
        -- It's important not to return defaultDefaultTys here (which
        -- we used to do) because in a TH program, tcDefaults [] is called
        -- repeatedly, once for each group of declarations between top-level
        -- splices.  We don't want to carefully set the default types in
        -- one group, only for the next group to ignore them and install
        -- defaultDefaultTys

tcDefaults decls
  = do  { ovl_str   <- xoptM LangExt.OverloadedStrings
        ; ext_deflt <- xoptM LangExt.ExtendedDefaultRules
        ; deflt_str <- if ovl_str
                       then mapM tcLookupClass [isStringClassName]
                       else return []
        ; deflt_interactive <- if ext_deflt
                               then mapM tcLookupClass interactiveClassNames
                               else return []
        ; tcg_env <- getGblEnv
        ; let extra_clss = deflt_str ++ deflt_interactive
              here = tcg_mod tcg_env
              is_internal_unit = moduleUnit here == ghcInternalUnit
        ; decls' <- case (is_internal_unit, decls) of
            -- Some internal GHC modules contain @default ()@ to declare that no defaults can take place
            -- in the module.
            -- We shortcut the treatment of such a default declaration with no class nor types: we won't
            -- try to point 'cd_class' to 'Num' since it may not even exist yet.
            (True, [L _ (DefaultDecl _ Nothing [])]) -> pure []
            -- Otherwise we take apart the declaration into the class constructor and its default types.
            _ ->  mapM (declarationParts extra_clss) decls
        ; defaultEnv . concat <$> mapM (reportDuplicates here extra_clss) (groupBy ((==) `on` sndOf3) decls') }
  where
    declarationParts :: [Class] -> LDefaultDecl GhcRn -> TcM (LDefaultDecl GhcRn, Class, [Type])
    reportDuplicates :: Module -> [Class] -> NonEmpty (LDefaultDecl GhcRn, Class, [Type]) -> TcM [ClassDefaults]
    declarationParts extra_clss decl@(L locn (DefaultDecl _ cls_tyMaybe mono_tys))
      = addErrCtxt DefaultDeclErrCtxt $
        setSrcSpan (locA locn)        $
        do { tau_tys <- mapAndReportM tc_default_ty mono_tys
           ; def_clsCon <- case cls_tyMaybe of
               Nothing ->
                 do { numTyCls <- tcLookupClass numClassName
                    ; let classTyConAndArgKinds cls = (cls, [], tyVarKind <$> classTyVars cls)
                          tyConsAndArgKinds = (numTyCls, [], [liftedTypeKind]) :| map classTyConAndArgKinds extra_clss
                    ; void $ mapAndReportM (check_instance_any tyConsAndArgKinds) tau_tys
                    ; return numTyCls }
               Just cls_name ->
                 do { named_deflt <- xoptM LangExt.NamedDefaults
                    ; checkErr named_deflt (TcRnIllegalNamedDefault decl)
                    ; let cls_ty = noLocA (HsSig { sig_ext   = noExtField
                                                 , sig_bndrs = HsOuterImplicit{hso_ximplicit = []}
                                                 , sig_body  = noLocA $ HsTyVar noAnn NotPromoted cls_name})
                    ; (_cls_tvs, cls, cls_tys, cls_arg_kinds) <- tcHsDefault cls_ty
                    ; case cls_arg_kinds
                      of [k] -> void $ mapAndReportM (check_instance_any (NE.singleton (cls, cls_tys, [k]))) tau_tys
                         _ -> addErrTc (TcRnNonUnaryTypeclassConstraint DefaultDeclCtxt cls_ty)
                    ; return cls }
           ; return (decl, def_clsCon, tau_tys) }
    reportDuplicates here extra_clss ((_, clsCon, tys) :| [])
      = pure [ ClassDefaults{cd_class = c, cd_types = tys, cd_module = Just here, cd_warn = Nothing}
             | c <- clsCon : extra_clss ]
    -- Report an error on multiple default declarations for the same class in the same module.
    -- See Note [Disambiguation of multiple default declarations] in GHC.Tc.Module
    reportDuplicates _ _ decls@((L locn _, cls, _) :| _)
      = setSrcSpan (locA locn) (addErrTc $ dupDefaultDeclErr cls (fstOf3 <$> decls))
        >> pure []

tc_default_ty :: LHsType GhcRn -> TcM Type
tc_default_ty hs_ty
 = do   { ty <- solveEqualities "tc_default_ty" $
                tcInferLHsType hs_ty
        ; ty <- zonkTcTypeToType ty   -- establish Type invariants
        ; checkValidType DefaultDeclCtxt ty
        ; return ty }

-- Check that the type is an instance of at least one of the default classes.
-- Beside the class type constructor, we take the already-supplied type
-- parameters and the expected kinds of the remaining parameters. We report
-- an error unless there's only one remaining parameter to fill and the given
-- type has the expected kind.
check_instance_any :: NonEmpty (Class, [Type], [Kind]) -> Type -> TcM ()
check_instance_any deflt_clss ty
 = do   { oks <- mapM (check_instance ty) deflt_clss
        ; checkTc (or oks) (TcRnBadDefaultType ty (NE.map fstOf3 deflt_clss))
        }

check_instance :: Type -> (Class, [Type], [Kind]) -> TcM Bool
-- Check that ty is an instance of cls
-- We only care about whether it worked or not; return a boolean
-- This checks that  cls :: k -> Constraint
-- with just one argument and no polymorphism; if we need to add
-- polymorphism we can make it more complicated.  For now we are
-- concerned with classes like
--    Num      :: Type -> Constraint
--    Foldable :: (Type->Type) -> Constraint
check_instance ty (cls, clsArgs, [cls_argKind])
  | cls_argKind `tcEqType` typeKind ty
  = simplifyDefault [mkTyConApp (classTyCon cls) (clsArgs ++ [ty])]
check_instance _ _
  = return False

dupDefaultDeclErr :: Class -> NonEmpty (LDefaultDecl GhcRn) -> TcRnMessage
dupDefaultDeclErr cls (L _ DefaultDecl {} :| dup_things)
  = TcRnMultipleDefaultDeclarations cls dup_things
