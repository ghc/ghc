{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | Typechecking @default@ declarations
module GHC.Tc.Gen.Default ( tcDefaultDecls, extendDefaultEnvWithLocalDefaults ) where

import GHC.Prelude
import GHC.Hs

import GHC.Builtin.Names
import GHC.Core.Class
import GHC.Core.Predicate ( Pred (..), classifyPredType )

import GHC.Data.Maybe ( firstJusts, maybeToList )

import GHC.Tc.Errors.Types
import GHC.Tc.Gen.HsType
import GHC.Tc.Solver.Monad  ( runTcS )
import GHC.Tc.Solver.Solve  ( solveWanteds )
import GHC.Tc.Types.Constraint ( isEmptyWC, andWC, mkSimpleWC )
import GHC.Tc.Types.Origin  ( CtOrigin(DefaultOrigin) )
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType ( newWanted )
import GHC.Tc.Utils.TcType

import GHC.Types.Basic ( TypeOrKind(..) )
import GHC.Types.DefaultEnv ( DefaultEnv, ClassDefaults (..), lookupDefaultEnv, insertDefaultEnv, DefaultProvenance (..) )
import GHC.Types.SrcLoc

import GHC.Unit.Types (ghcInternalUnit, moduleUnit)

import GHC.Utils.Outputable

import qualified GHC.LanguageExtensions as LangExt

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Traversable ( for )

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

* The `DefaultEnv` of all defaults in scope in a module is kept in the `tcg_default`
  field of `TcGblEnv`.

* This field is populated by `GHC.Tc.Gen.Default.tcDefaultDecls` which typechecks
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

  See Note [Builtin class defaults] in GHC.Tc.Utils.Env

* Beside the defaults, the `ExtendedDefaultRules` and `OverloadedStrings`
  extensions also affect the traditional `default` declarations that don't name
  the class. They have no effect on declarations with explicit class name.
  For details of their operation see the corresponding sections of GHC User's Guide:
  - https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#extension-ExtendedDefaultRules
  - https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_strings.html#extension-OverloadedStrings

* The module's `tcg_default` is consulted when defaulting unsolved constraints,
  in GHC.Tc.Solver.applyDefaultingRules.
  See Note [How type-class constraints are defaulted] in GHC.Tc.Solver

* Class defaults are imported automatically, like class instances. They are
  tracked separately from `ImportAvails`, and returned separately from them by
  `GHC.Rename.Names.rnImports`.

* Class defaults are exported explicitly.
  For example,
        module M( ..., default C, ... )
  exports the defaults for class C.

  A module's exported defaults are computed by exports_from_avail,
  tracked in tcg_default_exports, which are then transferred to mg_defaults,
  md_defaults, and mi_defaults_.

  Only defaults explicitly exported are actually exported.
  (i.e. No defaults are exported in a module header like:
          module M where ...)

  See Note [Default exports] in GHC.Tc.Gen.Export

* Since the class defaults merely help the solver infer the correct types, they
  leave no trace in Haskell Core.
-}

-- | Typecheck a collection of default declarations. These can be either:
--
--  - Haskell 98 default declarations, of the form @default (Float, Double)@
--  - Named default declarations, of the form @default Cls(Int, Char)@.
--    See Note [Named default declarations]
tcDefaultDecls :: [LDefaultDecl GhcRn] -> TcM [LocatedA ClassDefaults]
tcDefaultDecls decls =
  do
    tcg_env <- getGblEnv
    let here = tcg_mod tcg_env
        is_internal_unit = moduleUnit here == ghcInternalUnit
    case (is_internal_unit, decls) of
      -- No default declarations
      (_, []) -> return []
      -- As per Remark [default () in ghc-internal] in Note [Builtin class defaults],
      -- some modules in ghc-internal include an empty `default ()` declaration, in order
      -- to disable built-in defaults. This is no longer necessary (see `GHC.Tc.Utils.Env.tcGetDefaultTys`),
      -- but we must still make sure not to error if we fail to look up e.g. the 'Num'
      -- typeclass when typechecking such a default declaration. To do this, we wrap
      -- calls of 'tcLookupClass' in 'tryTc'.
      (True, [L _ (DefaultDecl _ Nothing [])]) -> do
        h2010_dflt_clss <- foldMapM (fmap maybeToList . fmap fst . tryTc . tcLookupClass) =<< getH2010DefaultNames
        case NE.nonEmpty h2010_dflt_clss of
          Nothing -> return []
          Just h2010_dflt_clss' -> toClassDefaults h2010_dflt_clss' decls
      -- Otherwise we take apart the declaration into the class constructor and its default types.
      _ -> do
        h2010_dflt_clss <- getH2010DefaultClasses
        toClassDefaults h2010_dflt_clss decls
  where
    getH2010DefaultClasses :: TcM (NonEmpty Class)
    -- All the classes subject to defaulting with a Haskell 2010 default
    -- declaration, of the form:
    --
    --   default (Int, Bool, Float)
    --
    -- Specifically:
    --    No extensions:       Num
    --    OverloadedStrings:   add IsString
    --    ExtendedDefaults:    add Show, Eq, Ord, Foldable, Traversable
    getH2010DefaultClasses = mapM tcLookupClass =<< getH2010DefaultNames
    getH2010DefaultNames
      = do { ovl_str   <- xoptM LangExt.OverloadedStrings
           ; ext_deflt <- xoptM LangExt.ExtendedDefaultRules
           ; let deflt_str = if ovl_str
                              then [isStringClassName]
                              else []
           ; let deflt_interactive = if ext_deflt
                                  then interactiveClassNames
                                  else []
           ; let extra_clss_names = deflt_str ++ deflt_interactive
           ; return $ numClassName :| extra_clss_names
           }
    declarationParts :: NonEmpty Class -> LDefaultDecl GhcRn -> TcM (Maybe (Maybe Class, LDefaultDecl GhcRn, [Type]))
    declarationParts h2010_dflt_clss decl@(L locn (DefaultDecl _ mb_cls_name dflt_hs_tys))
      = setSrcSpan (locA locn) $
          case mb_cls_name of
            -- Haskell 98 default declaration
            Nothing ->
              do { tau_tys <- addErrCtxt (DefaultDeclErrCtxt { ddec_in_type_list = True })
                            $ mapMaybeM (check_instance_any h2010_dflt_clss) dflt_hs_tys
                 ; return $ Just (Nothing, decl, tau_tys) }
            -- Named default declaration
            Just cls_name ->
              do { named_deflt <- xoptM LangExt.NamedDefaults
                 ; checkErr named_deflt (TcRnIllegalNamedDefault decl)
                 ; mb_cls <- addErrCtxt (DefaultDeclErrCtxt { ddec_in_type_list = False })
                           $ tcDefaultDeclClass cls_name
                 ; for mb_cls $ \ cls ->
              do { tau_tys <- addErrCtxt (DefaultDeclErrCtxt { ddec_in_type_list = True })
                            $ mapMaybeM (check_instance_any (NE.singleton cls)) dflt_hs_tys
                 ; return (Just cls, decl, tau_tys)
                 } }

    toClassDefaults :: NonEmpty Class -> [LDefaultDecl GhcRn] -> TcM [LocatedA ClassDefaults]
    toClassDefaults h2010_dflt_clss dfs = do
        dfs <- mapMaybeM (declarationParts h2010_dflt_clss) dfs
        return $ concatMap (go False) dfs
      where
        go h98 = \case
          (Nothing, rn_decl, tys) -> concatMap (go True) [(Just cls, rn_decl, tys) | cls <- NE.toList h2010_dflt_clss]
          (Just cls, (L locn _), tys) -> [(L locn $ ClassDefaults cls tys (DP_Local (locA locn) h98) Nothing)]

-- | Extend the default environment with the local default declarations
-- and do the action in the extended environment.
extendDefaultEnvWithLocalDefaults :: [LocatedA ClassDefaults] -> TcM a -> TcM a
extendDefaultEnvWithLocalDefaults decls action = do
  tcg_env <- getGblEnv
  let default_env = tcg_default tcg_env
  new_default_env <- insertDefaultDecls default_env decls
  updGblEnv (\gbl -> gbl { tcg_default = new_default_env } ) $ action

-- | Insert local default declarations into the default environment.
--
-- See 'insertDefaultDecl'.
insertDefaultDecls :: DefaultEnv -> [LocatedA ClassDefaults] -> TcM DefaultEnv
insertDefaultDecls = foldrM insertDefaultDecl
-- | Insert a local default declaration into the default environment.
--
-- If the class already has a local default declaration in the DefaultEnv,
-- report an error and return the original DefaultEnv. Otherwise, override
-- any existing default declarations (e.g. imported default declarations).
--
-- See Note [Disambiguation of multiple default declarations] in GHC.Tc.Module
insertDefaultDecl :: LocatedA ClassDefaults -> DefaultEnv -> TcM DefaultEnv
insertDefaultDecl (L decl_loc new_cls_defaults ) default_env =
  case lookupDefaultEnv default_env (className cls) of
    Just cls_defaults
      | DP_Local {} <- cd_provenance cls_defaults
      -> do { setSrcSpan (locA decl_loc) (addErrTc $ TcRnMultipleDefaultDeclarations cls cls_defaults)
            ; return default_env }
    _ -> return $ insertDefaultEnv new_cls_defaults default_env
      -- NB: this overrides imported and built-in default declarations
      -- for this class, if there were any.
  where
    cls = cd_class new_cls_defaults


-- | Check that the type is an instance of at least one of the default classes.
--
-- See Note [Instance check for default declarations]
check_instance_any :: NonEmpty Class
                        -- ^ classes, all assumed to be unary
                   -> LHsType GhcRn
                        -- ^ default type
                   -> TcM (Maybe Type)
check_instance_any deflt_clss ty
  = do  { oks <- mapM (\ cls -> simplifyDefault cls ty) deflt_clss
        ; case firstJusts oks of
            Nothing ->
             do { addErrTc $ TcRnBadDefaultType ty deflt_clss
                ; return Nothing }
            Just ty ->
             return $ Just ty
        }

-- | Given a class @C@ and a type @ty@, is @C ty@ soluble?
--
-- Used to check that a type is an instance of a class in a default
-- declaration.
--
-- See Note [Instance check for default declarations] in GHC.Tc.Solver.Default.
simplifyDefault
  :: Class -- ^ class, assumed to be unary,i.e. it takes some invisible arguments
           -- and then a single (final) visible argument
  -> LHsType GhcRn -- ^ default type
  -> TcM (Maybe Type)
simplifyDefault cls dflt_ty@(L l _)
  = do { let app_ty :: LHsType GhcRn
             app_ty = L l $ HsAppTy noExtField (nlHsTyVar NotPromoted (className cls)) dflt_ty
       ; (inst_pred, wtds) <- captureConstraints $ tcCheckLHsType app_ty constraintKind
       ; wtd_inst <- newWanted DefaultOrigin (Just TypeLevel) inst_pred
       ; let all_wanteds = wtds `andWC` mkSimpleWC [wtd_inst]
       ; (unsolved, _) <- runTcS $ solveWanteds all_wanteds
       ; traceTc "simplifyDefault" $
           vcat [ text "cls:" <+> ppr cls
                , text "dflt_ty:" <+> ppr dflt_ty
                , text "inst_pred:" <+> ppr inst_pred
                , text "all_wanteds " <+> ppr all_wanteds
                , text "unsolved:" <+> ppr unsolved ]
       ; let is_instance = isEmptyWC unsolved
       ; return $
           if | is_instance
              , ClassPred _ tys <- classifyPredType inst_pred
              -- inst_pred looks like (C @k1 .. @kn t);
              -- we want the final (visible) argument `t`
              , Just tys_ne <- NE.nonEmpty tys
              -> Just $ NE.last tys_ne
              | otherwise
              -> Nothing
       }

{- Note [Instance check for default declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see a named default declaration, such as:

  default C(ty_1, ..., ty_n)

we must check that each of the types 'ty1', ..., 'ty_n' is an instance of
the class 'C'. For each individual type 'ty', the strategy is thus:

  - Create a new Wanted constraint 'C ty', and run the solver on it.
    The default declaration 'default C(ty)' is valid iff the solver succeeds
    in solving this constraint (with no residual unsolved Wanteds).

This is implemented in GHC.Tc.Gen.Default.check_instance, and tested in T25882.

The only slightly subtle point is that we want to allow classes such as

  Typeable :: forall k. k -> Constraint

which take invisible arguments and a (single) visible argument. The function
GHC.Tc.Gen.HsType.tcDefaultDeclClass checks that the class 'C' takes a single
visible parameter.

Note that Haskell98 default declarations, of the form

  default (ty_1, ..., ty_n)

work similarly, except that instead of checking for a single class, we check
whether each type is an instance of:

  - only the Num class, by default
  - ... or the IsString class, with -XOverloadedStrings
  - ... or any of the Show, Eq, Ord, Foldable, and Traversable classes,
        with -XExtendedDefaultRules
-}
