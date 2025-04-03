{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-- | Typechecking @default@ declarations
module GHC.Tc.Gen.Default ( tcDefaults ) where

import GHC.Prelude
import GHC.Hs

import GHC.Builtin.Names
import GHC.Core.Class
import GHC.Core.Predicate ( Pred (..), classifyPredType )

import GHC.Data.Maybe ( firstJusts )

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
import GHC.Types.DefaultEnv ( DefaultEnv, ClassDefaults (..), defaultEnv )
import GHC.Types.SrcLoc

import GHC.Unit.Types (Module, ghcInternalUnit, moduleUnit)

import GHC.Utils.Misc (fstOf3, sndOf3)
import GHC.Utils.Outputable

import qualified GHC.LanguageExtensions as LangExt

import Data.Function (on)
import Data.List.NonEmpty ( NonEmpty (..), groupBy )
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
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

* The module's `tcg_default` is consulted when defaulting unsolved constraints,
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
  = do  { tcg_env <- getGblEnv
        ; let
            here = tcg_mod tcg_env
            is_internal_unit = moduleUnit here == ghcInternalUnit
        ; case (is_internal_unit, decls) of
            -- Some internal GHC modules contain @default ()@ to declare that no defaults can take place
            -- in the module.
            -- We shortcut the treatment of such a default declaration with no class nor types: we won't
            -- try to point 'cd_class' to 'Num' since it may not even exist yet.
          { (True, [L _ (DefaultDecl _ Nothing [])])
              -> return $ defaultEnv []
            -- Otherwise we take apart the declaration into the class constructor and its default types.
          ; _ ->
    do  { h2010_dflt_clss <- getH2010DefaultClasses
        ; decls' <- mapMaybeM (declarationParts h2010_dflt_clss) decls
        ; let
            -- Find duplicate default declarations
            decl_tag (mb_cls, _, _) =
              case mb_cls of
                Nothing -> Nothing
                Just cls -> if cls `elem` h2010_dflt_clss
                            then Nothing
                            else Just cls
            decl_groups = groupBy ((==) `on` decl_tag) decls'
        ; decls_without_dups <- mapM (reportDuplicates here h2010_dflt_clss) decl_groups
        ; return $ defaultEnv (concat decls_without_dups)
        } } }
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
    getH2010DefaultClasses
      = do { num_cls <- tcLookupClass numClassName
           ; ovl_str   <- xoptM LangExt.OverloadedStrings
           ; ext_deflt <- xoptM LangExt.ExtendedDefaultRules
           ; deflt_str <- if ovl_str
                          then mapM tcLookupClass [isStringClassName]
                          else return []
           ; deflt_interactive <- if ext_deflt
                                  then mapM tcLookupClass interactiveClassNames
                                  else return []
           ; let extra_clss = deflt_str ++ deflt_interactive
           ; return $ num_cls :| extra_clss
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

    reportDuplicates :: Module -> NonEmpty Class -> NonEmpty (Maybe Class, LDefaultDecl GhcRn, [Type]) -> TcM [ClassDefaults]
    reportDuplicates here h2010_dflt_clss ((mb_cls, _, tys) :| [])
      = pure [ ClassDefaults{cd_class = c, cd_types = tys, cd_module = Just here, cd_warn = Nothing }
             | c <- case mb_cls of
                      Nothing  -> NE.toList h2010_dflt_clss
                      Just cls -> [cls]
             ]
    -- Report an error on multiple default declarations for the same class in the same module.
    -- See Note [Disambiguation of multiple default declarations] in GHC.Tc.Module
    reportDuplicates _ (num_cls :| _) decls@((_, L locn _, _) :| _)
      = setSrcSpan (locA locn) (addErrTc $ dupDefaultDeclErr cls (sndOf3 <$> decls))
        >> pure []
      where
        cls = fromMaybe num_cls $ firstJusts (fmap fstOf3 decls)

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

dupDefaultDeclErr :: Class -> NonEmpty (LDefaultDecl GhcRn) -> TcRnMessage
dupDefaultDeclErr cls (L _ DefaultDecl {} :| dup_things)
  = TcRnMultipleDefaultDeclarations cls dup_things

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