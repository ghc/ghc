{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Type checking of type signatures in interface files
-}

{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.IfaceToCore (
        importDecl, checkWiredInTyCon, tcHiBootIface, typecheckIface,
        typecheckWholeCoreBindings,
        tcIfaceDefaults,
        typecheckIfacesForMerging,
        typecheckIfaceForInstantiate,
        tcIfaceDecl, tcIfaceDecls, tcIfaceDeclsFromIface,
        tcIfaceInst, tcIfaceFamInst, tcIfaceRules,
        tcIfaceAnnotations, tcIfaceCompleteMatches,
        tcIfaceExpr,    -- Desired by HERMIT (#7683)
        tcIfaceGlobal,
        tcIfaceOneShot, tcTopIfaceBindings,
        tcIfaceImport,
        hydrateCgBreakInfo
 ) where


import GHC.Prelude

import GHC.ByteCode.Types

import GHC.Driver.Session
import GHC.Driver.Config.Core.Lint ( initLintConfig )

import GHC.Builtin.CoercionRules (builtInCoAxiomRules)
import GHC.Builtin.WiredIn.Types

import GHC.Iface.Syntax
import GHC.Iface.Load
import GHC.Iface.Env

import GHC.StgToCmm.Types
import GHC.Runtime.Heap.Layout

import GHC.Tc.Errors.Types
import GHC.Tc.TyCl.Build
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Env

import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.FVs
import GHC.Core.TyCo.Rep    -- needs to build types & coercions in a knot
import GHC.Core.TyCo.FVs ( mkUnivCoDepsSets )
import GHC.Core.TyCo.Subst ( substTyCoVars )
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core
import GHC.Core.RoughMap( RoughMatchTc(..) )
import GHC.Core.Utils
import GHC.Core.Unfold( calcUnfoldingGuidance )
import GHC.Core.Unfold.Make
import GHC.Core.Lint
import GHC.Core.Rules (canShareRetainedSpecRule)
import GHC.Core.Make
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.TyCon.Set
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Dependent
  ( hasExistentialExpr, hasNativeDependentExprRhs, hasNativeDependentUnfolding
  , isRetainedVanillaDefaultMethod, hasRetainedRuleHead, supportsLocalUserRule
  , supportsErasedUserRule, supportsImportedErasedRule, supportsImportedUserRule
  , supportsNativeUserUnfolding )
import GHC.Core.Predicate ( mkNomEqPred )
import GHC.Core.Opt.OccurAnal ( occurAnalyseBndrsAndExpr )
import GHC.Core.Ppr

import GHC.Hs.Extension ( GhcRn )

import GHC.Unit.External
import GHC.Unit.Module
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Unit.Home.ModInfo
import qualified GHC.Unit.Home.Graph as HUG

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Logger
import GHC.Utils.Exception ( NonTermination(..), evaluate )

import GHC.Data.Bag
import GHC.Data.List.SetOps (hasNoDups)
import GHC.Data.Maybe
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict

import GHC.Types.Annotations
import GHC.Types.SourceFile
import GHC.Types.SourceText
import GHC.Types.Basic hiding ( SuccessFlag(..) )
import GHC.Types.CompleteMatch
import GHC.Types.SrcLoc
import GHC.Types.Avail
import GHC.Types.TypeEnv
import GHC.Types.Unique.FM
import GHC.Types.Unique ( hasKey )
import GHC.Types.Unique.DSet ( mkUniqDSet, emptyUniqDSet )
import GHC.Types.Unique.Set ( nonDetEltsUniqSet )
import GHC.Types.Unique.Supply
import GHC.Types.Demand( isDeadEndSig )
import GHC.Types.Literal
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.DefaultEnv ( ClassDefaults(..), DefaultEnv, mkDefaultEnv, DefaultProvenance(..) )
import GHC.Types.Id
import GHC.Types.Id.Make
import GHC.Types.Id.Info
import GHC.Types.Tickish
import GHC.Types.TyThing
import GHC.Types.Error

import GHC.Parser.Annotation (noLocA, noAnn)

import GHC.Fingerprint

import Control.Monad
import GHC.Driver.Env.KnotVars
import GHC.Unit.Module.WholeCoreBindings
import Data.IORef
import Data.Foldable
import Data.List(nub)
import qualified Data.Set as Set
import GHC.Builtin.Modules   ( rOOT_MAIN )
import GHC.Iface.Errors.Types

import Language.Haskell.Syntax.BooleanFormula (BooleanFormula)
import Language.Haskell.Syntax.BooleanFormula qualified as BF(BooleanFormula(..))
import Language.Haskell.Syntax.Extension (NoExtField (NoExtField))
import GHC.Builtin.KnownKeys (ioTyConKey)

{-
This module takes

        IfaceDecl -> TyThing
        IfaceType -> Type
        etc

An IfaceDecl is populated with RdrNames, and these are not renamed to
Names before typechecking, because there should be no scope errors etc.

        -- For (b) consider: f = \$(...h....)
        -- where h is imported, and calls f via an hi-boot file.
        -- This is bad!  But it is not seen as a staging error, because h
        -- is indeed imported.  We don't want the type-checker to black-hole
        -- when simplifying and compiling the splice!
        --
        -- Simple solution: discard any unfolding that mentions a variable
        -- bound in this module (and hence not yet processed).
        -- The discarding happens when forkM finds a type error.


************************************************************************
*                                                                      *
                Type-checking a complete interface
*                                                                      *
************************************************************************

Suppose we discover we don't need to recompile.  Then we must type
check the old interface file.  This is a bit different to the
incremental type checking we do as we suck in interface files.  Instead
we do things similarly as when we are typechecking source decls: we
bring into scope the type envt for the interface all at once, using a
knot.  Remember, the decls aren't necessarily in dependency order --
and even if they were, the type decls might be mutually recursive.

Note [Knot-tying typecheckIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are typechecking an interface A.hi, and we come across
a Name for another entity defined in A.hi.  How do we get the
'TyCon', in this case?  There are three cases:

    1) tcHiBootIface in GHC.IfaceToCore: We're typechecking an
    hi-boot file in preparation of checking if the hs file we're
    building is compatible.  In this case, we want all of the
    internal TyCons to MATCH the ones that we just constructed
    during typechecking: the knot is thus tied through if_rec_types.

    2) rehydrate in GHC.Driver.Make: We are rehydrating a
    mutually recursive cluster of hi files, in order to ensure
    that all of the references refer to each other correctly.
    In this case, the knot is tied through the HPT passed in,
    which contains all of the interfaces we are in the process
    of typechecking.

    3) genModDetails in GHC.Driver.Main: We are typechecking an
    old interface to generate the ModDetails.  In this case,
    we do the same thing as (2) and pass in an HPT with
    the HomeModInfo being generated to tie knots.

The upshot is that the CLIENT of this function is responsible
for making sure that the knot is tied correctly.  If you don't,
then you'll get a message saying that we couldn't load the
declaration you wanted.

BTW, in one-shot mode we never call typecheckIface; instead,
loadInterface handles type-checking interface.  In that case,
knots are tied through the EPS.  No problem!
-}

-- Clients of this function be careful, see Note [Knot-tying typecheckIface]
typecheckIface :: ModIface      -- Get the decls from here
               -> IfG ModDetails
typecheckIface iface
  | let iface_mod = mi_semantic_module iface
  = initIfaceLcl iface_mod (text "typecheckIface") (mi_boot iface) $ do
        {       -- Get the right set of decls and rules.  If we are compiling without -O
                -- we discard pragmas before typechecking, so that we don't "see"
                -- information that we shouldn't.  From a versioning point of view
                -- It's not actually *wrong* to do so, but in fact GHCi is unable
                -- to handle unboxed tuples, so it must not see unfoldings.
          ignore_prags <- goptM Opt_IgnoreInterfacePragmas

                -- Typecheck the decls.  This is done lazily, so that the knot-tying
                -- within this single module works out right.  It's the callers
                -- job to make sure the knot is tied.
        ; names_w_things <- tcIfaceDeclsFromIface ignore_prags iface
        ; let type_env = mkNameEnv names_w_things

                -- Now do those rules, instances and annotations
        ; defaults  <- tcIfaceDefaults iface_mod (mi_defaults iface)
        ; insts     <- mapM tcIfaceInst (mi_insts iface)
        ; fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
        ; rules     <- tcIfaceRules ignore_prags (mi_rules iface)
        ; anns      <- tcIfaceAnnotations (mi_anns iface)

                -- Exports
        ; exports <- ifaceExportNames (mi_exports iface)

                -- Complete Sigs
        ; complete_matches <- tcIfaceCompleteMatches (mi_complete_matches iface)

                -- Finished
        ; traceIf (vcat [text "Finished typechecking interface for" <+> ppr (mi_module iface),
                         -- Careful! If we tug on the TyThing thunks too early
                         -- we'll infinite loop with hs-boot.  See #10083 for
                         -- an example where this would cause non-termination.
                         text "Type envt:" <+> ppr (map fst names_w_things)])
        ; return $ ModDetails { md_types     = type_env
                              , md_defaults  = defaults
                              , md_insts     = mkInstEnv insts
                              , md_fam_insts = fam_insts
                              , md_rules     = rules
                              , md_anns      = anns
                              , md_exports   = exports
                              , md_complete_matches = complete_matches
                              }
    }

typecheckWholeCoreBindings :: IORef TypeEnv ->  WholeCoreBindings -> IfG [CoreBind]
typecheckWholeCoreBindings type_var WholeCoreBindings {wcb_bindings, wcb_module} =
  initIfaceLcl wcb_module (text "typecheckWholeCoreBindings") NotBoot $ do
    tcTopIfaceBindings type_var wcb_bindings


{-
************************************************************************
*                                                                      *
                Typechecking for merging
*                                                                      *
************************************************************************
-}

-- | Returns true if an 'IfaceDecl' is for @data T@ (an abstract data type)
isAbstractIfaceDecl :: IfaceDecl -> Bool
isAbstractIfaceDecl IfaceData{ ifCons = IfAbstractTyCon {} } = True
isAbstractIfaceDecl IfaceClass{ ifBody = IfAbstractClass } = True
isAbstractIfaceDecl IfaceFamily{ ifFamFlav = IfaceClosedTypeFamilyTyCon IfaceAbstractClosedTyFamTyCon } = True
isAbstractIfaceDecl _ = False

ifMaybeRoles :: IfaceDecl -> Maybe [Role]
ifMaybeRoles IfaceData    { ifRoles = rs } = Just rs
ifMaybeRoles IfaceSynonym { ifRoles = rs } = Just rs
ifMaybeRoles IfaceClass   { ifRoles = rs } = Just rs
ifMaybeRoles _ = Nothing

-- | Merge two 'IfaceDecl's together, preferring a non-abstract one.  If
-- both are non-abstract we pick one arbitrarily (and check for consistency
-- later.)
mergeIfaceDecl :: IfaceDecl -> IfaceDecl -> IfaceDecl
mergeIfaceDecl d1 d2
    | isAbstractIfaceDecl d1 = d2 `withRolesFrom` d1
    | isAbstractIfaceDecl d2 = d1 `withRolesFrom` d2
    | IfaceClass{ ifBody = IfConcreteClass { ifSigs = ops1, ifMinDef = bf1 } } <- d1
    , IfaceClass{ ifBody = IfConcreteClass { ifSigs = ops2, ifMinDef = bf2 } } <- d2
    = let ops = nonDetNameEnvElts $
                  plusNameEnv_C mergeIfaceClassOp
                    (mkNameEnv [ (n, op) | op@(IfaceClassOp n _ _) <- ops1 ])
                    (mkNameEnv [ (n, op) | op@(IfaceClassOp n _ _) <- ops2 ])

      in d1 { ifBody = (ifBody d1) {
                ifSigs  = ops,
                ifMinDef = mkOr [ bf1, bf2]
                }
            } `withRolesFrom` d2
    -- It doesn't matter; we'll check for consistency later when
    -- we merge, see 'mergeSignatures'
    | otherwise              = d1 `withRolesFrom` d2
      where
        -- The reason we need to duplicate mkOr here, instead of
        -- using BooleanFormula's mkOr and just doing the loop like:
        -- `toIfaceBooleanFormula . mkOr . fromIfaceBooleanFormula`
        -- is quite subtle. Say we have the following minimal pragma:
        -- {-# MINIMAL f | g #-}. If we use fromIfaceBooleanFormula
        -- first, we will end up doing
        -- `nub [Var (mkUnboundName f), Var (mkUnboundName g)]`,
        -- which might seem fine, but Name equallity is decided by
        -- their Unique, which will be identical since mkUnboundName
        -- just stuffs the mkUnboundKey unqiue into both.
        -- So the result will be {-# MINIMAL f #-}, oopsie.
        -- Duplication it is.
        mkOr :: [IfaceBooleanFormula] -> IfaceBooleanFormula
        mkOr = maybe (IfAnd []) (mkOr' . nub . concat) . mapM fromOr
          where
          -- See Note [Simplification of BooleanFormulas]
          fromOr bf = case bf of
            (IfOr xs)  -> Just xs
            (IfAnd []) -> Nothing
            _        -> Just [bf]
          mkOr' [x] = x
          mkOr' xs = IfOr xs

-- Note [Role merging]
-- ~~~~~~~~~~~~~~~~~~~
-- First, why might it be necessary to do a non-trivial role
-- merge?  It may rescue a merge that might otherwise fail:
--
--      signature A where
--          type role T nominal representational
--          data T a b
--
--      signature A where
--          type role T representational nominal
--          data T a b
--
-- A module that defines T as representational in both arguments
-- would successfully fill both signatures, so it would be better
-- if we merged the roles of these types in some nontrivial
-- way.
--
-- However, we have to be very careful about how we go about
-- doing this, because role subtyping is *conditional* on
-- the supertype being NOT representationally injective, e.g.,
-- if we have instead:
--
--      signature A where
--          type role T nominal representational
--          data T a b = T a b
--
--      signature A where
--          type role T representational nominal
--          data T a b = T a b
--
-- Should we merge the definitions of T so that the roles are R/R (or N/N)?
-- Absolutely not: neither resulting type is a subtype of the original
-- types (see Note [Role subtyping]), because data is not representationally
-- injective.
--
-- Thus, merging only occurs when BOTH TyCons in question are
-- representationally injective.  If they're not, no merge.

withRolesFrom :: IfaceDecl -> IfaceDecl -> IfaceDecl
d1 `withRolesFrom` d2
    | Just roles1 <- ifMaybeRoles d1
    , Just roles2 <- ifMaybeRoles d2
    , not (isRepInjectiveIfaceDecl d1 || isRepInjectiveIfaceDecl d2)
    = d1 { ifRoles = mergeRoles roles1 roles2 }
    | otherwise = d1
  where
    mergeRoles roles1 roles2 = zipWithEqual max roles1 roles2

isRepInjectiveIfaceDecl :: IfaceDecl -> Bool
isRepInjectiveIfaceDecl IfaceData{ ifCons = IfDataTyCon{} } = True
isRepInjectiveIfaceDecl IfaceFamily{ ifFamFlav = IfaceDataFamilyTyCon } = True
isRepInjectiveIfaceDecl _ = False

mergeIfaceClassOp :: IfaceClassOp -> IfaceClassOp -> IfaceClassOp
mergeIfaceClassOp op1@(IfaceClassOp _ _ (Just _)) _ = op1
mergeIfaceClassOp _ op2 = op2

-- | Merge two 'OccEnv's of 'IfaceDecl's by 'OccName'.
mergeIfaceDecls :: OccEnv IfaceDecl -> OccEnv IfaceDecl -> OccEnv IfaceDecl
mergeIfaceDecls = plusOccEnv_C mergeIfaceDecl

-- | This is a very interesting function.  Like typecheckIface, we want
-- to type check an interface file into a ModDetails.  However, the use-case
-- for these ModDetails is different: we want to compare all of the
-- ModDetails to ensure they define compatible declarations, and then
-- merge them together.  So in particular, we have to take a different
-- strategy for knot-tying: we first speculatively merge the declarations
-- to get the "base" truth for what we believe the types will be
-- (this is "type computation.")  Then we read everything in relative
-- to this truth and check for compatibility.
--
-- During the merge process, we may need to nondeterministically
-- pick a particular declaration to use, if multiple signatures define
-- the declaration ('mergeIfaceDecl').  If, for all choices, there
-- are no type synonym cycles in the resulting merged graph, then
-- we can show that our choice cannot matter. Consider the
-- set of entities which the declarations depend on: by assumption
-- of acyclicity, we can assume that these have already been shown to be equal
-- to each other (otherwise merging will fail).  Then it must
-- be the case that all candidate declarations here are type-equal
-- (the choice doesn't matter) or there is an inequality (in which
-- case merging will fail.)
--
-- Unfortunately, the choice can matter if there is a cycle.  Consider the
-- following merge:
--
--      signature H where { type A = C;  type B = A; data C      }
--      signature H where { type A = (); data B;     type C = B  }
--
-- If we pick @type A = C@ as our representative, there will be
-- a cycle and merging will fail. But if we pick @type A = ()@ as
-- our representative, no cycle occurs, and we instead conclude
-- that all of the types are unit.  So it seems that we either
-- (a) need a stronger acyclicity check which considers *all*
-- possible choices from a merge, or (b) we must find a selection
-- of declarations which is acyclic, and show that this is always
-- the "best" choice we could have made (ezyang conjectures this
-- is the case but does not have a proof).  For now this is
-- not implemented.
--
-- It's worth noting that at the moment, a data constructor and a
-- type synonym are never compatible.  Consider:
--
--      signature H where { type Int=C;         type B = Int; data C = Int}
--      signature H where { export Prelude.Int; data B;       type C = B; }
--
-- This will be rejected, because the reexported Int in the second
-- signature (a proper data type) is never considered equal to a
-- type synonym.  Perhaps this should be relaxed, where a type synonym
-- in a signature is considered implemented by a data type declaration
-- which matches the reference of the type synonym.
typecheckIfacesForMerging :: Module -> [ModIface] -> (KnotVars (IORef TypeEnv)) -> IfM lcl (TypeEnv, [ModDetails])
typecheckIfacesForMerging mod ifaces tc_env_vars =
  -- cannot be boot (False)
  initIfaceLcl mod (text "typecheckIfacesForMerging") NotBoot $ do
    ignore_prags <- goptM Opt_IgnoreInterfacePragmas
    -- Build the initial environment
    -- NB: Don't include dfuns here, because we don't want to
    -- serialize them out.  See Note [rnIfaceNeverExported] in GHC.Iface.Rename
    -- NB: But coercions are OK, because they will have the right OccName.
    let mk_decl_env decls
            = mkOccEnv [ (getOccName decl, decl)
                       | decl <- decls
                       , case decl of
                            IfaceId { ifIdDetails = IfDFunId } -> False -- exclude DFuns
                            IfaceId { ifIdDetails = IfSourceDFunId{} } -> False
                            _ -> True ]
        decl_envs = map (mk_decl_env . map snd . ifaceDeclsForTc) ifaces
                        :: [OccEnv IfaceDecl]
        decl_env = foldl' mergeIfaceDecls emptyOccEnv decl_envs
                        ::  OccEnv IfaceDecl
    -- TODO: change tcIfaceDecls to accept w/o Fingerprint
    names_w_things <- tcIfaceDecls ignore_prags (map (\x -> (fingerprint0, x))
                                                  (nonDetOccEnvElts decl_env))
    let global_type_env = mkNameEnv names_w_things
    case lookupKnotVars tc_env_vars mod of
      Just tc_env_var -> writeMutVar tc_env_var global_type_env
      Nothing -> return ()

    -- OK, now typecheck each ModIface using this environment
    details <- forM ifaces $ \iface -> do
        -- See Note [Resolving never-exported Names] in GHC.IfaceToCore
        type_env <- fixM $ \type_env ->
            setImplicitEnvM type_env $ do
                decls <- tcIfaceDeclsFromIface ignore_prags iface
                return (mkNameEnv decls)
        -- But note that we use this type_env to typecheck references to DFun
        -- in 'IfaceInst'
        setImplicitEnvM type_env $ do
        defaults  <- tcIfaceDefaults (mi_semantic_module iface) (mi_defaults iface)
        insts     <- mapM tcIfaceInst (mi_insts iface)
        fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
        rules     <- tcIfaceRules ignore_prags (mi_rules iface)
        anns      <- tcIfaceAnnotations (mi_anns iface)
        exports   <- ifaceExportNames (mi_exports iface)
        complete_matches <- tcIfaceCompleteMatches (mi_complete_matches iface)
        return $ ModDetails { md_types     = type_env
                            , md_defaults  = defaults
                            , md_insts     = mkInstEnv insts
                            , md_fam_insts = fam_insts
                            , md_rules     = rules
                            , md_anns      = anns
                            , md_exports   = exports
                            , md_complete_matches = complete_matches
                            }
    return (global_type_env, details)

-- | Typecheck a signature 'ModIface' under the assumption that we have
-- instantiated it under some implementation (recorded in 'mi_semantic_module')
-- and want to check if the implementation fills the signature.
--
-- This needs to operate slightly differently than 'typecheckIface'
-- because (1) we have a 'NameShape', from the exports of the
-- implementing module, which we will use to give our top-level
-- declarations the correct 'Name's even when the implementor
-- provided them with a reexport, and (2) we have to deal with
-- DFun silliness (see Note [rnIfaceNeverExported])
typecheckIfaceForInstantiate :: NameShape -> ModIface -> IfM lcl ModDetails
typecheckIfaceForInstantiate nsubst iface
  | let iface_mod = mi_semantic_module iface
  = initIfaceLclWithSubst iface_mod
                          (text "typecheckIfaceForInstantiate")
                          (mi_boot iface) nsubst $ do
    ignore_prags <- goptM Opt_IgnoreInterfacePragmas
    -- See Note [Resolving never-exported Names] in GHC.IfaceToCore
    type_env <- fixM $ \type_env ->
        setImplicitEnvM type_env $ do
            decls     <- tcIfaceDeclsFromIface ignore_prags iface
            return (mkNameEnv decls)
    -- See Note [rnIfaceNeverExported]
    setImplicitEnvM type_env $ do
    defaults  <- tcIfaceDefaults iface_mod (mi_defaults iface)
    insts     <- mapM tcIfaceInst (mi_insts iface)
    fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
    rules     <- tcIfaceRules ignore_prags (mi_rules iface)
    anns      <- tcIfaceAnnotations (mi_anns iface)
    exports   <- ifaceExportNames (mi_exports iface)
    complete_matches <- tcIfaceCompleteMatches (mi_complete_matches iface)
    return $ ModDetails { md_types     = type_env
                        , md_defaults  = defaults
                        , md_insts     = mkInstEnv insts
                        , md_fam_insts = fam_insts
                        , md_rules     = rules
                        , md_anns      = anns
                        , md_exports   = exports
                        , md_complete_matches = complete_matches
                        }

-- Note [Resolving never-exported Names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- For the high-level overview, see
-- Note [Handling never-exported TyThings under Backpack]
--
-- As described in 'typecheckIfacesForMerging', the splendid innovation
-- of signature merging is to rewrite all Names in each of the signatures
-- we are merging together to a pre-merged structure; this is the key
-- ingredient that lets us solve some problems when merging type
-- synonyms.
--
-- However, when a 'Name' refers to a NON-exported entity, as is the
-- case with the DFun of a ClsInst, or a CoAxiom of a type family,
-- this strategy causes problems: if we pick one and rewrite all
-- references to a shared 'Name', we will accidentally fail to check
-- if the DFun or CoAxioms are compatible, as they will never be
-- checked--only exported entities are checked for compatibility,
-- and a non-exported TyThing is checked WHEN we are checking the
-- ClsInst or type family for compatibility in checkBootDeclM.
-- By virtue of the fact that everything's been pointed to the merged
-- declaration, you'll never notice there's a difference even if there
-- is one.
--
-- Fortunately, there are only a few places in the interface declarations
-- where this can occur, so we replace those calls with 'tcIfaceImplicit',
-- which will consult a local TypeEnv that records any never-exported
-- TyThings which we should wire up with.
--
-- Note that we actually knot-tie this local TypeEnv (the 'fixM'), because a
-- type family can refer to a coercion axiom, all of which are done in one go
-- when we typecheck 'mi_decls'.  An alternate strategy would be to typecheck
-- coercions first before type families, but that seemed more fragile.
--

{-
************************************************************************
*                                                                      *
                Type and class declarations
*                                                                      *
************************************************************************
-}

tcHiBootIface :: HscSource -> Module -> TcRn SelfBootInfo
-- Load the hi-boot iface for the module being compiled,
-- if it indeed exists in the transitive closure of imports
-- Return the ModDetails; Nothing if no hi-boot iface
tcHiBootIface hsc_src mod
  | HsBootFile <- hsc_src            -- Already compiling a hs-boot file
  = return NoSelfBoot
  | otherwise
  = do  { traceIf (text "loadHiBootInterface" <+> ppr mod)

        ; mode <- getGhcMode
        ; if not (isOneShot mode)
                -- In --make and interactive mode, if this module has an hs-boot file
                -- we'll have compiled it already, and it'll be in the HPT
                --
                -- We check whether the interface is a *boot* interface.
                -- It can happen (when using GHC from Visual Studio) that we
                -- compile a module in TypecheckOnly mode, with a stable,
                -- fully-populated HPT.  In that case the boot interface isn't there
                -- (it's been replaced by the mother module) so we can't check it.
                -- And that's fine, because if M's ModInfo is in the HPT, then
                -- it's been compiled once, and we don't need to check the boot iface
          then do { (_, hug) <- getEpsAndHug
                  ; liftIO $
                    HUG.lookupHugByModule mod hug >>= pure . \case
                      Just info | mi_boot (hm_iface info) == IsBoot
                                -> SelfBoot { sb_mds = hm_details info }
                      _ -> NoSelfBoot }
          else do

        -- OK, so we're in one-shot mode.
        -- Re #9245, we always check if there is an hi-boot interface
        -- to check consistency against, rather than just when we notice
        -- that an hi-boot is necessary due to a circular import.
        { hsc_env <- getTopEnv
        ; read_result <- liftIO $ findAndReadIface hsc_env need
                                  (fst (getModuleInstantiation mod)) mod
                                  IsBoot  -- Hi-boot file

        ; case read_result of {
            Succeeded (iface, _path) ->
              do { tc_iface <- initIfaceTcRn $ typecheckIface iface
                 ; return $ SelfBoot { sb_mds = tc_iface } } ;
            Failed err               ->

        -- There was no hi-boot file. But if there is circularity in
        -- the module graph, there really should have been one.
        -- Since we've read all the direct imports by now,
        -- eps_is_boot will record if any of our imports mention the
        -- current module, which either means a module loop (not
        -- a SOURCE import) or that our hi-boot file has mysteriously
        -- disappeared.
    do  { eps <- getEps
        ; case lookupInstalledModuleEnv (eps_is_boot eps) (toUnitId <$> mod) of
            -- The typical case
            Nothing -> return NoSelfBoot
            -- error cases
            Just (GWIB { gwib_isBoot = is_boot }) -> case is_boot of
              IsBoot ->
                let diag = Can'tFindInterface err
                             (LookingForHiBoot mod)
                in failWithTc (TcRnInterfaceError diag)
              -- The hi-boot file has mysteriously disappeared.
              NotBoot -> failWithTc (TcRnInterfaceError (CircularImport mod))
              -- Someone below us imported us!
              -- This is a loop with no hi-boot in the way
    }}}}
  where
    need = text "Need the hi-boot interface for" <+> ppr mod
                 <+> text "to compare against the Real Thing"

{-
************************************************************************
*                                                                      *
                Type and class declarations
*                                                                      *
************************************************************************

When typechecking a data type decl, we *lazily* (via forkM) typecheck
the constructor argument types.  This is in the hope that we may never
poke on those argument types, and hence may never need to load the
interface files for types mentioned in the arg types.

E.g.
        data Foo.S = MkS Baz.T
Maybe we can get away without even loading the interface for Baz!

This is not just a performance thing.  Suppose we have
        data Foo.S = MkS Baz.T
        data Baz.T = MkT Foo.S
(in different interface files, of course).
Now, first we load and typecheck Foo.S, and add it to the type envt.
If we do explore MkS's argument, we'll load and typecheck Baz.T.
If we explore MkT's argument we'll find Foo.S already in the envt.

If we typechecked constructor args eagerly, when loading Foo.S we'd try to
typecheck the type Baz.T.  So we'd fault in Baz.T... and then need Foo.S...
which isn't done yet.

All very cunning. However, there is a rather subtle gotcha which bit
me when developing this stuff.  When we typecheck the decl for S, we
extend the type envt with S, MkS, and all its implicit Ids.  Suppose
(a bug, but it happened) that the list of implicit Ids depended in
turn on the constructor arg types.  Then the following sequence of
events takes place:
        * we build a thunk <t> for the constructor arg tys
        * we build a thunk for the extended type environment (depends on <t>)
        * we write the extended type envt into the global EPS mutvar

Now we look something up in the type envt
        * that pulls on <t>
        * which reads the global type envt out of the global EPS mutvar
        * but that depends in turn on <t>

It's subtle, because, it'd work fine if we typechecked the constructor args
eagerly -- they don't need the extended type envt.  They just get the extended
type envt by accident, because they look at it later.

What this means is that the implicitTyThings MUST NOT DEPEND on any of
the forkM stuff.
-}

tcIfaceDecl :: Bool     -- ^ True <=> discard IdInfo on IfaceId bindings
            -> IfaceDecl
            -> IfL TyThing
tcIfaceDecl = tc_iface_decl Nothing

tc_iface_decl :: Maybe Class  -- ^ For associated type/data family declarations
              -> Bool         -- ^ True <=> discard IdInfo on IfaceId bindings
              -> IfaceDecl
              -> IfL TyThing
tc_iface_decl _ ignore_prags (IfaceId {ifName = name, ifType = iface_type,
                                       ifIdDetails = raw_details, ifIdInfo = info,
                                       ifIdSourceProofRules = deps })
  = do  { ty <- tcIfaceType iface_type
        ; let (details, proof_rules) = case raw_details of
                IfSourceProofId legacy -> (IfVanillaId, map (, []) legacy)
                _ -> (raw_details, deps)
        ; details <- tcIdDetails name ty details
        ; info <- tcIdInfo ignore_prags TopLevel name ty info
        ; rules <- tcIfaceSourceProofRules proof_rules
        ; let checked_info = if null rules then info else setSourceProofRuleInfo info rules
        ; return (AnId (mkGlobalId details name ty checked_info)) }

tc_iface_decl _ _
  (IfaceData {ifName = tc_name,
              ifKind = kind,
              ifCType = cType,
              ifBinders = binders,
              ifNbEtaBinders = nb_eta_binders,
              ifResKind = res_kind,
              ifRoles = roles,
              ifCtxt = ctxt, ifGadtSyntax = gadt_syn,
              ifCons = rdr_cons,
              ifParent = mb_parent })
  = do { let has_retained = case rdr_cons of
                              IfDataTyCon _ cons -> any (not . null . ifConArgBinders) cons
                              IfNewTyCon con     -> not (null (ifConArgBinders con))
                              _                  -> False
       ; when has_retained $
           unless (null ctxt && case mb_parent of { IfNoParent -> True; _ -> False }) $
             failIfM (text "Invalid retained constructor fields in interface:"
                       <+> ppr tc_name
                       $$ text "Expected an ordinary data parent with an empty context.")
       ; kind' <- tcIfaceType kind
       ; shape <- tcIfaceDataDomainShape binders ctxt mb_parent rdr_cons
       ; tycon <- fixM $ \ tycon -> do
               { cons <- tcIfaceDataCons tc_name tycon rdr_cons
               ; bindIfaceTyConBinders_AT binders $ \ binders' ->
            do { res_kind' <- tcIfaceType res_kind
               ; when has_retained $ unless (res_kind' `eqType` liftedTypeKind) $
                   failIfM (text "Invalid retained constructor fields in interface:"
                             <+> ppr tc_name $$ text "The parent result kind must be Type.")
               ; when has_retained $ unless (kind' `eqType` mkTyConKind binders' res_kind') $
                   failIfM (text "Invalid retained constructor fields in interface:"
                             <+> ppr tc_name $$ text "The parent kind and its binders disagree.")
               ; stupid_theta <- tcIfaceCtxt ctxt
               ; parent' <- tc_parent tc_name mb_parent
               ; return (setTyConNativeDomainShape
                         (mkAlgTyCon tc_name kind' binders' nb_eta_binders res_kind'
                                    roles cType stupid_theta
                                    cons parent' gadt_syn) shape) } }
       ; traceIf (text "tcIfaceDecl4" <+> ppr tycon)
       ; return (ATyCon tycon) }
  where
    tc_parent :: Name -> IfaceTyConParent -> IfL AlgTyConFlav
    tc_parent tc_name IfNoParent
      = do { tc_rep_name <- newTyConRepName tc_name
           ; return (VanillaAlgTyCon tc_rep_name) }
    tc_parent _ (IfDataInstance ax_name _ arg_tys)
      = do { ax <- tcIfaceUnbranchedAxiom ax_name
           ; let fam_tc  = coAxiomTyCon ax
           ; lhs_tys <- tcIfaceAppArgs arg_tys
           ; return (DataFamInstTyCon ax fam_tc lhs_tys) }

tc_iface_decl _ _
  (IfaceSynonym {ifName = tc_name,
                 ifKind = kind,
                 ifRoles = roles,
                 ifSynRhs = rhs_ty,
                 ifSynSourceProofRules = proof_rules,
                 ifBinders = binders,
                 ifResKind = res_kind })
   = do { kind' <- tcIfaceType kind
        ; bindIfaceTyConBinders_AT binders $ \ binders' ->
     do { res_kind' <- tcIfaceType res_kind     -- Note [Synonym kind loop]
        ; rhs      <- forkM (mk_doc tc_name) $ do
            { rhs <- tcIfaceType rhs_ty
            ; case rhs of
                TyConApp target [] ->
                  -- See Note [Imported matchability prefixes].
                  checkImportedMatchabilityPrefix target
                    (tyConKind target `eqType` res_kind')
                _ -> return ()
            ; return rhs }
        ; shape <- if null binders
                   then NativeSynonymDomain <$> tcIfaceDomainShape rhs_ty
                   else return NativeUnknownDomain
        ; let tycon = setTyConNativeDomainShape
                        (buildSynTyCon tc_name kind' binders' res_kind' roles rhs) shape
        ; deps <- tcIfaceSourceProofRules proof_rules
        ; return (ATyCon (setSynTyConSourceProofRules tycon deps)) } }
   where
     mk_doc n = text "Type synonym" <+> ppr n

tc_iface_decl parent _ (IfaceFamily {ifName = tc_name,
                                     ifKind = kind,
                                     ifFamFlav = fam_flav,
                                     ifBinders = binders,
                                     ifNbEtaBinders = nb_eta,
                                     ifResKind = res_kind,
                                     ifResVar = res, ifFamInj = inj })
   = do { kind' <- tcIfaceType kind
        ; bindIfaceTyConBinders_AT binders $ \ binders' ->
     do { res_kind' <- tcIfaceType res_kind    -- Note [Synonym kind loop]
        ; rhs      <- forkM (mk_doc tc_name) $
                      tc_fam_flav tc_name fam_flav
        ; res_name <- traverse (newIfaceName . mkTyVarOccFS . ifLclNameFS) res
        ; let tycon = mkFamilyTyCon tc_name kind' binders' nb_eta res_kind' res_name rhs parent inj
        ; return (ATyCon tycon) } }
   where
     mk_doc n = text "Type synonym" <+> ppr n

     tc_fam_flav :: Name -> IfaceFamTyConFlav -> IfL FamTyConFlav
     tc_fam_flav tc_name IfaceDataFamilyTyCon
       = do { tc_rep_name <- newTyConRepName tc_name
            ; return (DataFamilyTyCon tc_rep_name) }
     tc_fam_flav _ IfaceOpenTypeFamilyTyCon = return OpenTypeFamilyTyCon
     tc_fam_flav _ IfaceConstrainedTypeFamilyTyCon =
       return ConstrainedTypeFamilyTyCon
     tc_fam_flav local (IfaceClosedTypeFamilyTyCon ctf) = ClosedTypeFamilyTyCon <$>
       case ctf of
        IfaceClosedTyFamTyCon mb_ax_name_branches ->
         do { ax <- traverse (tcIfaceBranchedAxiom . fst) mb_ax_name_branches
            ; return (CTF ax) }
        IfaceScopedClosedTyFamTyCon owner is_sum (ax_name, _) -> do
            when (isNothing (nameModule_maybe owner)
                  || nameModule_maybe owner /= nameModule_maybe local
                  || (owner == local && not is_sum))
              (failIfM (text "Invalid lexical scope for a retained-sum family:" <+> ppr local))
            CTF_Scoped owner is_sum <$> tcIfaceBranchedAxiom ax_name
        IfaceLambdaClosedTyFamTyCon (ax_name, _) ->
            CTF_Scoped local False <$> tcIfaceBranchedAxiom ax_name
        IfaceAbstractClosedTyFamTyCon -> return CTF_Abstract
        IfaceBuiltInClosedTyFamTyCon ->
          pprPanic "tc_iface_decl" $
            text "IfaceBuiltInClosedTyFamTyCon in interface file"

tc_iface_decl _parent _ignore_prags
            (IfaceClass {ifName = tc_name,
                         ifKind = kind,
                         ifRoles = roles,
                         ifBinders = binders,
                         ifFDs = rdr_fds,
                         ifBody = IfAbstractClass})
  = do { kind' <- tcIfaceType kind
       ; bindIfaceTyConBinders binders $ \ binders' ->
    do { fds  <- mapM tc_fd rdr_fds
       ; cls  <- buildAbstractClass tc_name kind' binders' roles fds
       ; return (ATyCon (classTyCon cls)) } }

tc_iface_decl _parent ignore_prags
            (IfaceClass {ifName = tc_name,
                         ifKind = kind,
                         ifRoles = roles,
                         ifBinders = binders,
                         ifFDs = rdr_fds,
                         ifBody = IfConcreteClass {
                             ifClassCtxt = rdr_ctxt,
                             ifATs = rdr_ats, ifSigs = rdr_sigs,
                             ifMinDef = if_mindef, ifUnary = unary
                         }})
  = do { kind' <- tcIfaceType kind
       ; bindIfaceTyConBinders binders $ \ binders' ->
    do { traceIf (text "tc-iface-class1" <+> ppr tc_name)
       ; ctxt <- mapM tc_sc rdr_ctxt
       ; traceIf (text "tc-iface-class2" <+> ppr tc_name)
       ; sigs <- mapM tc_sig rdr_sigs
       ; fds  <- mapM tc_fd rdr_fds
       ; traceIf (text "tc-iface-class3" <+> ppr tc_name)
       ; mindef <- tc_boolean_formula if_mindef
       ; cls  <- fixM $ \ cls -> do
                 { ats  <- mapM (tc_at cls) rdr_ats
                 ; traceIf (text "tc-iface-class4" <+> ppr tc_name)
                 ; buildClass tc_name kind' binders' roles fds ctxt ats sigs mindef unary }
       ; return (ATyCon (classTyCon cls)) } }
  where
   tc_sc pred = forkM (mk_sc_doc pred) (tcIfaceType pred)
        -- The *length* of the superclasses is used by buildClass, and hence must
        -- not be inside the thunk.  But the *content* maybe recursive and hence
        -- must be lazy (via forkM).  Example:
        --     class C (T a) => D a where
        --       data T a
        -- Here the associated type T is knot-tied with the class, and
        -- so we must not pull on T too eagerly.  See #5970

   tc_sig :: IfaceClassOp -> IfL TcMethInfo
   tc_sig (IfaceClassOp op_name rdr_ty dm)
     = do { let doc = mk_op_doc op_name rdr_ty
          ; op_ty <- forkM (doc <+> text "ty") $ tcIfaceType rdr_ty
                -- Must be done lazily for just the same reason as the
                -- type of a data con; to avoid sucking in types that
                -- it mentions unless it's necessary to do so
          ; dm'   <- tc_dm doc dm
          ; return (op_name, op_ty, dm') }

   tc_dm :: SDoc
         -> Maybe (DefMethSpec IfaceType)
         -> IfL (Maybe (DefMethSpec (SrcSpan, Type)))
   tc_dm _   Nothing               = return Nothing
   tc_dm _   (Just VanillaDM)      = return (Just VanillaDM)
   tc_dm doc (Just (GenericDM ty))
        = do { -- Must be done lazily to avoid sucking in types
             ; ty' <- forkM (doc <+> text "dm") $ tcIfaceType ty
             ; return (Just (GenericDM (noSrcSpan, ty'))) }

   tc_at cls (IfaceAT tc_decl if_def)
     = do ATyCon tc <- tc_iface_decl (Just cls) ignore_prags tc_decl
          mb_def <- case if_def of
                      Nothing  -> return Nothing
                      Just def -> forkM (mk_at_doc tc)                 $
                                  extendIfaceTyVarEnv (tyConTyVars tc) $
                                  do { tc_def <- tcIfaceType def
                                     ; return (Just (tc_def, NoVI)) }
                  -- Must be done lazily in case the RHS of the defaults mention
                  -- the type constructor being defined here
                  -- e.g.   type AT a; type AT b = AT [b]   #8002
          return (ATI tc mb_def)

   tc_boolean_formula :: IfaceBooleanFormula -> IfL (BooleanFormula GhcRn)
   tc_boolean_formula (IfAnd ibfs  ) = BF.And    NoExtField . map noLocA <$> traverse tc_boolean_formula ibfs
   tc_boolean_formula (IfOr ibfs   ) = BF.Or     NoExtField . map noLocA <$> traverse tc_boolean_formula ibfs
   tc_boolean_formula (IfParens ibf) = BF.Parens noAnn      .     noLocA <$>          tc_boolean_formula ibf
   tc_boolean_formula (IfVar nm    ) = BF.Var    NoExtField .     noLocA <$> (lookupIfaceTop . mkVarOccFS . ifLclNameFS $ nm)

   mk_sc_doc pred = text "Superclass" <+> ppr pred
   mk_at_doc tc = text "Associated type" <+> ppr tc
   mk_op_doc op_name op_ty = text "Class op" <+> sep [ppr op_name, ppr op_ty]

tc_iface_decl _ _ (IfaceAxiom { ifName = tc_name, ifTyCon = tc
                              , ifAxBranches = branches, ifAxSourceSpans = spans
                              , ifAxSourceProofRules = deps, ifRole = role })
  = do { tc_tycon    <- tcIfaceTyCon tc
       -- Must be done lazily, because axioms are forced when checking
       -- for family instance consistency, and the RHS may mention
       -- a hs-boot declared type constructor that is going to be
       -- defined by this module.
       -- e.g. type instance F Int = ToBeDefined
       -- See #13803
       ; tc_branches <- forkM (text "Axiom branches" <+> ppr tc_name)
                      $ tc_ax_branches spans branches
       ; rules <- traverse (traverse tcIfaceSourceProofRules) deps
       ; let axiom = (mkBranchedCoAxiom role tc_name tc_tycon tc_branches)
                      { co_ax_source_proof_rules = rules }
       ; return (ACoAxiom axiom) }

tc_iface_decl _ _ (IfacePatSyn{ ifName = name
                              , ifPatMatcher = if_matcher
                              , ifPatBuilder = if_builder
                              , ifPatIsInfix = is_infix
                              , ifPatUnivBndrs = univ_bndrs
                              , ifPatExBndrs = ex_bndrs
                              , ifPatProvCtxt = prov_ctxt
                              , ifPatReqCtxt = req_ctxt
                              , ifPatArgs = args
                              , ifPatTy = pat_ty
                              , ifFieldLabels = field_labels })
  = do { traceIf (text "tc_iface_decl" <+> ppr name)
       ; matcher <- tc_pr if_matcher
       ; builder <- traverse tc_pr if_builder
       ; bindIfaceForAllBndrs univ_bndrs $ \univ_tvs -> do
       { bindIfaceForAllBndrs ex_bndrs $ \ex_tvs -> do
       { patsyn <- forkM (mk_doc name) $
             do { prov_theta <- tcIfaceCtxt prov_ctxt
                ; req_theta  <- tcIfaceCtxt req_ctxt
                ; pat_ty     <- tcIfaceType pat_ty
                ; arg_tys    <- mapM tcIfaceType args
                ; return $ buildPatSyn name is_infix matcher builder
                                       (univ_tvs, req_theta)
                                       (ex_tvs, prov_theta)
                                       arg_tys pat_ty field_labels }
       ; return $ AConLike . PatSynCon $ patsyn }}}
  where
     mk_doc n = text "Pattern synonym" <+> ppr n
     tc_pr :: (IfExtName, Bool) -> IfL (Name, Type, Bool)
     tc_pr (nm, b) = do { id <- forkM (ppr nm) (tcIfaceExtId nm)
                        ; return (nm, idType id, b) }

tcTopIfaceBindings :: IORef TypeEnv -> [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo]
          -> IfL [CoreBind]
tcTopIfaceBindings ty_var ver_decls
   = do
      int <- mapM tcTopBinders ver_decls
      let all_ids :: [Id] = concatMap toList int
      liftIO $ modifyIORef ty_var (flip extendTypeEnvList (map AnId all_ids))

      extendIfaceIdEnv all_ids $ mapM tc_iface_bindings int

tcTopBinders :: IfaceBindingX a IfaceTopBndrInfo -> IfL (IfaceBindingX a Id)
tcTopBinders = traverse mk_top_id

tc_iface_bindings ::  IfaceBindingX IfaceMaybeRhs Id -> IfL CoreBind
tc_iface_bindings (IfaceNonRec b rhs) = do
    rhs' <- tc_iface_binding b rhs
    return $ NonRec b rhs'
tc_iface_bindings (IfaceRec bs) = do
  rs <- mapM (\(b, rhs) -> (b,) <$> tc_iface_binding b rhs) bs
  return (Rec rs)

-- | See Note [Interface File with Core: Sharing RHSs]
tc_iface_binding :: Id -> IfaceMaybeRhs -> IfL CoreExpr
tc_iface_binding i IfUseUnfoldingRhs =
  case maybeUnfoldingTemplate $ realIdUnfolding i of
    Just e -> return e
    Nothing -> pprPanic "tc_iface_binding" (vcat [text "Binding" <+> quotes (ppr i) <+> text "had an unfolding when the interface file was created"
                                                 , text "which has now gone missing, something has badly gone wrong."
                                                 , text "Unfolding:" <+> ppr (realIdUnfolding i)])

tc_iface_binding _ (IfRhs rhs) = tcIfaceExpr rhs

mk_top_id :: IfaceTopBndrInfo -> IfL Id
mk_top_id (IfGblTopBndr gbl_name)
  -- See Note [Root-main Id]
  -- This special binding is actually defined in the current module
  -- (hence don't go looking for it externally) but the module name is rOOT_MAIN
  -- rather than the current module so we need this special case.
  -- See some similar logic in `GHC.Rename.Env`.
  | Just rOOT_MAIN == nameModule_maybe gbl_name
    = loadKnownKeyOccMaps >>= \case
        Failed err -> failIfM (pprDiagnostic err)
        Succeeded kk_maps -> lookupKnownKeyThing ioTyConKey (KES_FromModule kk_maps) >>= \case
          Failed err          -> failIfM (pprDiagnostic err)
          Succeeded ioTyThing -> do
            ATyCon ioTyCon <- pure ioTyThing
            return $ mkExportedVanillaId gbl_name (mkTyConApp ioTyCon [unitTy])
  | otherwise = tcIfaceExtId gbl_name
mk_top_id (IfLclTopBndr raw_name iface_type info details) = do
   ty <- tcIfaceType iface_type
   rec { details' <- tcIdDetails name ty details
       ; let occ = case details' of
                 RecSelId { sel_tycon = parent }
                   -> let con_fs = getOccFS $ recSelFirstConName parent
                      in mkRecFieldOccFS con_fs (ifLclNameFS raw_name)
                 _ -> mkVarOccFS (ifLclNameFS raw_name)
       ; name <- newIfaceName occ }
   info' <- tcIdInfo False TopLevel name ty info
   let new_id = mkGlobalId details' name ty info'
   return new_id

-- | Read declarations, omitting optional templates outside the supported subset.
tcIfaceDeclsFromIface :: Bool -> ModIface -> IfL [(Name, TyThing)]
tcIfaceDeclsFromIface ignore_prags = tcIfaceDecls ignore_prags . ifaceDeclsForTc

tcIfaceDecls :: Bool
          -> [(Fingerprint, IfaceDecl)]
          -> IfL [(Name,TyThing)]
tcIfaceDecls ignore_prags ver_decls
   = concatMapM (tc_iface_decl_fingerprint ignore_prags) ver_decls

-- | Remove optional erased package templates before exposing IdInfo.
-- See Note [Erased package unfoldings].
ifaceDeclsForTc :: ModIface -> [(Fingerprint, IfaceDecl)]
ifaceDeclsForTc iface
  | FullIfaceBinHandle (Strict.Just (features, _)) <- mi_hi_bytes iface
  , any (`Set.member` features) [ErasedExistentialEvidence, ImplicitExistentialEvidence]
  = [(fingerprint, prune decl) | (fingerprint, decl) <- mi_decls iface]
  | otherwise = mi_decls iface
  where
    prune decl@IfaceId { ifIdInfo = info } = decl { ifIdInfo = filter keep info }
    prune decl = decl

    keep (HsUnfold _ (IfCoreUnfold src _ _ rhs))
      | RetainedUserSrc <- src = True
      | otherwise = isCompulsorySource src || not (hasIfacePackageExpr rhs)
    keep _ = True

{- Note [Erased package unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The retained optimizer can process Pack and Unpack. An optional template
with these expressions can cross an interface when its top-level binding
has the RetainedUserSrc tag. The tag selects that optimizer in a client.
Check the expression forms, types and scope before using the template.
Check nested unfolding templates in their own thunks. Older readers know
the package and stable-template tags and discard the optional template.
They can still call the original object code.

Check the serialized syntax only when the interface header records an
erased existential tag. This leaves ordinary interface loading unchanged
and keeps the typed unfolding lazy. See Note [Lazily checking Unfoldings].
Keep mandatory templates. Restricted SimpleOpt must expand them before
the driver checks whether the result can enter optimization.
-}

tc_iface_decl_fingerprint :: Bool                    -- Don't load pragmas into the decl pool
          -> (Fingerprint, IfaceDecl)
          -> IfL [(Name,TyThing)]   -- The list can be poked eagerly, but the
                                    -- TyThings are forkM'd thunks
tc_iface_decl_fingerprint ignore_prags (_version, decl)
  = do  {       -- Populate the name cache with final versions of all
                -- the names associated with the decl
          let !main_name = ifName decl
                -- Force this field access, as `main_name` thunk will otherwise
                -- be retained in the thunk created by `forkM`.

        -- Typecheck the thing, lazily
        -- NB. Firstly, the laziness is there in case we never need the
        -- declaration (in one-shot mode), and secondly it is there so that
        -- we don't look up the occurrence of a name before calling mk_new_bndr
        -- on the binder.  This is important because we must get the right name
        -- which includes its nameParent.

        ; thing <- forkM doc $ do { bumpDeclStats main_name
                                  ; tcIfaceDecl ignore_prags decl }

        -- Populate the type environment with the implicitTyThings too.
        --
        -- Note [Tricky iface loop]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~
        -- Summary: The delicate point here is that 'mini-env' must be
        -- buildable from 'thing' without demanding any of the things
        -- 'forkM'd by tcIfaceDecl.
        --
        -- In more detail: Consider the example
        --      data T a = MkT { x :: T a }
        -- The implicitTyThings of T are:  [ <datacon MkT>, <selector x>]
        -- (plus their workers, wrappers, coercions etc etc)
        --
        -- We want to return an environment
        --      [ "MkT" -> <datacon MkT>, "x" -> <selector x>, ... ]
        -- (where the "MkT" is the *Name* associated with MkT, etc.)
        --
        -- We do this by mapping the implicit_names to the associated
        -- TyThings.  By the invariant on ifaceDeclImplicitBndrs and
        -- implicitTyThings, we can use getOccName on the implicit
        -- TyThings to make this association: each Name's OccName should
        -- be the OccName of exactly one implicitTyThing.  So the key is
        -- to define a "mini-env"
        --
        -- [ 'MkT' -> <datacon MkT>, 'x' -> <selector x>, ... ]
        -- where the 'MkT' here is the *OccName* associated with MkT.
        --
        -- However, there is a subtlety: due to how type checking needs
        -- to be staged, we can't poke on the forkM'd thunks inside the
        -- implicitTyThings while building this mini-env.
        -- If we poke these thunks too early, two problems could happen:
        --    (1) When processing mutually recursive modules across
        --        hs-boot boundaries, poking too early will do the
        --        type-checking before the recursive knot has been tied,
        --        so things will be type-checked in the wrong
        --        environment, and necessary variables won't be in
        --        scope.
        --
        --    (2) Looking up one OccName in the mini_env will cause
        --        others to be looked up, which might cause that
        --        original one to be looked up again, and hence loop.
        --
        -- The code below works because of the following invariant:
        -- getOccName on a TyThing does not force the suspended type
        -- checks in order to extract the name. For example, we don't
        -- poke on the "T a" type of <selector x> on the way to
        -- extracting <selector x>'s OccName. Of course, there is no
        -- reason in principle why getting the OccName should force the
        -- thunks, but this means we need to be careful in
        -- implicitTyThings and its helper functions.
        --
        -- All a bit too finely-balanced for my liking.

        -- This mini-env and lookup function mediates between the
        --'Name's n and the map from 'OccName's to the implicit TyThings
        ; let mini_env = mkOccEnv [(getOccName t, t) | t <- implicitTyThings thing]
              lookup n = case lookupOccEnv mini_env (getOccName n) of
                           Just thing -> thing
                           Nothing    ->
                             pprPanic "tc_iface_decl_fingerprint" (ppr main_name <+> ppr n $$ ppr (decl))

        ; implicit_names <- mapM lookupIfaceTop (ifaceDeclImplicitBndrs decl)

--         ; traceIf (text "Loading decl for " <> ppr main_name $$ ppr implicit_names)
        ; return $ (main_name, thing) :
                      -- uses the invariant that implicit_names and
                      -- implicitTyThings are bijective
                      [(n, lookup n) | n <- implicit_names]
        }
  where
    doc = text "Declaration for" <+> ppr (ifName decl)

bumpDeclStats :: Name -> IfL ()         -- Record that one more declaration has actually been used
bumpDeclStats name
  = do  { traceIf (text "Loading decl for" <+> ppr name)
        ; updateEps_ (\eps -> let stats = eps_stats eps
                              in eps { eps_stats = stats { n_decls_out = n_decls_out stats + 1 } })
        }

tc_fd :: FunDep IfLclName -> IfL (FunDep TyVar)
tc_fd (tvs1, tvs2) = do { tvs1' <- mapM tcIfaceTyVar tvs1
                        ; tvs2' <- mapM tcIfaceTyVar tvs2
                        ; return (tvs1', tvs2') }

tc_ax_branches :: [SrcSpan] -> [IfaceAxBranch] -> IfL [CoAxBranch]
tc_ax_branches spans branches = do
    this_mod <- getIfModule
    let fallback = mkGeneralSrcSpan (fsLit "module " `appendFS` moduleNameFS (moduleName this_mod))
        locs | not (null spans), equalLength spans branches = spans
             | otherwise = repeat fallback
    zipWithM tc_ax_branch locs branches

tc_ax_branch :: SrcSpan -> IfaceAxBranch -> IfL CoAxBranch
tc_ax_branch loc (IfaceAxBranch { ifaxbTyVars = tv_bndrs
                            , ifaxbEtaTyVars = eta_tv_bndrs
                            , ifaxbCoVars = cv_bndrs
                            , ifaxbLHS = lhs, ifaxbRHS = rhs
                            , ifaxbRoles = roles })
  = bindIfaceTyConBinders_AT
      (map (\b -> Bndr (IfaceTvBndr b) (NamedTCB Inferred)) tv_bndrs) $ \ tvs ->
         -- The _AT variant is needed here; see Note [CoAxBranch type variables] in GHC.Core.Coercion.Axiom
    bindIfaceIds cv_bndrs $ \ cvs -> do
    { tc_lhs   <- tcIfaceAppArgs lhs
    ; tc_rhs   <- tcIfaceType rhs
    ; eta_tvs  <- bindIfaceTyVars eta_tv_bndrs return
    ; return (CoAxBranch { cab_loc      = loc
                         , cab_tvs      = binderVars tvs
                         , cab_eta_tvs  = eta_tvs
                         , cab_cvs      = cvs
                         , cab_lhs      = tc_lhs
                         , cab_roles    = roles
                         , cab_rhs      = tc_rhs
                         , cab_overlaps = placeHolderOverlaps
                         , cab_incomps  = placeHolderIncomps }) }

tcIfaceDataCons :: Name -> TyCon -> IfaceConDecls -> IfL AlgTyConRhs
tcIfaceDataCons tycon_name tycon if_cons
  = case if_cons of
        IfAbstractTyCon
          -> return AbstractTyCon
        IfDataTyCon type_data cons
          -> do  { data_cons  <- mapM tc_con_decl cons
                 ; return $
                     mkLevPolyDataTyConRhs
                       (isFixedRuntimeRepKind $ tyConResKind tycon)
                       type_data
                       data_cons }
        IfNewTyCon con
          -> do  { data_con  <- tc_con_decl con
                 ; mkNewTyConRhs tycon_name tycon data_con }
  where

    tag_map :: NameEnv ConTag
    tag_map = mkTyConTagMap tycon

    tc_con_decl (IfCon { ifConInfix = is_infix,
                         ifConUnivTvs = univ_bndrs,
                         ifConExTCvs = ex_bndrs,
                         ifConUserTvBinders = user_bndrs,
                         ifConName = dc_name, ifConWrapper = has_wrapper,
                         ifConArgBinders = arg_binders,
                         ifConCtxt = ctxt, ifConEqSpec = spec,
                         ifConArgTys = args, ifConFields = lbl_names,
                         ifConStricts = if_stricts,
                         ifConSrcStricts = if_src_stricts,
                         ifConSourceOrder = source_order})
     = do { let terminal_order = legacyConOrder source_order
                context_only = terminal_order == gadtTrailingContextOnly
                late_count = max 0 terminal_order
                ordered_type_groups = case source_order of
                  OrderedConArgs groups -> orderedConTypeGroups groups
                  LegacyConOrder _ -> []
                ordered_late_count = late_count + sum (map snd ordered_type_groups)
                admissible_order
                  | context_only = not (null ctxt) && ordinary_data && all type_binder ex_bndrs
                  | otherwise = late_count > 0 && late_count <= length user_bndrs && null ex_bndrs
          ; case source_order of
              LegacyConOrder _ -> pure ()
              OrderedConArgs groups ->
                let valid_group = case groups of
                      ConTypeArgs initial : rest ->
                        initial >= 0 && all positive rest &&
                        initial + ordered_late_count == length user_bndrs &&
                        sum [n | ConContextArgs n <- rest] == length ctxt &&
                        sum [n | ConValueArgs n <- rest] == length args &&
                        (any non_field (drop 1 (dropWhile non_field rest)) ||
                         leading_context_order rest)
                      _ -> False
                    positive (ConTypeArgs n) = n > 0
                    positive (ConContextArgs n) = n > 0
                    positive (ConValueArgs n) = n > 0
                    non_field ConValueArgs{} = False
                    non_field _ = True
                    leading_context_order (ConContextArgs{} : _) =
                      not (null args) && any ((== 0) . fst) ordered_type_groups
                    leading_context_order _ = False
                in unless (valid_group && all type_binder ex_bndrs && ordinary_data &&
                           has_wrapper && null arg_binders && null spec && null lbl_names) $
                     failIfM (text "Invalid interior constructor group in interface:" <+> ppr dc_name)
          ; when (terminal_order /= 0) $
              unless (admissible_order && has_wrapper && not (null args) &&
                      null arg_binders && null spec &&
                      null lbl_names) $
                failIfM (text "Invalid terminal constructor foralls in interface:" <+> ppr dc_name)
          ; unless (null arg_binders) $
              check_retained_shape dc_name has_wrapper arg_binders args univ_bndrs user_bndrs
                ex_bndrs spec ctxt lbl_names if_stricts if_src_stricts
          ; bindIfaceForAllBndrs user_bndrs $ \ if_user_tvs ->
       let user_tvs = [Bndr tv (ifaceForAllTyFlag vis) | Bndr tv vis <- if_user_tvs] in
       bindDataConBinders   univ_bndrs $ \ univ_tvs ->
       bindDataConBinders   ex_bndrs   $ \ ex_tvs  ->
     do { traceIf (text "Start interface-file tc_con_decl" <+> ppr dc_name)
        ; unless (null arg_binders) $
            forM_ [(kind, tv) | (IfaceTvBndr (_, kind), tv) <- zip univ_bndrs univ_tvs] $
              \(kind, tv) -> do
              { declared_kind <- tcIfaceType kind
              ; unless (declared_kind `eqType` varType tv) $
                  failIfM (text "Invalid retained constructor fields in interface:"
                            <+> ppr dc_name
                            $$ text "Universal and user binder kinds disagree:" <+> ppr tv) }

        ; let (early, late) = splitAt (length user_tvs - ordered_late_count) user_tvs
              late_vars = mkVarSet (binderVars late)
              trailing_context = context_only ||
                (late_count /= 0 && all (isSpecifiedForAllTyFlag . binderFlag) late)
              check_prefix_scope tys = when (ordered_late_count /= 0) $
                unless (disjointVarSet late_vars (tyCoVarsOfTypes tys)) $
                  failIfM (text "Invalid terminal constructor binder scope:" <+> ppr dc_name)
        ; case source_order of
            OrderedConArgs _ ->
              unless (valid_type_groups late ordered_type_groups &&
                      late_vars `subVarSet` mkVarSet (univ_tvs ++ ex_tvs)) $
                failIfM (text "Invalid interior constructor binder scope:" <+> ppr dc_name)
            LegacyConOrder _ -> pure ()
        ; when (late_count /= 0) $
            unless ((all isVisibleForAllTyBinder late || (trailing_context && not (null ctxt))) &&
                    late_vars `subVarSet` mkVarSet univ_tvs) $
              failIfM (text "Invalid terminal constructor binder scope:" <+> ppr dc_name)

        -- Read the context and argument types, but lazily for two reasons
        -- (a) to avoid looking tugging on a recursive use of
        --     the type itself, which is knot-tied
        -- (b) to avoid faulting in the component types unless
        --     they are really needed
        ; ~(eq_spec, theta, retained_binders, arg_tys, stricts) <- forkM (mk_doc dc_name) $
             do { theta <- tcIfaceCtxt ctxt
                ; unless (null arg_binders || validRetainedConTheta theta) $
                    failIfM (text "Invalid retained constructor context in interface:" <+> ppr dc_name)
                -- A predicate or field can refer to this TyCon. Check its
                -- scope in the same lazy computation that reads the type.
                ; let prefix_theta = if trailing_context then [] else theta
                ; when (terminal_order /= 0) $
                    check_prefix_scope (map (varType . binderVar) early ++ prefix_theta)
                -- This fixes #13710.  The enclosing lazy thunk gets
                -- forced when typechecking record wildcard pattern
                -- matching (it's not completely clear why this
                -- tuple is needed), which causes trouble if one of
                -- the argument types was recursively defined.
                -- See also Note [Tying the knot]
                ; ~(retained_binders, arg_tys) <-
                    if null arg_binders
                    then do { arg_tys <- forkM (mk_doc dc_name <+> text "arg_tys") $
                                  do { tys <- mapM (\(w, ty) -> mkScaled <$> tcIfaceType w <*> tcIfaceType ty) args
                                     ; case source_order of
                                         OrderedConArgs groups ->
                                           unless (orderedConArgsHaveScope groups user_tvs theta tys) $
                                             failIfM (text "Invalid interior constructor binder scope:" <+> ppr dc_name)
                                         _ -> check_prefix_scope (concatMap (\(Scaled mult ty) -> [mult, ty]) tys)
                                     ; return tys }
                            ; return ([], arg_tys) }
                    else tc_retained_args dc_name emptyVarSet arg_binders args
                ; let retained_fields = [field | Just field <- retained_binders]
                ; eq_spec <- extendIfaceIdEnv retained_fields $ tcIfaceEqSpec spec
                ; when (not (null arg_binders) && not (null spec)) $
                    case (retained_fields, eq_spec) of
                      ([field], [eq]) -> do
                        { valid <- evaluateIfaceNativeDomain (ppr dc_name)
                            (isNativeClosedIndexType (varType field)
                             && varType field `eqType` varType (eqSpecTyVar eq)
                             && all (not . anyFreeVarsOfType isId . scaledMult) arg_tys)
                        ; unless valid $
                            failIfM (text "Invalid retained result field in interface:" <+> ppr dc_name) }
                      _ -> failIfM (text "Invalid retained result field in interface:" <+> ppr dc_name)
                ; stricts <- mapM tc_strict if_stricts
                        -- The IfBang field can mention
                        -- the type itself; hence inside forkM
                ; return (eq_spec, theta, retained_binders, arg_tys, stricts) }

        -- Remember, tycon is the representation tycon
        ; let orig_res_ty = mkFamilyTyConApp tycon
                              (substTyCoVars (mkTvSubstPrs (map eqSpecPair eq_spec))
                                             univ_tvs)

        ; prom_rep_name <- newTyConRepName dc_name

        ; let bang_opts = FixedBangOpts stricts
            -- Pass the HsImplBangs (i.e. final decisions) to buildDataCon;
            -- it'll use these to guide the construction of a worker.
            -- See Note [Bangs on imported data constructors] in GHC.Types.Id.Make

        ; con <- buildDataConWithTelescope source_order
                       (if null arg_binders then [] else retained_binders)
                       (pprPanic "tcIfaceDataCons: FamInstEnvs" (ppr dc_name))
                       bang_opts
                       dc_name is_infix prom_rep_name
                       (map src_strict if_src_stricts)
                       lbl_names
                       univ_tvs ex_tvs user_tvs
                       eq_spec theta
                       arg_tys orig_res_ty tycon tag_map
        ; traceIf (text "Done interface-file tc_con_decl" <+> ppr dc_name)
        ; return con } }

    ordinary_data = case if_cons of
                      IfDataTyCon False _ -> True
                      _                   -> False

    type_binder (IfaceTvBndr _) = True
    type_binder _              = False

    -- Each written forall is either required or invisible. The existing
    -- binder flags also store each invisible binder's specificity.
    valid_type_groups bs [] = null bs
    valid_type_groups bs ((_, n) : rest) =
      let (here, later) = splitAt n bs
      in (all isVisibleForAllTyBinder here ||
          all isInvisibleForAllTyBinder here) &&
         valid_type_groups later rest

    -- Check only interface syntax here. The parent TyCon is still knot-tied.
    check_retained_shape name wrapper binders args univs users exs spec ctxt fields bangs src_bangs
      = unless (binders `equalLength` args
                && length names == length (nub names)
                && all type_binder univs && all (\(Bndr b _) -> type_binder b) users
                && length univ_names == length (nub univ_names)
                && length user_names == length (nub user_names)
                && valid_result && null exs && null fields
                && (null ctxt || (not wrapper && null spec && user_names == univ_names))
                && ordinary_data
                && (null bangs || bangs `equalLength` args)
                && all lazy_bang bangs
                && (null src_bangs || src_bangs `equalLength` args)
                && all lazy_src_bang src_bangs) $
          failIfM (text "Invalid retained constructor fields in interface:" <+> ppr name
                   $$ text "Expected aligned distinct field names and a lazy data constructor"
                   $$ text "without an existential or record field."
                   $$ text "User type binders must match the universal binders outside the result equality.")
      where
        names = [n | Just n <- binders]
        univ_names = map ifaceBndrName univs
        user_names = [ifaceBndrName b | Bndr b _ <- users]
        valid_result
          | wrapper
          , [field] <- names
          , [(lhs, IfaceTermVar rhs)] <- spec
          = lhs `elem` univ_names && rhs == field
            && matching_users (filter (/= lhs) univ_names)
          | otherwise
          = not wrapper && null spec && matching_users univ_names
        matching_users expected = expected `equalLength` user_names && all (`elem` expected) user_names
        lazy_bang IfNoBang = True
        lazy_bang _        = False
        lazy_src_bang (IfSrcBang unpack strict)
          = not (isSrcUnpacked unpack) && not (isSrcStrict strict)

    -- Allocate the lexical binder skeleton before forcing its domain.
    -- See Note [Importing retained constructor field telescopes].
    tc_retained_args _ _ [] [] = return ([], [])
    tc_retained_args name scope (mb : bs) ((w, ty) : args)
      = do { ~(mult, field_ty) <- forkM (mk_doc name <+> text "retained field") $
               do { when (isJust mb) $
                      do { mult_shape <- tcIfaceDomainShape w
                         ; domain_shape <- tcIfaceDomainShape ty
                         ; precheckIfaceNativeDomain
                             (text "Invalid retained constructor fields in interface:" <+> ppr name)
                             (nativeDomainShapePossible NativeManyMultUse mult_shape
                              && nativeDomainShapePossible NativeRetainedUse domain_shape) }
                  ; mult <- tcIfaceType w
                  ; field_ty <- tcIfaceType ty
                  ; valid <- evaluateIfaceNativeDomain (ppr name) $
                            typeKind mult `eqType` multiplicityTy
                            && isLiftedTypeKind (typeKind field_ty)
                            && not (anyFreeVarsOfType
                                 (\v -> isId v && not (v `elemVarSet` scope)) field_ty)
                            && case mb of
                                 Nothing -> True
                                 Just _  -> isManyTy mult && isNativeRetainedFieldType field_ty
                  ; unless valid $
                      failIfM (text "Invalid retained constructor fields in interface:" <+> ppr name
                               $$ text "Expected a lifted field with only earlier runtime indices;"
                               $$ text "a named field must have multiplicity Many and a supported retained domain.")
                  ; return (mult, field_ty) }
           ; case mb of
               Nothing -> do
                 { ~(rest, fields) <- tc_retained_args name scope bs args
                 ; return (Nothing : rest, Scaled mult field_ty : fields) }
               Just fs -> do
                 { field_name <- newIfaceName (mkVarOccFS (ifLclNameFS fs))
                 ; let field = mkLocalId field_name ManyTy field_ty
                 ; ~(rest, fields) <- extendIfaceIdEnv [field] $
                     tc_retained_args name (extendVarSet scope field) bs args
                 ; return (Just field : rest, Scaled mult field_ty : fields) } }
    tc_retained_args name _ _ _
      = failIfM (text "Invalid retained constructor fields in interface:" <+> ppr name
                   $$ text "Field names and argument types are not aligned.")

    mk_doc con_name = text "Constructor" <+> ppr con_name

    tc_strict :: IfaceBang -> IfL HsImplBang
    tc_strict IfNoBang = return (HsLazy)
    tc_strict IfStrict = return (HsStrict True)
    tc_strict IfUnpack = return (HsUnpack Nothing)
    tc_strict (IfUnpackCo if_co) = do { co <- tcIfaceCo if_co
                                      ; return (HsUnpack (Just co)) }

    src_strict :: IfaceSrcBang -> HsSrcBang
    src_strict (IfSrcBang unpk bang) = HsSrcBang NoSourceText unpk bang

{- Note [Importing retained constructor field telescopes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Allocate retained field Ids from left to right. Their names scope only over
later field types. Decode each field type in a separate forkM under that
lexical environment. Keep the Id skeleton independent of the decoded domain.
Use literal ManyTy for each named Id. Id construction forces multiplicity,
so using the deferred decoded multiplicity would also force domain validation.
Validate the encoded multiplicity in the deferred field decoder instead.

This separation is required for recursive interface knots. The parent TyCon
can expose its nonempty field metadata before a domain is validated. The native
index-domain checker rejects that metadata before it inspects the fields.
Self and mutual retained-domain cycles therefore fail instead of recursively
forcing their own domain validation. Constructor type validation must not run
when the DataCon skeleton is allocated. See Note [Retained constructor field
telescopes] in GHC.Core.DataCon.

A named field can bind an eligible opaque function head as well as a scalar.
Its domain remains independently closed. A later TermAppTy resolves its head
from this actual lexical Id environment. The complete field-type check still
rejects every runtime Id outside the earlier-field prefix. Keep these checks
deferred for recursive function domains too. See Note [Opaque call indices in
interfaces] in GHC.Iface.Type. See Note [Detected recursive native interface
domains] for the boundary around each field eligibility result.

The ordinary field decoder keeps its existing lazy boundary. No new field
name scopes over the constructor result, context, or erased binders. This
codec alone does not enable source dependent constructor declarations. Retained
declarations require an ordinary lifted parent. With no existential or
equality specification, user and universal binders name the same variables.
Check their names before binding, then check declared kinds after reuse;
bindDataConBinders alone reuses names without comparing their kinds.
-}

bindDataConBinders :: [IfaceBndr] -> ([CoreBndr] -> IfL a) -> IfL a
bindDataConBinders [] thing_inside
  = thing_inside []
bindDataConBinders (b : bs) thing_inside
  = bind_tv            b  $ \b'  ->
    bindDataConBinders bs $ \bs' ->
    thing_inside (b':bs')
  where
    bind_tv tv thing =
      do { mb_tv <- lookupIfaceVar tv
         ; case mb_tv of
             Just b' -> thing b'
             Nothing -> bindIfaceBndr tv thing
         }

tcIfaceEqSpec :: IfaceEqSpec -> IfL [EqSpec]
tcIfaceEqSpec spec
  = mapM do_item spec
  where
    do_item (occ, if_ty) = do { tv <- tcIfaceTyVar occ
                              ; ty <- tcIfaceType if_ty
                              ; return (mkEqSpec tv ty) }

{-
Note [Synonym kind loop]
~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we eagerly grab the *kind* from the interface file, but
build a forkM thunk for the *rhs* (and family stuff).  To see why,
consider this (#2412)

M.hs:       module M where { import X; data T = MkT S }
X.hs:       module X where { import {-# SOURCE #-} M; type S = T }
M.hs-boot:  module M where { data T }

When kind-checking M.hs we need S's kind.  But we do not want to
find S's kind from (typeKind S-rhs), because we don't want to look at
S-rhs yet!  Since S is imported from X.hi, S gets just one chance to
be defined, and we must not do that until we've finished with M.T.

Solution: record S's kind in the interface file; now we can safely
look at it.

************************************************************************
*                                                                      *
                Instances
*                                                                      *
************************************************************************
-}

tcRoughTyCon :: Maybe IfaceTyCon -> RoughMatchTc
tcRoughTyCon (Just tc) = RM_KnownTc (ifaceTyConName tc)
tcRoughTyCon Nothing   = RM_WildCard

{- Note [Tricky rehydrating IfaceDefaults loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's a potential circular dependency when rehydrating IfaceDefaults into
a DefaultEnv (a map from class names to defaults):

1. To construct a DefaultEnv, we need the Class objects corresponding to
   the class names in the IfaceDefault

2. If a class is defined in the current module, rehydrating it requires
   looking into the ModDetails we're currently building

3. But that ModDetails needs the DefaultEnv we're trying to create!

This creates a circular dependency:
   - DefaultEnv needs Class objects
   - Class objects (for current module) need ModDetails
   - ModDetails needs DefaultEnv

Our solution is to break this loop by using just the class name from
IfaceDefault, rather than trying to fully resolve the Class. Since
DefaultEnv is keyed by Name anyway, we don't need the full Class object
for the map construction - we only need it for the map values.

In tcIfaceDefault we create ClassDefaults records containing the actual
Class objects, but we do this *after* creating the DefaultEnv keyed by Name.

This approach allows us to tie the knot properly without causing a loop.
-}

-- | 'tcIfaceDefaults' rehydrates a list of default declarations
-- lazily, and returns a DefaultEnv.
tcIfaceDefaults :: Module -> [IfaceDefault] -> IfL DefaultEnv
tcIfaceDefaults this_mod defaults = do
  defaults <- mapM do_one defaults
  return $ mkDefaultEnv defaults
  where
    do_one idf = do
      -- Invariant: (className class_default) == name
      -- see Note [Tricky rehydrating IfaceDefaults loop]
      let name = ifDefaultCls idf

      -- Now look up the Class and the default types.
      -- We must use forkM here, as these may be knot-tied (see #25858).
      -- See Note [Rehydrating Modules] in GHC.Driver.Make
      -- as well as Note [Knot-tying typecheckIface] in GHC.IfaceToCore.
      class_default <- forkM (text "tcIfaceDefault" <+> ppr name) $ tcIfaceDefault this_mod idf
      return (name, class_default)

tcIfaceDefault :: Module -> IfaceDefault -> IfL ClassDefaults
tcIfaceDefault this_mod IfaceDefault { ifDefaultCls = cls_name
                                     , ifDefaultTys = tys
                                     , ifDefaultWarn = iface_warn }
  = do { cls <- fmap tyThingConClass (tcIfaceImplicit cls_name)
       ; tys' <- traverse tcIfaceType tys
       ; let warn = fmap fromIfaceWarningTxt iface_warn
       ; return ClassDefaults { cd_class = cls
                              , cd_types = tys'
                              , cd_provenance = DP_Imported this_mod
                              , cd_warn = warn } }
    where
       tyThingConClass :: TyThing -> Class
       tyThingConClass th = case tyConClass_maybe $ tyThingTyCon th of
                         Just cls -> cls
                         Nothing  -> pprPanic "tcIfaceDefault, expected class" (ppr th)

tcIfaceInst :: IfaceClsInst -> IfL ClsInst
tcIfaceInst (IfaceClsInst { ifDFun = dfun_name, ifOFlag = oflag
                          , ifInstCls = cls, ifInstTys = mb_tcs
                          , ifInstOrph = orph, ifInstWarn = iface_warn })
  = do { dfun <- forkM (text "Dict fun" <+> ppr dfun_name) $
                    fmap tyThingId (tcIfaceImplicit dfun_name)
       ; let mb_tcs' = map tcRoughTyCon mb_tcs
             warn = fmap fromIfaceWarningTxt iface_warn
       ; return (mkImportedClsInst cls mb_tcs' dfun_name dfun oflag orph warn) }

tcIfaceFamInst :: IfaceFamInst -> IfL FamInst
tcIfaceFamInst (IfaceFamInst { ifFamInstFam = fam, ifFamInstTys = mb_tcs
                             , ifFamInstAxiom = axiom_name
                             , ifFamInstOrph = orphan } )
    = do { axiom' <- forkM (text "Axiom" <+> ppr axiom_name) $
                     tcIfaceUnbranchedAxiom axiom_name
             -- will panic if branched, but that's OK
         ; let mb_tcs' = map tcRoughTyCon mb_tcs
         ; return (mkImportedFamInst fam mb_tcs' axiom' orphan) }

{-
************************************************************************
*                                                                      *
                Rules
*                                                                      *
************************************************************************

We move a IfaceRule from eps_rules to eps_rule_base when all its LHS free vars
are in the type environment.  However, remember that typechecking a Rule may
(as a side effect) augment the type envt, and so we may need to iterate the process.
-}

tcIfaceRules :: Bool            -- True <=> ignore rules
             -> [IfaceRule]
             -> IfL [CoreRule]
tcIfaceRules ignore_prags if_rules
  | ignore_prags = return []
  | otherwise    = mapM tcIfaceRule if_rules

tcIfaceRule :: IfaceRule -> IfL CoreRule
tcIfaceRule iface_rule@IfaceRule{ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
                        ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs,
                        ifRuleKind = kind, ifRuleOrph = orph}
  = do  { this_mod <- getIfModule
        ; ~(bndrs', args', rhs') <-
                -- Typecheck the payload lazily, in the hope it'll never be looked at
                forkM (text "Rule" <+> pprRuleName name) $
                bindIfaceBndrs bndrs                      $ \ bndrs' ->
                do { args'  <- mapM tcIfaceExpr args
                   ; rhs'   <- tcIfaceExpr rhs
                   ; let native = any hasNativeDependentUnfolding (rhs' : args' ++ map varToCoreExpr bndrs')
                   ; let checked = case kind of
                           IfaceErasedUserRule | isOrphan orph ->
                             Just (supportsImportedErasedRule, "Unsupported erased orphan rule shape.")
                           IfaceAnchoredErasedRule ->
                             Just (supportsImportedErasedRule, "Unsupported anchored erased rule shape.")
                           IfaceErasedUserRule -> Just (supportsErasedUserRule, "Unsupported erased user rule shape.")
                           IfaceRetainedUserRule -> Just (supportsLocalUserRule, "Unsupported retained function rule shape.")
                           IfaceImportedRetainedRule -> Just (supportsImportedUserRule, "Unsupported imported retained rule shape.")
                           IfaceRetainedAutoRule -> Just (canShareRetainedSpecRule, "Unsupported retained specialization shape.")
                           _ -> Nothing
                   ; case checked of
                       Just (accepts, shape_error) -> do
                         let (owner_ok, owner_error) = case kind of
                               IfaceErasedUserRule | isOrphan orph ->
                                 (maybe False (/= this_mod) (nameModule_maybe fn),
                                  "Expected an imported head for the orphan rule.")
                               IfaceAnchoredErasedRule ->
                                 (not (isOrphan orph) && maybe False (/= this_mod) (nameModule_maybe fn),
                                  "Expected a local anchor and an imported rule head.")
                               IfaceImportedRetainedRule ->
                                 (maybe False (/= this_mod) (nameModule_maybe fn), "Expected an imported retained rule head.")
                               _ -> (nameModule_maybe fn == Just this_mod,
                                     "Expected a rule head defined in this module.")
                         unless owner_ok $
                           failIfM (retained_doc $$ text owner_error)
                         thing <- tcIfaceGlobal fn
                         fun <- case thing of
                           AnId id -> pure id
                           _ -> failIfM (retained_doc $$ text "Expected an identifier at the rule head.")
                         let rule = mk_rule this_mod bndrs' args' rhs'
                         let has_evidence = case kind of
                               IfaceRetainedUserRule -> hasRetainedRuleHead fun
                               IfaceImportedRetainedRule -> hasRetainedRuleHead fun
                               _ -> native
                         unless (has_evidence && accepts fun rule) $
                           failIfM (retained_doc $$ text shape_error)
                         dflags <- getDynFlags
                         case lintRule (initLintConfig dflags []) fun rule of
                           Nothing -> pure ()
                           Just errs -> failIfM (retained_doc $$ vcat (bagToList errs))
                       Nothing -> do
                         when native $
                           failIfM (text "Native dependent interface rules are not supported:" <+> pprRuleName name)
                         when (kind == IfaceUserRule) $ do
                           head_thing <- tcIfaceGlobal fn
                           case head_thing of
                             AnId fun | hasRetainedRuleHead fun ->
                               failIfM (text "Missing retained function rule evidence:" <+> pprRuleName name)
                             _ -> pure ()
                   ; when (isNothing checked) $ whenGOptM Opt_DoCoreLinting $ do
                      { dflags <- getDynFlags
                      ; (_, lcl_env) <- getEnvs
                      ; let in_scope :: [Var]
                            in_scope = ((nonDetEltsUFM $ if_tv_env lcl_env) ++
                                        (nonDetEltsUFM $ if_id_env lcl_env) ++
                                        bndrs' ++
                                        exprsFreeIdsList args')
                      ; case lintExpr (initLintConfig dflags in_scope) rhs' of
                          Nothing   -> return ()
                          Just errs -> do
                            logger <- getLogger
                            liftIO $ displayLintResults logger doc
                                               (pprCoreExpr rhs')
                                               (emptyBag, errs) }
                   ; return (bndrs', args', rhs') }
        ; let (bndrs_occ, rhs_occ) = occurAnalyseBndrsAndExpr bndrs' rhs'
                   -- See (OUR1) in Note [OccInfo in unfoldings and rules]
        ; return (mk_rule this_mod bndrs_occ args' rhs_occ) }
  where
    retained_doc = case kind of
      IfaceAnchoredErasedRule -> text "Invalid anchored erased package rule:" <+> pprRuleName name
      IfaceErasedUserRule -> text "Invalid erased package rule:" <+> pprRuleName name
      IfaceRetainedUserRule -> text "Invalid retained function rule:" <+> pprRuleName name
      IfaceImportedRetainedRule -> text "Invalid imported retained function rule:" <+> pprRuleName name
      _ -> text "Invalid retained specialization rule:" <+> pprRuleName name

    mk_rule this_mod bndrs' args' rhs' =
      Rule { ru_name   = name
           , ru_fn     = fn
           , ru_act    = act
           , ru_bndrs  = bndrs'
           , ru_args   = args'
           , ru_rhs    = rhs'
           , ru_rough  = map ifTopFreeName args
           , ru_origin = this_mod
           , ru_orphan = orph
           , ru_auto   = ifRuleAuto iface_rule
           , ru_local  = False }
              -- ru_local=False: an imported RULE is never for a local Id or, even if
              -- it is (module loop, perhaps) we'll just leave it in the non-local set
        -- This function *must* mirror exactly what Rules.roughTopNames does
        -- We could have stored the ru_rough field in the iface file
        -- but that would be redundant, I think.
        -- The only wrinkle is that we must not be deceived by
        -- type synonyms at the top of a type arg.  Since
        -- we can't tell at this point, we are careful not
        -- to write them out in coreRuleToIfaceRule
    ifTopFreeName :: IfaceExpr -> Maybe Name
    ifTopFreeName (IfaceType (IfaceTyConApp tc _ )) = Just (ifaceTyConName tc)
    ifTopFreeName (IfaceType (IfaceTupleTy s _ ts)) = Just (tupleTyConName s (length (appArgsIfaceTypes ts)))
    ifTopFreeName (IfaceApp f _)                    = ifTopFreeName f
    ifTopFreeName (IfaceExt n)                      = Just n
    ifTopFreeName _                                 = Nothing

    doc = text "Unfolding of" <+> ppr name

{-
************************************************************************
*                                                                      *
                Annotations
*                                                                      *
************************************************************************
-}

tcIfaceAnnotations :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceAnnotations = mapM tcIfaceAnnotation

tcIfaceAnnotation :: IfaceAnnotation -> IfL Annotation
tcIfaceAnnotation (IfaceAnnotation target serialized) = do
    target' <- tcIfaceAnnTarget target
    return $ Annotation {
        ann_target = target',
        ann_value = serialized
    }

tcIfaceAnnTarget :: IfaceAnnTarget -> IfL (AnnTarget Name)
tcIfaceAnnTarget (NamedTarget occ) =
    NamedTarget <$> lookupIfaceTop occ
tcIfaceAnnTarget (ModuleTarget mod) =
    return $ ModuleTarget mod

{-
************************************************************************
*                                                                      *
                Complete Match Pragmas
*                                                                      *
************************************************************************
-}

tcIfaceCompleteMatches :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
tcIfaceCompleteMatches = mapM tcIfaceCompleteMatch

tcIfaceCompleteMatch :: IfaceCompleteMatch -> IfL CompleteMatch
tcIfaceCompleteMatch (IfaceCompleteMatch ms mtc) =
  return $ CompleteMatch (mkUniqDSet ms) mtc

{- Note [Positioning of forkM]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to be lazy when type checking the interface, since these functions are
called when the interface itself is being loaded, which means it is not in the
PIT yet. In particular, the `tcIfaceTCon` must be inside the forkM, otherwise
we'll try to look it up the TyCon, find it's not there, and so initiate the
process (again) of loading the (very same) interface file. Result: infinite
loop. See #19744.
-}

{-
************************************************************************
*                                                                      *
                        Types
*                                                                      *
************************************************************************
-}

-- | Derive a scalar declaration view without decoding constructor fields.
-- See Note [Checking native domain shapes].
tcIfaceDataDomainShape :: [IfaceTyConBinder] -> IfaceContext -> IfaceTyConParent
                       -> IfaceConDecls -> IfL NativeDomainShape
tcIfaceDataDomainShape binders ctxt parent cons
  | null binders, null ctxt, IfNoParent <- parent
  , IfDataTyCon False cs <- cons
  , all ordinary cs
  = NativeDataDomain <$> mapM field (concatMap ifConArgTys cs)
  | otherwise = return NativeInvalidDomain
  where
    ordinary con = null (ifConUnivTvs con) && null (ifConExTCvs con)
                && null (ifConEqSpec con) && null (ifConCtxt con)
                && null (ifConArgBinders con)
    field (mult, ty) = (,) <$> tcIfaceDomainShape mult <*> tcIfaceDomainShape ty

-- | Project raw syntax without forcing a decoded Type or a referenced TyCon.
-- See Note [Checking native domain shapes].
tcIfaceDomainShape :: IfaceType -> IfL NativeDomainShape
tcIfaceDomainShape (IfaceTyConApp tc IA_Nil)
  = NativeTyConDomain <$> forkM (text "Native domain shape") (tcIfaceTyCon tc)
tcIfaceDomainShape (IfaceTupleTy BoxedTuple NotPromoted IA_Nil)
  = return (NativeTyConDomain unitTyCon)
tcIfaceDomainShape (IfaceFunTy FTF_T_T ma mult arg res)
  = NativeFunctionDomain <$> tcIfaceDomainShape ma
                         <*> tcIfaceDomainShape mult
                         <*> tcIfaceDomainShape arg <*> tcIfaceDomainShape res
tcIfaceDomainShape (IfaceDepFunTy _vis (mult, _, arg) res)
  = NativePiDomain <$> tcIfaceDomainShape mult
                   <*> tcIfaceDomainShape arg
                   <*> tcIfaceDomainShape res
tcIfaceDomainShape IfaceExistsTy{} = return NativeInvalidDomain
tcIfaceDomainShape IfaceForAllTy{} = return NativeInvalidDomain
tcIfaceDomainShape IfaceLitTy{} = return NativeInvalidDomain
tcIfaceDomainShape _ = return NativeUnknownDomain

-- | Select the existing eligibility policy for a shape precheck.
data NativeDomainUse = NativeScalarUse | NativeFunctionUse | NativeStructuralFunctionUse
                     | NativeRetainedUse | NativeOpaqueUse
                     | NativeFunctionEndpointUse
                     | NativeFieldMultUse | NativeManyMultUse | NativeUnmatchableUse
  deriving Eq

-- | Project an available Type head without expanding synonyms.
-- See Note [Checking native domain shapes].
nativeTypeDomainShape :: Type -> NativeDomainShape
nativeTypeDomainShape (TyConApp tc []) = NativeTyConDomain tc
nativeTypeDomainShape (FunTy FTF_T_T ma mult arg res)
  = NativeFunctionDomain (nativeTypeDomainShape ma)
                         (nativeTypeDomainShape mult)
                         (nativeTypeDomainShape arg) (nativeTypeDomainShape res)
nativeTypeDomainShape FunTy{} = NativeInvalidDomain
nativeTypeDomainShape (DepFunTy _vis binder body)
  = NativePiDomain (nativeTypeDomainShape (idMult binder))
                   (nativeTypeDomainShape (varType binder))
                   (nativeTypeDomainShape body)
nativeTypeDomainShape _ = NativeUnknownDomain

-- | Reject known unsupported shapes; True still requires the typed checks.
-- See Note [Checking native domain shapes].
nativeDomainShapePossible :: NativeDomainUse -> NativeDomainShape -> Bool
nativeDomainShapePossible use shape = go emptyTyConSet emptyTyConSet use shape
  where
    -- See Note [Opaque native interface values].
    go seen aliases NativeOpaqueUse (NativeTyConDomain tc)
      | Just (NativeSynonymDomain rhs) <- tyConNativeDomainShape tc
      = not (tc `elemTyConSet` aliases)
        && go seen (extendTyConSet aliases tc) NativeOpaqueUse rhs
      | otherwise = True
    go _ _ NativeOpaqueUse _ = True
    go seen aliases NativeRetainedUse shape
      = go seen aliases NativeScalarUse shape
        || go seen aliases NativeStructuralFunctionUse shape
    go seen aliases mode (NativeTyConDomain tc)
      | mode == NativeUnmatchableUse, tc == unmatchableDataConTyCon = True
      | mode == NativeUnmatchableUse, tc == matchableDataConTyCon = False
      | mode == NativeFieldMultUse || mode == NativeManyMultUse
      , tc == manyDataConTyCon = True
      | mode == NativeFieldMultUse, tc == oneDataConTyCon = True
      | mode == NativeManyMultUse, tc == oneDataConTyCon = False
      | Just view <- tyConNativeDomainShape tc
      = case view of
          NativeSynonymDomain rhs
            | tc `elemTyConSet` aliases -> False
            | otherwise -> go seen (extendTyConSet aliases tc) mode rhs
          NativeDataDomain{}
            -> data_use mode
               && (tc `elemTyConSet` seen
                   || go (extendTyConSet seen tc) emptyTyConSet mode view)
          _ -> go seen aliases mode view
      | otherwise = True
    go _ _ _ NativeUnknownDomain = True
    go _ _ _ NativeInvalidDomain = False
    go _ aliases mode (NativePiDomain mult arg res)
      = function_use mode
        && go emptyTyConSet aliases NativeManyMultUse mult
        && go emptyTyConSet aliases (function_endpoint_use mode) arg
        && go emptyTyConSet aliases (function_endpoint_use mode) res
    go seen _ mode (NativeDataDomain fields)
      = data_use mode && all field fields
      where
        field (mult, ty) = go seen emptyTyConSet NativeFieldMultUse mult
                       && go seen emptyTyConSet field_use ty
        field_use | mode == NativeScalarUse = NativeRetainedUse
                  | otherwise = NativeFunctionEndpointUse
    go seen aliases mode (NativeFunctionDomain ma mult arg res)
      = function_use mode
        && go seen aliases NativeUnmatchableUse ma
        && go seen aliases NativeManyMultUse mult
        && go emptyTyConSet aliases (function_endpoint_use mode) arg
        && go emptyTyConSet aliases (function_endpoint_use mode) res
    go seen aliases mode (NativeSynonymDomain rhs) = go seen aliases mode rhs

    data_use mode = mode == NativeScalarUse || mode == NativeFunctionEndpointUse
    function_use mode = mode == NativeFunctionUse || mode == NativeStructuralFunctionUse
    function_endpoint_use NativeFunctionUse = NativeOpaqueUse
    function_endpoint_use _ = NativeFunctionEndpointUse

-- | Preserve the Id guards before inspecting its imported domain view.
nativeIdDomainShapePossible :: NativeDomainUse -> Id -> Bool
nativeIdDomainShapePossible use v
  | not (isLocalId v && not (isCoVar v) && isManyTy (idMult v)) = True
  | use == NativeRetainedUse
  = nativeDomainShapePossible NativeScalarUse shape
    || not function_guards
    || nativeDomainShapePossible NativeStructuralFunctionUse shape
  | use == NativeFunctionUse, not function_guards = True
  | otherwise = nativeDomainShapePossible use shape
  where
    shape = nativeTypeDomainShape (varType v)
    function_guards = not (isJoinIdDetails (idDetails v))
                   && noFreeVarsOfType (idMult v)

-- | Report an early domain rejection without printing unfinished Types.
-- See Note [Checking native domain shapes].
precheckIfaceNativeDomain :: SDoc -> Bool -> IfL ()
precheckIfaceNativeDomain name possible
  = do { valid <- evaluateIfaceNativeDomain name possible
       ; unless valid $ failIfM (name $$ text "Unsupported retained domain shape.") }

{- Note [Checking native domain shapes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Inspect the raw view before coreFullView or constructor-field decoding. Data
back-edges are valid, but each queued field must pass. Keep a separate active
synonym stack. Reset it on data edges: Alias = A; data A = MkA Alias is valid.
An alias-only cycle is not a supported domain.

For a generic Pi, check the raw multiplicity and domain before bindIfaceId.
Its mkLocalIdOrCoVar call expands aliases to choose an Id or a CoVar. An Id
precheck alone would reach an alias-only cycle after that expansion had begun.
Keep the Id precheck and the typed checks after allocation as well.

Fields permit One or Many. Function arrows and retained binders require Many.
Function arrows require U matchability. Resolve metadata by wired TyCon
identity, including nullary aliases.
An algebraic domain can contain a function field. Check each endpoint of
that field with the function-free algebraic policy. Start a separate data
visited set for each endpoint. An enclosing data back-edge must not certify
that endpoint. For a lexical call head, both endpoints can be opaque lifted
types. The argument to the call must have the declared domain as its kind.
A raw Pi is not scalar. mkDepFunTyWithVis can convert a vacuous visible Pi
to an ordinary arrow. A hidden Pi remains retained. Keep the domain projection
conservative for both flags. Check the multiplicity, argument and result
before decoding its binder. This rejects recursive structural domains
through a Pi without forcing their Type knots.
Applied synonyms and casts remain
unknown. The precheck must not reject their possible normalization results.
Leave constraint arrows unknown so that their decoders check scoped indices
in predicates before the typed domain check rejects the arrow visibility.
Leave raw variable and call-index forms unknown so that their decoders check
lexical scope.

True means only that this view cannot reject the domain. Run the existing
typed predicate afterward, inside the exception boundary. No unchecked
Type escapes. Keep the Id guards before domain inspection. The raw named-field
precheck runs before decoding that field; the existing field checks still run
in their original order. Parent kinds and binder headers must already exist.
This is not a termination guarantee for those headers or unclassified forms.
See Note [Native interface domain shapes] in GHC.Core.TyCon.
-}

{- Note [Opaque native interface values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A runtime index can name an opaque value at any lifted value type. Do not
traverse its constructors. An Int value does not require the unlifted field
of I# to be an index. A generic cons head keeps its instantiated field type.
Core formation also permits generated dependent lambdas over these values.
Source foreach domains have a separate structural check in GHC.Tc.Gen.HsType.

The raw opaque precheck rejects nullary alias cycles before bindIfaceId can
expand them. Other raw shapes require the typed lifted-value check. Keep the
stricter structural and function-endpoint prechecks at their existing callers.
Opaque identity does not grant native call permission.

Check promoted constructor applications that contain runtime indices against
their complete structural parent and instantiated fields. Checking each local
Id alone would permit an Int cons to contain a Bool head. Ordinary promoted
applications without runtime dependencies keep their existing import path.
-}

tcIfaceType :: IfaceType -> IfL Type
tcIfaceType = go
  where
    go (IfaceTyVar n)            = TyVarTy <$> tcIfaceTyVar n
    go (IfaceFreeTyVar n)        = pprPanic "tcIfaceType:IfaceFreeTyVar" (ppr n)
    go (IfaceFreeTermVar v)
      = failIfM (text "Cannot import a diagnostic-only retained index:" <+> ppr v)
    go (IfaceFreeTermAppTy fun _)
      = failIfM (text "Cannot import a diagnostic-only retained call head:" <+> ppr fun)
    go (IfaceTermAppTy fun arg)
      -- See Note [Opaque call indices in interfaces] in GHC.Iface.Type.
      = do { f <- tcIfaceLclId fun
           ; precheckIfaceNativeDomain
               (text "Invalid retained call head in interface:" <+> ppr fun)
               (nativeIdDomainShapePossible NativeFunctionUse f)
           ; fun_tys <- evaluateIfaceNativeDomain (ppr fun)
                          (nativeIndexFunctionTys_maybe f)
           ; case fun_tys of
               Nothing ->
                 failIfM (text "Invalid retained call head in interface:" <+> ppr fun
                          $$ text "Expected a local unrestricted function with lifted argument and result types."
                          $$ text "Actual type:" <+> ppr (varType f)
                          $$ text "Actual multiplicity:" <+> ppr (idMult f))
               Just (domain, _) ->
                 do { arg' <- go arg
                    ; unless (typeKind arg' `eqType` domain) $
                        failIfM (text "Invalid retained call argument in interface for:" <+> ppr fun
                                 $$ text "Expected kind:" <+> ppr domain
                                 $$ text "Actual kind:" <+> ppr (typeKind arg'))
                    ; return (TermAppTy f arg') } }
    go (IfaceTermVar n)
      = do { v <- tcIfaceLclId n
           ; precheckIfaceNativeDomain
               (text "Invalid retained index in interface:" <+> ppr n)
               (nativeIdDomainShapePossible NativeOpaqueUse v)
           ; valid <- evaluateIfaceNativeDomain (ppr n) (isNativeOpaqueValueId v)
           ; unless valid $
               failIfM (text "Invalid retained index in interface:" <+> ppr n
                        $$ text "Expected a local identifier with multiplicity Many"
                        $$ text "at a lifted value type."
                        $$ text "Actual type:" <+> ppr (varType v)
                        $$ text "Actual multiplicity:" <+> ppr (idMult v))
           ; return (TermVarTy v) }
    go (IfaceExistsTy flag bndr@(name, _) body)
      = bindIfaceTyVar bndr $ \tv -> do
          check_exists_kind name (text "binder kind") (typeKind (varType tv))
          body' <- go body
          check_exists_kind name (text "payload") (typeKind body')
          return (ExistsTy flag tv body')
    go (IfaceDepDictTy bndr@(_, name, _) body)
      = bindIfaceId bndr $ \dict -> do
          unless (isNativeClassDictionaryId dict) $
            failIfM (text "Invalid scoped dictionary binder in interface:" <+> ppr name)
          body' <- go body
          unless (typeKind body' `eqType` liftedTypeKind) $
            failIfM (text "Expected a lifted scoped dictionary result in interface:" <+> ppr name)
          return (DepDictTy dict body')
    go (IfaceClassAppTy method inst (IfaceTermVar dict) (IfaceTermVar arg))
      = do selector <- tcIfaceExtId method
           inst' <- go inst
           dict' <- tcIfaceLclId dict
           arg' <- tcIfaceLclId arg
           case nativeClassSelectorIndex (mkApps (Var selector) [Type inst', Var dict', Var arg']) of
             Just index -> return index
             Nothing -> failIfM (text "Invalid scoped class selector in interface:" <+> ppr method)
    go IfaceClassAppTy{}
      = failIfM (text "Scoped class selector in interface requires two lexical references")
    go (IfaceDepFunTy vis bndr@(mult, name, domain) body)
      -- See Note [Retained binders in interfaces].
      -- See Note [Exported function-valued retained signatures] in GHC.Tc.Gen.HsType.
      = do { mult_shape <- tcIfaceDomainShape mult
           ; domain_shape <- tcIfaceDomainShape domain
           ; precheckIfaceNativeDomain
               (text "Invalid retained binder in interface:" <+> ppr name)
               (nativeDomainShapePossible NativeManyMultUse mult_shape
                && nativeDomainShapePossible NativeOpaqueUse domain_shape)
           ; bindIfaceId bndr $ \ v ->
        do { precheckIfaceNativeDomain
               (text "Invalid retained binder in interface:" <+> ppr name)
               (nativeIdDomainShapePossible NativeOpaqueUse v)
           ; valid <- evaluateIfaceNativeDomain (ppr name) (isNativeOpaqueValueId v)
           ; unless valid $
               failIfM (text "Invalid retained binder in interface:" <+> ppr v
                        $$ text "Expected a local identifier with multiplicity Many"
                        $$ text "at a lifted value type."
                        $$ text "Actual type:" <+> ppr (varType v)
                        $$ text "Actual multiplicity:" <+> ppr (idMult v))
           ; body' <- go body
           ; unless (typeKind body' `eqType` liftedTypeKind) $
               failIfM (text "Invalid retained function result in interface for:" <+> ppr v
                        $$ text "Expected kind Type."
                        $$ text "Actual kind:" <+> ppr (typeKind body'))
           ; return (mkDepFunTyWithVis vis v body') } }
    go (IfaceLitTy l)            = LitTy <$> tcIfaceTyLit l
    go (IfaceFunTy flag ma w t1 t2) = FunTy flag <$> go ma <*> go w <*> go t1 <*> go t2
    go (IfaceTupleTy s i tks)    = tcIfaceTupleTy s i tks
    go (IfaceAppTy t ts)
      = do { t'  <- go t
           ; foldlM apply_arg t' (appArgsIfaceTypes ts) }
    go (IfaceTyConApp tc tks)
      = do { tc' <- tcIfaceTyCon tc
           ; let args = appArgsIfaceTypes tks
           ; tks' <- mapM go args
           ; case (tks, tks') of
               (IA_Arg _ (IfaceForAllTyFlag vis) _, arg : _) ->
                 checkImportedMatchabilityPrefix tc' $
                   case vis of
                     Inferred -> isMatchabilityArgument arg
                     _ -> False
               _ -> return ()
           ; case isPromotedDataCon_maybe tc' of
               Just con
                 -- See Note [Runtime arguments of promoted GADT constructors].
                 | let (_, _, eqs, theta, _, _) = dataConFullSig con
                       indexed = any (anyFreeVarsOfType isId) tks' && not (null eqs) && null theta
                 , not (null (dataConRepArgBinders con)) || indexed ->
                   check_iface_retained_args (ppr tc) (tyConKind tc') tks'
                 | isJust (tyConDataFamCoercion_maybe (dataConTyCon con))
                 , let (tys, fields) = splitAt (length (dataConUserTyVars con)) tks'
                 , any (anyFreeVarsOfType isId) fields ->
                 do { valid <- evaluateIfaceNativeDomain (ppr tc) $
                          isJust (nativeFamilyPromotedCon_maybe con tys fields)
                      ; unless valid $
                          failIfM (text "Invalid retained data family constructor fields in interface:"
                                   <+> ppr tc) }
                 | let (tys, fields) = splitAt (length (dataConUnivTyVars con)) tks'
                 , any (anyFreeVarsOfType isId) fields ->
                 do { valid <- evaluateIfaceNativeDomain (ppr tc) $
                          isJust (nativeIndexDataConTyArgs (mkTyConApp (dataConTyCon con) tys) con)
                          && nativeIndexArgKinds con tys fields
                      ; unless valid $
                          failIfM (text "Invalid retained constructor fields in interface:"
                                   <+> ppr tc) }
               _ | any (call_operand False) args ->
                     check_call_arg_kinds (ppr tc) (tyConKind tc') tks'
               _ -> return ()
           ; return (mkTyConApp tc' tks') }
    go (IfaceForAllTy ma bndr t)
      = do { ma' <- go ma
           ; checkIfaceForAllMatchability ma'
           ; bindIfaceForAllBndr bndr $ \ tv' vis ->
        ForAllTy ma' (Bndr tv' (ifaceForAllTyFlag vis)) <$> go t }
    go (IfaceCastTy ty co)
      = do { ty' <- go ty
           ; co' <- tcIfaceCo co
           ; case ty' of
               TermVarTy f | isNativeIndexFunctionId f ->
                 do { valid <- evaluateIfaceNativeDomain (ppr f) $
                        coercionRole co' == Nominal
                        && coercionLKind co' `eqType` varType f
                        && coercionRKind co' `eqType` varType f
                        && isJust (nativeIndexFunctionTypeTys_maybe (coercionRKind co'))
                    ; unless valid $
                        failIfM (text "A retained function-value cast changes its fixed domain:"
                                 <+> ppr f)
                    ; return (mkCastTy ty' co') }
               _ -> return (CastTy ty' co') }
    go (IfaceCoercionTy co)  = CoercionTy <$> tcIfaceCo co

    -- A raw ordinary application can expose a native function-value head.
    -- Check its argument before the smart maker forms TermAppTy.
    app fun@(TermVarTy f) arg
      = do { domains <- evaluateIfaceNativeDomain (ppr f)
                          (nativeIndexFunctionTys_maybe f)
           ; case domains of
               Just (domain, _) | typeKind arg `eqType` domain ->
                 return (mkAppTy fun arg)
               _ -> failIfM (text "Invalid retained function-value application in interface:"
                             <+> ppr f) }
    app TermAppTy{} _
      = failIfM (text "A retained call result cannot be applied in an interface.")
    app fun@(TyConApp tc _) arg
      | Just con <- isPromotedDataCon_maybe tc
      , not (null (dataConRepArgBinders con))
      = do { check_iface_retained_args (ppr tc) (typeKind fun) [arg]
           ; return (mkAppTy fun arg) }
    app fun arg = return (mkAppTy fun arg)

    apply_arg fun raw = do { arg <- go raw
                           ; when (call_operand False raw) $
                               check_call_arg_kinds (ppr fun) (typeKind fun) [arg]
                           ; app fun arg }

    -- A call can also be encoded by applying a lexical term variable.
    -- Inspect its serialized head without forcing ordinary decoded types.
    call_operand _ IfaceTermAppTy{} = True
    call_operand applied IfaceTermVar{} = applied
    call_operand applied (IfaceCastTy ty _) = call_operand applied ty
    call_operand _ (IfaceAppTy fun _) = call_operand True fun
    call_operand _ _ = False

    -- See Note [Opaque call indices in interfaces] in GHC.Iface.Type.
    check_call_arg_kinds name = check_iface_arg_kinds name
      (text "Invalid type application containing a retained call in interface:" <+> name)

    -- An interface can encode the same application in one TyConApp or
    -- append arguments using AppTy. Both forms use the promoted telescope.
    -- Stop at the end of the supplied arguments, so valid prefixes remain
    -- valid. Each preceding argument determines the next dependent kind.
    check_iface_retained_args name = check_iface_arg_kinds name
      (text "Invalid retained constructor fields in interface:" <+> name)
    check_iface_arg_kinds name diagnostic kind args
      = do { failure <- evaluateIfaceNativeDomain name (check kind args)
           ; forM_ failure $ \reason -> failIfM (diagnostic $$ reason) }
      where
        check _ [] = Nothing
        check kind (arg : rest)
          | Just (_, binder, _) <- splitPiTy_maybe kind
          , let expected = piTyBinderType binder
                actual = typeKind arg
          = if actual `eqType` expected
            then check (piResultTy kind arg) rest
            else Just (text "Expected argument kind:" <+> ppr expected
                       $$ text "Actual argument kind:" <+> ppr actual)
          | otherwise = Just (text "The type has no binder for this argument.")

-- | Check a forall annotation before its binder enters scope.
-- See Note [Forall interface metadata] in GHC.Iface.Type.
checkIfaceForAllMatchability :: Type -> IfL ()
checkIfaceForAllMatchability ma
  = unless (typeKind ma `eqType` matchabilityTy) $
      failIfM (text "Invalid forall matchability in interface:" <+> ppr ma
               $$ text "Expected kind Matchability.")

{- Note [Runtime arguments of promoted GADT constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DataKinds gives 'VCons a telescope with an element type a, a tail length n,
a head of kind a, and a tail of kind Vec a n. The result has kind Vec a (S n).
The constructor can occur in an exported type with retained values for its
head and tail. Check them against the promoted telescope, substituting each
argument before checking the next one. Splitting arguments at the number
of parent parameters cannot recover the tail length from the result index.

This checks an application in a type. Deriving evidence from a case expression
requires the constructor and fields of the actual scrutinee.
-}

-- | Check the exact lifted kind without printing a possibly recursive Type.
-- See Note [Positioning of forkM] and Note [Detected recursive native interface domains].
check_exists_kind :: IfLclName -> SDoc -> Kind -> IfL ()
check_exists_kind name part kind = do
    valid <- evaluateIfaceValidation
      (text "Cannot validate existential type in interface:" <+> ppr name)
      (kind `eqType` liftedTypeKind)
    unless valid $
      failIfM (text "Existential" <+> part <+> text "must have kind Type:" <+> ppr name)

-- | Evaluate native interface eligibility and report detected recursive knots.
-- See Note [Detected recursive native interface domains].
evaluateIfaceNativeDomain :: SDoc -> a -> IfL a
evaluateIfaceNativeDomain name =
  evaluateIfaceValidation (text "Cannot validate retained type in interface:" <+> name)

evaluateIfaceValidation :: SDoc -> a -> IfL a
evaluateIfaceValidation description result
  = do { checked <- liftIO (try (evaluate result))
       ; case checked of
           Left NonTermination ->
             failIfM (description $$ text "Validation encountered recursive type information.")
           Right value -> return value }

{- Note [Detected recursive native interface domains]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The native domain predicate follows algebraic constructor fields. A generic
IfaceDepFunTy in an ordinary field can force this predicate before its field
Type exists. Following that field again can raise NonTermination. The visited
TyCon set handles available recursive field Types, not an active decoder.

Keep validation before returning the decoded Type. Evaluate each existing
eligibility result inside IO and catch only NonTermination. Bool guards and
the outer Maybe constructor force the native domain checks. Preserve their
order. On a detected knot, fail through the existing interface error path.
Do not return a placeholder Type, skip a check, or retry the active thunk.
Use the encoded name in the diagnostic; printing a recursive field Type can
force the same failed computation. Other exceptions propagate unchanged.

This boundary improves diagnostics. It does not stage the loader or guarantee
termination. The runtime does not guarantee detection of every loop. A
shape-first loader needs independent declaration and synonym information.
Merely deferring validation into Type children is insufficient: typeKind can
inspect a Type head without forcing those children. See Note [Retained binders
in interfaces], Note [Checking native domain shapes] and Note [Positioning of forkM].
-}

{- Note [Retained binders in interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained binder is a runtime Id, not a TyVar or an erased forall binder.
bindIfaceId allocates a fresh VarName and Unique. It decodes the domain and
multiplicity outside the binder scope, then extends the local Id environment
for the body. Each IfaceTermVar occurrence looks up that same fresh Id.

Check the algebraic index domain and Many multiplicity before decoding the
body. Check each index occurrence too, since the local Id environment also
contains ordinary Ids and CoVars. The result must have kind Type, not
Constraint. Use eqType: isLiftedTypeKind also accepts Constraint. Malformed
native interface data must fail before construction of a retained type.

Domain validation can force constructor argument types. Keep it within the
existing deferred declaration decoding. See Note [Positioning of forkM].
The domain predicate tracks visited TyCons before it follows their fields.
Thus a recursive Nat domain does not require an infinite field traversal.
An unfinished field decoder is a separate case. See Note [Detected recursive
native interface domains] for the diagnostic boundary around eligibility.

IfaceFreeTermVar preserves an open Id only for diagnostics. It cannot enter
this decoding path. Boolean branch proof reconstruction is described in
Note [Native constructor evidence in interfaces]. Module unfolding generation
remains separately gated.
-}

{- Note [Imported matchability prefixes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An upgraded provider can add a leading inferred Matchability parameter
to a type family. An old client still refers to the same family Name.
For an applied family, check the first recorded argument. A partial
application can omit trailing arguments.

A bare alias such as @type Saved = K@ has no argument to inspect.
Its interface records the result kind at the time Saved was compiled.
Compare that kind with the current kind of K. A polymorphic bare alias
already has the new telescope. An old monomorphic alias needs a rebuild.
Keep this comparison in the deferred RHS action. Loading the kind of
Saved can participate in the import cycle in Note [Synonym kind loop].
-}

-- | Check a client's view of an imported type constructor's prefix.
checkImportedMatchabilityPrefix :: TyCon -> Bool -> IfL ()
checkImportedMatchabilityPrefix tc valid_prefix
  | not (isWiredInName name) = do
      owner <- getIfModule
      when (nameModule name /= owner) $
        case tyConBinders tc of
          b : _ | isInferredTyConBinder b
                , tyVarKind (binderVar b) `eqType` matchabilityTy
                , not valid_prefix ->
            failIfM (text "Incompatible interface dependency:" <+> ppr name
                     $$ text "An inferred matchability argument is missing."
                     $$ text "Rebuild the module that refers to it:" <+> ppr owner)
          _ -> return ()
  | otherwise = return ()
  where
    name = tyConName tc

isMatchabilityArgument :: Type -> Bool
isMatchabilityArgument arg = typeKind arg `eqType` matchabilityTy

tcIfaceTupleTy :: TupleSort -> PromotionFlag -> IfaceAppArgs -> IfL Type
tcIfaceTupleTy sort is_promoted args
 = do { args' <- tcIfaceAppArgs args
      ; let arity = length args'
      ; base_tc <- tcTupleTyCon True sort arity
      ; case is_promoted of
          NotPromoted
            -> return (mkTyConApp base_tc args')

          IsPromoted
            -> do { let tc        = promoteDataCon (tyConSingleDataCon base_tc)
                        kind_args = map typeKind args'
                  ; return (mkTyConApp tc (kind_args ++ args')) } }

-- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
tcTupleTyCon :: Bool    -- True <=> typechecking a *type* (vs. an expr)
             -> TupleSort
             -> Arity   -- the number of args. *not* the tuple arity.
             -> IfL TyCon
tcTupleTyCon in_type sort arity
  = case sort of
      ConstraintTuple -> return (cTupleTyCon arity)
      BoxedTuple      -> return (tupleTyCon Boxed   arity)
      UnboxedTuple    -> return (tupleTyCon Unboxed arity')
        where arity' | in_type   = arity `div` 2
                     | otherwise = arity
                      -- in expressions, we only have term args

tcIfaceAppArgs :: IfaceAppArgs -> IfL [Type]
tcIfaceAppArgs = mapM tcIfaceType . appArgsIfaceTypes

-----------------------------------------
tcIfaceCtxt :: IfaceContext -> IfL ThetaType
tcIfaceCtxt sts = mapM tcIfaceType sts

-----------------------------------------
tcIfaceTyLit :: IfaceTyLit -> IfL TyLit
tcIfaceTyLit (IfaceNumTyLit n) = return (NumTyLit n)
tcIfaceTyLit (IfaceIntPrimTyLit n) = return (IntPrimTyLit n)
tcIfaceTyLit (IfaceStrTyLit n) = return (StrTyLit (getLexicalFastString n))
tcIfaceTyLit (IfaceCharTyLit n) = return (CharTyLit n)

{-
%************************************************************************
%*                                                                      *
                        Coercions
*                                                                      *
************************************************************************
-}

tcIfaceCo :: IfaceCoercion -> IfL Coercion
tcIfaceCo = go
  where
    go_mco IfaceMRefl    = pure MRefl
    go_mco (IfaceMCo co) = MCo <$> (go co)

    go (IfaceReflCo t)           = Refl <$> tcIfaceType t
    go (IfaceGReflCo r t mco)    = GRefl r <$> tcIfaceType t <*> go_mco mco
    go (IfaceFunCo r ma w c1 c2) = mkFunCoNoFTF r <$> go ma <*> go w <*> go c1 <*> go c2
    go (IfaceDepFunCo visL visR bndr@(_, name, _) domain body)
      = do { domain' <- go_mco domain
           ; case domain' of
               MRefl -> pure ()
               MCo delta -> unless (coercionRole delta == Nominal) $
                 failIfM (text "Invalid retained domain coercion in interface for:" <+> ppr name
                          $$ text "Expected nominal evidence." $$ ppr delta)
           ; bindIfaceId bndr $ \ bndr' ->
        do { body' <- go body
           ; unless (coercionRole body' /= Nominal || visL == visR) $
               failIfM (text "Invalid retained function coercion in interface for:" <+> ppr name
                        $$ text "Nominal evidence requires identical visibilities.")
           ; return (DepFunCo visL visR bndr' domain' body') } }
    go (IfaceTermAppCo {})
      = failIfM (text "Opaque runtime call congruence interfaces are not implemented.")
    go (IfaceTyConAppCo r tc cs)
      = do { tc' <- tcIfaceTyCon tc
           ; cs' <- mapM go cs
           ; case cs' of
               co : _ -> checkImportedMatchabilityPrefix tc'
                           (isMatchabilityArgument (coercionLKind co) &&
                            isMatchabilityArgument (coercionRKind co))
               _ -> return ()
           ; return (TyConAppCo r tc' cs') }
    go (IfaceAppCo c1 c2)        = AppCo <$> go c1 <*> go c2
    go (IfaceForAllCo tcv visL visR ma k co)
      = do { ma' <- go ma
           ; unless (coercionRole ma' == Nominal) $
               failIfM (text "Invalid forall matchability coercion in interface:"
                        $$ text "Expected a nominal coercion." $$ ppr ma')
           ; checkIfaceForAllMatchability (coercionLKind ma')
           ; checkIfaceForAllMatchability (coercionRKind ma')
           ; k' <- go_mco k
           ; bindIfaceBndr tcv $ \ tv' ->
        do { co' <- go co
           ; return (ForAllCo { fco_tcv = tv', fco_visL = ifaceForAllTyFlag visL, fco_visR = ifaceForAllTyFlag visR
                              , fco_ma = ma', fco_kind = k', fco_body = co' }) } }
    go (IfaceCoVarCo n)           = CoVarCo <$> go_var n
    go (IfaceUnivCo p r t1 t2 ds) =
      do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2
         ; cvs <- mapM go_dep ds
         ; return $
             UnivCo
               { uco_prov = p, uco_role = r
               , uco_lty = t1', uco_rty = t2'
                 -- No coercion holes in interface files, as per
                 -- Note [Holes in IfaceCoercion] in GHC.Iface.Type
               , uco_deps = mkUnivCoDepsSets (mkDVarSet cvs) emptyUniqDSet
              }
         }
    go (IfaceSymCo c)            = SymCo    <$> go c
    go (IfaceTransCo c1 c2)      = TransCo  <$> go c1
                                            <*> go c2
    go (IfaceInstCo c1 t2)       = InstCo   <$> go c1
                                            <*> go t2
    go (IfaceSelCo d c)          = do { c' <- go c
                                      ; return $ mkSelCo d c' }
    go (IfaceLRCo lr c)          = mkLRCo lr <$> go c
    go (IfaceKindCo c)           = KindCo   <$> go c
    go (IfaceSubCo c)            = SubCo    <$> go c
    go (IfaceAxiomCo ax cos)
      = do { args <- mapM go cos
           ; rule <- tcIfaceAxiomRule ax args
           ; case rule of
               BuiltInRule name _ roles proves
                 | Just _ <- matchableProjectionDirection name
                 -> unless (map coercionRole args == roles &&
                            isJust (proves (map coercionKind args))) $
                      failIfM (text "Invalid matchable application projection in interface:" <+> ppr name)
               _ -> return ()
           ; return (AxiomCo rule args) }
    go (IfaceFreeCoVar c)        = pprPanic "tcIfaceCo:IfaceFreeCoVar" (ppr c)
    go (IfaceHoleCo c)           = pprPanic "tcIfaceCo:IfaceHoleCo"    (ppr c)

    go_dep :: IfaceUnivCoDep -> IfL CoVar
    go_dep (IfaceUnivCoDepCoVar n)      = go_var n
    go_dep (IfaceUnivCoDepFreeCoVar cv) = pprPanic "tcIfaceCo:IfaceUnivCoDepFreeCoVar" (ppr cv)
    go_dep (IfaceUnivCoDepHole cv)      = pprPanic "tcIfaceCo:IfaceUnivCoDepHole"      (ppr cv)

    go_var :: IfLclName -> IfL CoVar
    go_var = tcIfaceLclId

{-
************************************************************************
*                                                                      *
                        Core
*                                                                      *
************************************************************************
-}

tcIfaceExpr :: IfaceExpr -> IfL CoreExpr
tcIfaceExpr (IfaceType ty)
  = Type <$> tcIfaceType ty

tcIfaceExpr (IfaceCo co)
  = Coercion <$> tcIfaceCo co

tcIfaceExpr (IfaceCast expr co)
  = Cast <$> tcIfaceExpr expr <*> tcIfaceCo co

tcIfaceExpr (IfaceLcl name)
  = Var <$> tcIfaceLclId name

tcIfaceExpr (IfaceExt gbl)
  = Var <$> tcIfaceExtId gbl

tcIfaceExpr (IfaceLitRubbish tc rep)
  = do rep' <- tcIfaceType rep
       return (Lit (LitRubbish tc rep'))

tcIfaceExpr (IfaceLit lit)
  = do lit' <- tcIfaceLit lit
       return (Lit lit')

tcIfaceExpr (IfaceFCall cc ty) = do
    ty' <- tcIfaceType ty
    u <- newUnique
    return (Var (mkFCallId u cc ty'))

tcIfaceExpr (IfaceTuple sort args)
  = do { args' <- mapM tcIfaceExpr args
       ; tc <- tcTupleTyCon False sort arity
       ; let con_tys = map exprType args'
             some_con_args = map Type con_tys ++ args'
             con_args = case sort of
               UnboxedTuple -> map (Type . getRuntimeRep) con_tys ++ some_con_args
               _            -> some_con_args
                        -- Put the missing type arguments back in
             con_id   = dataConWorkId (tyConSingleDataCon tc)
       ; return (mkApps (Var con_id) con_args) }
  where
    arity = length args

tcIfaceExpr (IfaceLam (bndr, os) body)
  = bindIfaceBndr bndr $ \bndr' ->
    Lam (tcIfaceOneShot os bndr') <$> tcIfaceExpr body
  where
    tcIfaceOneShot IfaceOneShot b = setOneShotLambda b
    tcIfaceOneShot _            b = b

tcIfaceExpr (IfacePack schema witness payload)
  = do { schema' <- tcIfaceType schema
       ; witness' <- tcIfaceType witness
       ; payload' <- tcIfaceExpr payload
       ; when (isTyCoArg payload') $
           failIfM (text "An existential Pack payload must be a term in interface.")
       ; case coreFullView schema' of
           ExistsTy _ a body
             | typeKind witness' `eqType` varType a
             , substTyWith [a] [witness'] body `eqType` exprType payload'
             -> return (Pack schema' witness' payload')
           _ -> failIfM (text "Invalid existential Pack in interface.") }

tcIfaceExpr (IfaceUnpack witness@(name, _) value package body)
  = do { package' <- tcIfaceExpr package
       ; when (isTyCoArg package') $
           failIfM (text "An existential package must be a term in interface.")
       ; case coreFullView (exprType package') of
           ExistsTy _ a payload_ty ->
             bindIfaceTyVar witness $ \witness' ->
             bindIfaceId value $ \value' ->
             do { valid <- evaluateIfaceValidation (ppr name) (varType witness' `eqType` varType a)
                ; unless valid $
                    failIfM (text "Existential Unpack witness kind differs from its package:" <+> ppr name)
                ; let mult = idMult value'
                ; unless (isNonCoVarId value'
                          && isMultiplicityTy (typeKind mult)
                          && not (witness' `elemVarSet` tyCoVarsOfType mult)
                          && substTyWith [a] [mkTyVarTy witness'] payload_ty `eqType` varType value') $
                    failIfM (text "Invalid existential payload binder in interface.")
                ; body' <- tcIfaceExpr body
                ; when (isTyCoArg body') $
                    failIfM (text "An existential Unpack body must be a term in interface.")
                ; let result_fvs = tyCoVarsOfType (exprType body')
                ; when (witness' `elemVarSet` result_fvs || value' `elemVarSet` result_fvs) $
                    failIfM (text "Existential Unpack binder escapes its scope in interface.")
                ; return (Unpack witness' value' package' body') }
           _ -> failIfM (text "Unpack requires an existential package in interface.") }

tcIfaceExpr (IfaceApp fun arg@IfaceIndexedConArg{}) = tcIfaceRetainedApp fun arg
tcIfaceExpr (IfaceApp fun arg@IfaceRetainedCharArg{}) = tcIfaceRetainedApp fun arg
tcIfaceExpr (IfaceApp fun arg@IfaceRetainedNaturalArg{}) = tcIfaceRetainedApp fun arg
tcIfaceExpr (IfaceDictApp fun name)
  = do { f <- tcIfaceExpr fun
       ; dict <- tcIfaceLclId name
       ; case coreFullView (exprType f) of
           DepDictTy binder _
             | isNativeClassDictionaryId dict
             , idType dict `eqType` idType binder -> return (App f (Var dict))
           _ -> failIfM (text "Invalid scoped dictionary application in interface:" <+> ppr name) }

tcIfaceExpr (IfaceIndexedConArg _ _)
  = failIfM (text "Indexed constructor operand outside a retained application or proof case.")
tcIfaceExpr (IfaceRetainedCharArg _)
  = failIfM (text "Character operand outside a retained application or proof case.")
tcIfaceExpr (IfaceRetainedNaturalArg _)
  = failIfM (text "Natural operand outside a retained application or proof case.")
tcIfaceExpr (IfaceApp fun arg)
  = App <$> tcIfaceExpr fun <*> tcIfaceExpr arg

tcIfaceExpr (IfaceECase scrut ty)
  = do { scrut' <- tcIfaceExpr scrut
       ; ty' <- tcIfaceType ty
       ; return (castBottomExpr scrut' ty') }

tcIfaceExpr (IfaceCase scrut case_bndr alts)  = do
    -- See Note [Native index alias evidence in interfaces].
    forM_ alts $ \ (IfaceAlt con names _) -> case con of
      IfaceDefaultAlt | not (null names) ->
        failIfM (text "An ordinary interface DEFAULT alternative cannot bind names:" <+> ppr names)
      IfaceDefaultAltWithProof -> case names of
        [proof] | proof /= case_bndr -> return ()
        _ -> failIfM (text "An index alias alternative requires one proof distinct from its case binder:" <+> ppr names)
      _ -> return ()
    (tagged_literal, tagged_operand, scrut') <- tcIfaceRetainedOperand scrut
    -- Resolve each constructor once, before inspecting its representation arity.
    let scrut_index = exprToNativeCaseIndex scrut'
        constructed = maybe False isNativeConstructedGadtIndex scrut_index
        is_indexed IfaceDataFamilyCon = False
        is_indexed _ = True
        indexed_data_alt valid_shape name names = do
          dc <- tcIfaceDataCon name
          unless (isNativeIndexedDataAlt dc names && hasNoDups (case_bndr : names)
                  && (not constructed || any is_indexed tagged_operand) && valid_shape dc) $
            failIfM (text "Invalid indexed constructor proof in interface alternative:"
                     <+> ppr dc <+> ppr names)
          return (DataAlt dc)
    resolved <- forM alts $ \ (IfaceAlt con names rhs) -> do
      alt_con <- case con of
        IfaceDefaultAlt   -> return DEFAULT
        IfaceDefaultAltWithProof -> return DEFAULT
        IfaceLitAlt lit   -> LitAlt <$> tcIfaceLit lit
        IfaceDataAlt name -> do
          dc <- tcIfaceDataCon name
          when (isNativeIndexedDataAlt dc names) $
            failIfM (text "Missing indexed constructor proof tag:" <+> ppr dc)
          return (DataAlt dc)
        IfaceIndexedDataAltWithProof name ->
          indexed_data_alt (\dc -> dataConSourceArity dc == 0 && null (dataConExTyCoVars dc)) name names
        IfaceIndexedFieldsAltWithProof name ->
          indexed_data_alt (\dc -> dataConSourceArity dc > 0) name names
        IfaceIndexedExistentialAltWithProof name ->
          indexed_data_alt (\dc -> dataConSourceArity dc == 0 && not (null (dataConExTyCoVars dc))) name names
        IfaceDataAltWithProof name -> do
          dc <- tcIfaceDataCon name
          unless (isNativeIndexDataAlt dc names) $
            failIfM (text "Missing or unsupported constructor proof in interface alternative:" <+> ppr dc <+> ppr names)
          return (DataAlt dc)
      return (alt_con, names, rhs)
    let native_alt (DataAlt con, names, _) = isNativeIndexDataAlt con names
        native_alt _ = False
        has_alt con = any (\(c, _, _) -> c == con) resolved
        constructor_proof = any native_alt resolved
        indexed_proof = any (\(con, ns, _) -> case con of
                               DataAlt dc -> isNativeIndexedDataAlt dc ns
                               _ -> False) resolved
        native = constructor_proof || indexed_proof || any (\(c, ns, _) -> c == DEFAULT && not (null ns)) resolved
    when (tagged_literal && not (native && isJust scrut_index)) $
      failIfM (text "Invalid literal scrutinee in native index interface proof:" <+> ppr scrut')
    forM_ tagged_operand $ \shape ->
      unless (native && isJust scrut_index && matchesIndexedConShape shape scrut') $
        failIfM (text "Invalid indexed constructor scrutinee in native index interface proof:"
                 <+> ppr scrut')
    when constructor_proof $ do
      when (isNothing scrut_index) $
        failIfM (text "Unsupported scrutinee for native index interface evidence:" <+> ppr scrut')
      unless (has_alt DEFAULT || case splitTyConApp_maybe (exprType scrut') of
               Just (tc, _) | isAlgTyCon tc -> all (has_alt . DataAlt) (tyConDataCons tc)
               _ -> False) $
        failIfM (text "Native index interface proof alternatives must be exhaustive:" <+> ppr scrut')
    case_bndr_name <- newIfaceName (mkVarOccFS (ifLclNameFS case_bndr))
    let
        scrut_ty   = exprType scrut'
        case_mult  = ManyTy
        case_bndr' = mkLocalIdOrCoVar case_bndr_name case_mult scrut_ty
     -- "OrCoVar" since a coercion can be a scrutinee with -fdefer-type-errors
     -- (e.g. see test T15695). Ticket #17291 covers fixing this problem.
        tc_app     = splitTyConApp scrut_ty
                -- NB: Won't always succeed (polymorphic case)
                --     but won't be demanded in those cases
                -- NB: not tcSplitTyConApp; we are looking at Core here
                --     look through non-rec newtypes to find the tycon that
                --     corresponds to the datacon in this case alternative

    extendIfaceIdEnv [case_bndr'] $ do
     alts' <- mapM (tcIfaceAlt scrut' scrut_index case_bndr' tc_app) resolved
     result_ty <- if native
       then do
         -- coreAltType panics on an escaping binder. Report the interface error.
         result_tys <- forM alts' $ \ (Alt con bs rhs) ->
           case occCheckExpand (case_bndr' : bs) (exprType rhs) of
             Just ty -> return ty
             Nothing -> failIfM (text "Escaping binder in native index interface result:" <+> ppr con)
         case result_tys of
           ty : _ -> return ty
           [] -> failIfM (text "Native index interface case has no alternatives")
       else return (coreAltsType alts')
     return (Case scrut' case_bndr' result_ty alts')

tcIfaceExpr (IfaceLet (IfaceNonRec (IfLetBndr fs ty info ji) rhs) body)
  = do  { name    <- newIfaceName (mkVarOccFS (ifLclNameFS fs))
        ; ty'     <- tcIfaceType ty
        ; id_info <- tcIdInfo False {- Don't ignore prags; we are inside one! -}
                              NotTopLevel name ty' info
        ; let id = mkLocalIdWithInfo name ManyTy ty' id_info
                     `asJoinId_maybe` ji
        ; rhs' <- tcIfaceExpr rhs
        ; body' <- extendIfaceIdEnv [id] (tcIfaceExpr body)
        ; return (Let (NonRec id rhs') body') }

tcIfaceExpr (IfaceLet (IfaceRec pairs) body)
  = do { ids <- mapM tc_rec_bndr (map fst pairs)
       ; extendIfaceIdEnv ids $ do
       { pairs' <- zipWithM tc_pair pairs ids
       ; body' <- tcIfaceExpr body
       ; return (Let (Rec pairs') body') } }
 where
   tc_rec_bndr (IfLetBndr fs ty _ ji)
     = do { name <- newIfaceName (mkVarOccFS (ifLclNameFS fs))
          ; ty'  <- tcIfaceType ty
          ; return (mkLocalId name ManyTy ty' `asJoinId_maybe` ji) }
   tc_pair (IfLetBndr _ _ info _, rhs) id
     = do { rhs' <- tcIfaceExpr rhs
          ; id_info <- tcIdInfo False {- Don't ignore prags; we are inside one! -}
                                NotTopLevel (idName id) (idType id) info
          ; return (setIdInfo id id_info, rhs') }

tcIfaceExpr (IfaceTick tickish expr) = do
    expr' <- tcIfaceExpr expr
    -- If debug flag is not set: Ignore source notes
    need_notes <- needSourceNotes <$> getDynFlags
    case tickish of
      IfaceSource{} | not (need_notes)
                    -> return expr'
      _otherwise    -> do
        tickish' <- tcIfaceTickish tickish
        return (Tick tickish' expr')

-- | Check the indexed type of an application with a tagged operand.
tcIfaceRetainedApp :: IfaceExpr -> IfaceExpr -> IfL CoreExpr
tcIfaceRetainedApp fun arg
  = do { f <- tcIfaceExpr fun
       ; (_, shape, a) <- tcIfaceRetainedOperand arg
       ; case coreFullView (exprType f) of
           DepFunTy _ binder _ | all (`matchesIndexedConShape` a) shape
             , Just index <- retainedArgIndex a
             , typeKind index `eqType` varType binder -> return (App f a)
           _ -> failIfM (text "Invalid tagged operand in retained interface application.") }

-- | Read optional capability tags in their writer order.
tcIfaceRetainedOperand :: IfaceExpr -> IfL (Bool, [IfaceIndexedConShape], CoreExpr)
tcIfaceRetainedOperand expr
  = do { let (tagged_natural, char_operand) = case expr of
               IfaceRetainedNaturalArg operand -> (True, operand)
               _ -> (False, expr)
       ; let (tagged_char, rest) = case char_operand of
               IfaceRetainedCharArg operand -> (True, operand)
               _ -> (False, char_operand)
       ; let (shape, operand) = shape_tags rest
       ; result <- tcIfaceExpr operand
       ; when (tagged_char && not (containsRetainedCharLiteral result)) $
           failIfM (text "Missing boxed character literal in tagged interface operand:" <+> ppr result)
       ; when (tagged_natural && not (containsRetainedNaturalLiteral result)) $
           failIfM (text "Missing boxed natural literal in tagged interface operand:" <+> ppr result)
       ; return (tagged_char || tagged_natural, shape, result) }
  where
    shape_tags (IfaceIndexedConArg con subexpr) =
      let (shapes, operand) = shape_tags subexpr
      in (con : shapes, operand)
    shape_tags operand = ([], operand)

-------------------------
tcIfaceTickish :: IfaceTickish -> IfL CoreTickish
tcIfaceTickish (IfaceHpcTick modl ix)   = return (HpcTick modl ix)
tcIfaceTickish (IfaceSCC  cc tick push) = return (ProfNote cc tick push)
tcIfaceTickish (IfaceSource src name)   = return (SourceNote src (LexicalFastString name))
tcIfaceTickish (IfaceBreakpoint bid fvs) = do
  fvs' <- mapM tcIfaceExpr fvs
  return (Breakpoint NoExtField bid [f | Var f <- fvs'])

-------------------------
tcIfaceLit :: Literal -> IfL Literal
tcIfaceLit lit = return lit

-------------------------
tcIfaceAlt :: CoreExpr -> Maybe Type -> Id -> (TyCon, [Type])
           -> (AltCon, [IfLclName], IfaceExpr) -> IfL CoreAlt
tcIfaceAlt _ _ _ _ (DEFAULT, [], rhs) =
  Alt DEFAULT [] <$> tcIfaceExpr rhs
tcIfaceAlt scrut _ case_bndr _ (DEFAULT, [proof], rhs)
  | isNativeIndexAliasBinder scrut case_bndr
  , Just index <- exprToNativeCaseIndex scrut
  , eqType (idType case_bndr) (typeKind index)
  = do { name <- newIfaceName (mkVarOccFS (ifLclNameFS proof))
       ; let cv = mkCoVar name (mkNomEqPred (TermVarTy case_bndr) index)
       ; Alt DEFAULT [cv] <$> extendIfaceIdEnv [cv] (tcIfaceExpr rhs) }
tcIfaceAlt scrut _ _ _ (DEFAULT, _, _) =
  failIfM (text "Unsupported scrutinee for native index alias interface evidence:" <+> ppr scrut)

tcIfaceAlt _ _ _ _ (con@(LitAlt _), names, rhs) = do
  unless (null names) $
    failIfM (text "Interface literal alternative cannot bind names:" <+> ppr names)
  Alt con [] <$> tcIfaceExpr rhs

tcIfaceAlt scrut scrut_index case_bndr (tycon, inst_tys) (DataAlt con, names, rhs)
  | isNativeIndexedDataAlt con names
  , (ordinary_names, [proof_name]) <- splitAt (length names - 1) names
  = do { uniqs <- getUniquesM
       ; let (exs, fields) =
               dataConRepFSInstPat (map ifLclNameFS ordinary_names) uniqs mult con inst_tys
             ordinary = exs ++ fields
       ; case scrut_index of
           Just index
             | isNativeIndexedCaseScrutinee index
             , Just constructor <- nativeIndexedCon_maybe (exprType scrut) con ordinary ->
                 do { fresh <- newIfaceName (mkVarOccFS (ifLclNameFS proof_name))
                    ; let cv = mkCoVar fresh (mkNomEqPred index constructor)
                    ; rhs' <- extendIfaceEnvs exs $
                              extendIfaceIdEnv (fields ++ [cv]) (tcIfaceExpr rhs)
                    ; return (Alt (DataAlt con) (ordinary ++ [cv]) rhs') }
           _ -> failIfM (text "Invalid indexed constructor proof endpoint:"
                         <+> ppr con <+> ppr (exprType scrut)) }
  | isNativeIndexDataAlt con names
  = do { (field_names, proof_name) <- case splitAt (dataConRepArity con) names of
           (fs, [p]) | hasNoDups names -> return (fs, p)
           _ -> failIfM (text "Native index interface alternative requires constructor fields followed by one proof, with distinct field and proof names:"
                         <+> ppr con <+> ppr names)
       ; index <- case scrut_index of
           Just i  -> return i
           Nothing -> failIfM (text "Unsupported scrutinee for native index interface evidence:" <+> ppr scrut)
       ; tys <- case nativeIndexDataConTyArgs (exprType scrut) con of
           Just args -> return args
           Nothing   -> failIfM (text "Unsupported native index constructor domain:" <+> ppr con <+> ppr (exprType scrut))
       ; uniqs <- getUniquesM
       ; let (ex, fields) = dataConRepFSInstPat (map ifLclNameFS field_names) uniqs mult con tys
             indices = map TermVarTy fields
       ; unless (null ex && all isNativeOpaqueValueId fields && nativeIndexArgKinds con tys indices) $
           failIfM (text "Invalid constructor fields in native index interface evidence:" <+> ppr con <+> ppr fields)
       ; fresh <- newIfaceName (mkVarOccFS (ifLclNameFS proof_name))
       ; let cv = mkCoVar fresh (mkNomEqPred index (mkTyConApp (promoteDataCon con) (tys ++ indices)))
             binders = fields ++ [cv]
       ; rhs' <- extendIfaceIdEnv binders (tcIfaceExpr rhs)
       ; return (Alt (DataAlt con) binders rhs') }
  | otherwise
  = do { when (debugIsOn && not (con `elem` tyConDataCons tycon))
           (failIfM (ppr scrut $$ ppr con $$ ppr tycon $$ ppr (tyConDataCons tycon)))
       ; uniqs <- getUniquesM
       ; let (ex_tvs, arg_ids) =
               dataConRepFSInstPat (map ifLclNameFS names) uniqs mult con inst_tys
       ; rhs' <- extendIfaceEnvs ex_tvs $ extendIfaceIdEnv arg_ids $ tcIfaceExpr rhs
       ; return (Alt (DataAlt con) (ex_tvs ++ arg_ids) rhs') }
  where
    mult = idMult case_bndr

-- | Check the payload against the capabilities advertised by its wire tag.
matchesIndexedConShape :: IfaceIndexedConShape -> CoreExpr -> Bool
matchesIndexedConShape IfaceSimpleIndexedCon e =
  containsIndexedConOperand e && not (containsGeneralIndexedConOperand e)
matchesIndexedConShape IfaceGeneralIndexedCon e = containsGeneralIndexedConOperand e
matchesIndexedConShape IfaceDataFamilyCon e = containsRetainedDataFamilyConOperand e

{- Note [Native index alias evidence in interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A case on a callback can name its result before matching its constructors.
In classify f b, the outer DEFAULT binds the equality call_index ~# f b.
The inner case inspects call_index. The expression that the caller evaluates
must still be the same application after reading the unfolding.

Write a distinct alternative tag and one local proof name. Derive the left
endpoint from the fresh, unrestricted case binder and the right endpoint
from the actual scrutinee with exprToNativeCaseIndex, as in Core Lint.
This preserves the identity of a local function without unfolding it.
Bind the fresh nominal CoVar only while reading this branch. Check the
result type for escaping binders before returning the case.

An ordinary DEFAULT has no binders. Its existing tag and bytes stay the
same. See Note [Native index alias evidence] in GHC.Core.Lint and
Note [Versioning retained evidence] in GHC.Iface.Binary.
-}

{- Note [Native constructor evidence in interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An interface data alternative stores the names of its binders. A native
alternative has one extra name after its constructor fields. For example,
a branch matching Pair x payload binds a proof that the scrutinee index
equals the promoted Pair applied to x and payload.

The earlier reader supports this slot only on Bool. Preserve that encoding
for Bool. Write IfaceDataAltWithProof for the other supported constructors.
When reading a legacy unfolding, recognize its proof slot by the same
constructor shape. See Note [Versioning retained evidence] in GHC.Iface.Binary.

Check that all field and proof names differ before extending the interface
environment. A repeated name would hide the preceding field in the branch.
Instantiate the ordinary representation fields from left to right. This
gives the payload its type at the fresh x. Derive the promoted result from
those same field Ids and the structural index of the actual scrutinee.
Do not inspect an opaque call head to manufacture an index.
Bind a fresh nominal CoVar
with these endpoints only while reading the branch body. This also covers
a nullary constructor such as True. Check branch-local escape before
recovering the case result, and validate imported unfoldings against
their declared type.

The first indexed operand tag admits the nullary and predecessor forms.
A reader for those forms cannot reconstruct every GADT worker. For example,
Mk2 :: b -> a -> Both (Maybe a) [b] refines two indices. Write the general
operand tag if the index contains any constructor outside the earlier forms.
This lets the earlier reader reject the interface at its version header.

See Note [Instantiating retained constructor fields] in GHC.Core.Utils
and Note [Native index alias evidence] in GHC.Core.Lint.
-}

{-
************************************************************************
*                                                                      *
                IdInfo
*                                                                      *
************************************************************************
-}

-- | Read identities used by the source proof rule checker.
tcIfaceSourceProofRules :: [IfaceSourceProofRule] -> IfL [SourceProofRule]
tcIfaceSourceProofRules = traverse $ \((owner, first, mapper), args) ->
  (,,,) <$> symbol owner <*> traverse symbol first <*> pure mapper <*> traverse (traverse symbol) args
  where
    symbol = uncurry lookupOrig

tcIdDetails :: Name -> Type -> IfaceIdDetails -> IfL IdDetails
tcIdDetails _ _  IfVanillaId           = return VanillaId
tcIdDetails owner ty (IfSourceTableId table_name origin) = do
    table <- forkM (text "Source table of" <+> ppr owner) $ do
      thing <- tcIfaceGlobal table_name
      case thing of
        ATyCon tc | let fun = mkGlobalId (SourceTableId owner tc origin) owner ty vanillaIdInfo
                  , Just _ <- sourceTableId_maybe fun -> return tc
        _ -> failIfM (text "Invalid source table for" <+> ppr owner <+> ppr table_name)
    return (SourceTableId owner table origin)
tcIdDetails _ _ (IfSourceProofId _) = pure VanillaId
tcIdDetails _ _  (IfWorkerLikeId dmds) = return $ WorkerLikeId dmds
tcIdDetails _ ty IfDFunId              = return (mkDFunIdDetails cls)
  where
    (_, _, cls, _) = tcSplitDFunTy ty

tcIdDetails owner ty (IfSourceDFunId methods literal) =
  forkM (text "Source methods of" <+> ppr owner) $
    case sourceDFunDetails_maybe owner ty methods literal of
      Just details -> return details
      Nothing -> failIfM (text "Invalid source methods for dictionary" <+> ppr owner)

tcIdDetails nm _ (IfRecSelId tc _first_con naughty fl)
  = do { tc' <- either (fmap RecSelData . tcIfaceTyCon)
                       (fmap (RecSelPatSyn . tyThingPatSyn) . tcIfaceDecl False)
                       tc
       ; let all_cons         = recSelParentCons tc'
             cons_partitioned = conLikesRecSelInfo all_cons [flLabel fl]
       ; return (RecSelId
                   { sel_tycon      = tc'
                   , sel_naughty    = naughty
                   , sel_fieldLabel = fl { flSelector = nm }
                   , sel_cons       = cons_partitioned }
                       -- Reconstructed here since we don't want Uniques in the Iface file
                ) }
  where
    tyThingPatSyn (AConLike (PatSynCon ps)) = ps
    tyThingPatSyn _ = panic "tcIdDetails: expecting patsyn"

tcIdInfo :: Bool -> TopLevelFlag -> Name -> Type -> IfaceIdInfo -> IfL IdInfo
tcIdInfo ignore_prags toplvl name ty info = do
    lcl_env <- getLclEnv
    -- Set the CgInfo to something sensible but uninformative before
    -- we start; default assumption is that it has CAFs
    let init_info = if if_boot lcl_env == IsBoot
                      then vanillaIdInfo `setUnfoldingInfo` BootUnfolding
                      else vanillaIdInfo

    foldlM tcPrag init_info (needed_prags info)
  where
    needed_prags :: [IfaceInfoItem] -> [IfaceInfoItem]
    needed_prags items
      | not ignore_prags = items
      | otherwise        = filter need_prag items

    need_prag :: IfaceInfoItem -> Bool
      -- Always read in compulsory unfoldings
      -- See Note [Always expose compulsory unfoldings] in GHC.Iface.Tidy
    need_prag (HsUnfold _ (IfCoreUnfold src _ _ _)) = isCompulsorySource src
    need_prag _ = False

    tcPrag :: IdInfo -> IfaceInfoItem -> IfL IdInfo
    tcPrag info HsNoCafRefs        = return (info `setCafInfo`   NoCafRefs)
    tcPrag info (HsArity arity)    = return (info `setArityInfo` arity)
    tcPrag info (HsDmdSig str)     = return (info `setDmdSigInfo` str)
    tcPrag info (HsCprSig cpr)     = return (info `setCprSigInfo` cpr)
    tcPrag info (HsInline prag)    = return (info `setInlinePragInfo` prag)
    tcPrag info (HsLFInfo lf_info) = do
      lf_info <- tcLFInfo lf_info
      return (info `setLFInfo` lf_info)

    tcPrag info (HsTagSig sig) = do
      return (info `setTagSig` sig)

        -- The next two are lazy, so they don't transitively suck stuff in
    tcPrag info (HsUnfold lb if_unf)
      = do { unf <- tcUnfolding toplvl name ty info if_unf
           ; let info1 | lb        = info `setOccInfo` strongLoopBreaker
                       | otherwise = info
           ; return (info1 `setUnfoldingInfo` unf) }

tcLFInfo :: IfaceLFInfo -> IfL LambdaFormInfo
tcLFInfo lfi = case lfi of
    IfLFReEntrant rep_arity ->
      -- LFReEntrant closures in interface files are guaranteed to
      --
      -- - Be top-level, as only top-level closures are exported.
      -- - Have no free variables, as only non-top-level closures have free
      --   variables
      -- - Don't have ArgDescrs, as ArgDescr is used when generating code for
      --   the closure
      --
      -- These invariants are checked when generating LFInfos in toIfaceLFInfo.
      return (LFReEntrant TopLevel rep_arity True ArgUnknown)

    IfLFThunk updatable mb_fun ->
      -- LFThunk closure in interface files are guaranteed to
      --
      -- - Be top-level
      -- - No have free variables
      --
      -- These invariants are checked when generating LFInfos in toIfaceLFInfo.
      return (LFThunk TopLevel True updatable NonStandardThunk mb_fun)

    IfLFUnlifted ->
      return LFUnlifted

    IfLFCon con_name ->
      LFCon <$!> tcIfaceDataCon con_name

    IfLFUnknown fun_flag ->
      return (LFUnknown fun_flag)

tcUnfolding :: TopLevelFlag -> Name -> Type -> IdInfo -> IfaceUnfolding -> IfL Unfolding
-- See Note [Lazily checking Unfoldings]
tcUnfolding toplvl name ty info (IfCoreUnfold src cache if_guidance if_expr)
  = do  { uf_opts <- unfoldingOpts <$> getDynFlags
        ; expr <- tcUnfoldingRhs src toplvl name ty if_expr
        ; let guidance = case if_guidance of
                 IfWhen arity unsat_ok boring_ok -> UnfWhen arity unsat_ok boring_ok
                 IfNoGuidance -> calcUnfoldingGuidance uf_opts is_top_bottoming False expr
          -- See Note [Tying the 'CoreUnfolding' knot]
        ; return $ mkCoreUnfolding src True expr (Just cache) guidance }
  where
    -- Strictness should occur before unfolding!
    is_top_bottoming = isTopLevel toplvl && isDeadEndSig (dmdSigInfo info)

tcUnfolding _toplvl name dfun_ty _ (IfDFunUnfold bs ops)
  = bindIfaceBndrs bs $ \ bs' ->
    do { ops1 <- forkM doc $ do
           ops' <- mapM tcIfaceExpr ops
           when (any hasNativeDependentUnfolding ops') $
             failIfM (text "Native dependent dictionary unfoldings are not supported:" <+> ppr name)
           return ops'
       ; return $ mkDFunUnfolding bs' (classDataCon cls) ops1 }
  where
    doc = text "Class ops for dfun" <+> ppr name
    (_, _, cls, _) = tcSplitDFunTy dfun_ty

{- Note [Tying the 'CoreUnfolding' knot]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The unfolding of recursive definitions can contain references to the
Id being defined. Consider the following example:

    foo :: ()
    foo = foo

The unfolding template of 'foo' is, of course, 'foo'; so the interface
file for this module contains:

    foo :: ();  Unfolding = foo

When rehydrating the interface file we are going to make an Id for
'foo' (in GHC.IfaceToCore), with an 'Unfolding'. We used to make this
'Unfolding' by calling 'mkFinalUnfolding', but that needs to populate,
among other fields, the 'uf_is_value' field, by computing
'exprIsValue' of the template (in this case, 'foo').

'exprIsValue e' looks at the unfoldings of variables in 'e' to see if
they are evaluated; so it consults the `uf_is_value` field of
variables in `e`. Now we can see the problem: to set the `uf_is_value`
field of `foo`'s unfolding, we look at its unfolding (in this case
just `foo` itself!). Loop. This is the root cause of ticket #22272.

The simple solution we chose is to serialise the various auxiliary
fields of `CoreUnfolding` so that we don't need to recreate them when
rehydrating. Specifically, the following fields are moved to the
'UnfoldingCache', which is persisted in the interface file:

* 'uf_is_conlike'
* 'uf_is_value'
* 'uf_is_work_free'
* 'uf_expandable'

These four bits make the interface files only one byte larger per
unfolding; on the other hand, this does save calls to 'exprIsValue',
'exprIsExpandable' etc for every imported Id.

We could choose to do this only for loop breakers. But that's a bit
more complicated and it seems good all round.
-}

{- Note [Lazily checking Unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For unfoldings, we try to do the job lazily, so that we never typecheck
an unfolding that isn't going to be looked at.

The main idea is that if M.hi has a declaration
   f :: Int -> Int
   f = \x. ...A.g...   -- The unfolding for f

then we don't even want to /read/ A.hi unless f's unfolding is actually used; say,
if f is inlined. But we need to be careful. Even if we don't inline f, we might ask
hasNoBinding of it (Core Lint does this in GHC.Core.Lint.checkCanEtaExpand),
and hasNoBinding looks to see if f has a compulsory unfolding.
So the root Unfolding constructor must be visible: we want to be able to read the 'uf_src'
field which says whether it is a compulsory unfolding, without forcing the unfolding RHS
which is stored in 'uf_tmpl'. This matters for efficiency, but not only: if g's unfolding
mentions f, we must not look at the unfolding RHS for f, as this is precisely what we are
in the middle of checking (so looking at it would cause a loop).

Conclusion: `tcUnfolding` must return an `Unfolding` whose `uf_src` field is readable without
forcing the `uf_tmpl` field. In particular, all the functions used at the end of
`tcUnfolding` (such as `mkFinalUnfolding`, `mkCoreUnfolding`) must be
lazy in `expr`.

Ticket #21139
-}

tcUnfoldingRhs :: UnfoldingSource
               -> TopLevelFlag -> Name -> Type -> IfaceExpr -> IfL CoreExpr
tcUnfoldingRhs source toplvl name declared_ty expr
  = forkM doc $ do
    core_expr' <- tcIfaceExpr expr

    -- Keep native checks in this thunk, including for nested unfoldings.
    -- See Note [Native dependent unfolding templates] in GHC.Core.Unfold.Make.
    let native = hasNativeDependentExprRhs core_expr'
                 || hasNativeDependentExprRhs (Type declared_ty)
                 || case source of { RetainedUserSrc -> True; _ -> False }
    -- See Note [Compulsory retained defaults] in GHC.Iface.Tidy.
    case source of
      RetainedDefaultSrc ->
        unless (isRetainedVanillaDefaultMethod name declared_ty) $
          failIfM (text "Invalid compulsory retained default method:" <+> ppr name)
      CompulsorySrc ->
        when native $
          failIfM (text "Unsupported compulsory retained unfolding:" <+> ppr name)
      RetainedUserSrc ->
        -- See Note [Erased package unfoldings].
        unless (not (hasNativeDependentExprRhs (Type declared_ty))
                && (not (hasExistentialExpr core_expr')
                    || (isTopLevel toplvl && supportsNativeUserUnfolding core_expr'))) $
          failIfM (text "Invalid retained user unfolding:" <+> ppr name)
      _ -> pure ()
    dflags <- getDynFlags
    -- Check for type consistency in the unfolding
    -- See Note [Linting Unfoldings from Interfaces] in GHC.Core.Lint
    when (native || (isTopLevel toplvl && gopt Opt_DoCoreLinting dflags)) $ do
        in_scope <- nonDetEltsUniqSet <$> get_in_scope
        logger   <- getLogger
        case lintUnfolding is_compulsory (initLintConfig dflags in_scope) noSrcLoc core_expr' of
          Nothing   -> return ()
          Just errs
            | native -> failIfM (text "Invalid native unfolding in interface:" <+> ppr name
                                  $$ vcat (bagToList errs))
            | otherwise -> liftIO $
                displayLintResults logger doc
                                   (pprCoreExpr core_expr') (emptyBag, errs)
    when (native && not (exprType core_expr' `eqType` declared_ty)) $
      failIfM (text "Native interface unfolding does not match its declared type:" <+> ppr name
               $$ text "Declared type:" <+> ppr declared_ty
               $$ text "Unfolding type:" <+> ppr (exprType core_expr'))
    return core_expr'
  where
    is_compulsory = isCompulsorySource source
    doc = ppWhen is_compulsory (text "Compulsory") <+>
          text "Unfolding of" <+> ppr name

    get_in_scope :: IfL VarSet -- Totally disgusting; but just for linting
    get_in_scope
        = do { (gbl_env, lcl_env) <- getEnvs
             ; let type_envs = knotVarElems (if_rec_types gbl_env)
             ; top_level_vars <- concat <$> mapM (fmap typeEnvIds . setLclEnv ())  type_envs
             ; return (bindingsVars (if_tv_env lcl_env) `unionVarSet`
                       bindingsVars (if_id_env lcl_env) `unionVarSet`
                       mkVarSet top_level_vars) }

    bindingsVars :: FastStringEnv Var -> VarSet
    bindingsVars ufm = mkVarSet $ nonDetEltsUFM ufm
      -- It's OK to use nonDetEltsUFM here because we immediately forget
      -- the ordering by creating a set

tcIfaceOneShot :: IfaceOneShot -> OneShotInfo
tcIfaceOneShot IfaceNoOneShot = NoOneShotInfo
tcIfaceOneShot IfaceOneShot = OneShotLam

{-
************************************************************************
*                                                                      *
                Getting from Names to TyThings
*                                                                      *
************************************************************************
-}

tcIfaceGlobal :: HasDebugCallStack => Name -> IfL TyThing
tcIfaceGlobal name
  | Just thing <- wiredInNameTyThing_maybe name
        -- Wired-in things include TyCons, DataCons, and Ids
        -- Even though we are in an interface file, we want to make
        -- sure the instances and RULES of this thing (particularly TyCon) are loaded
        -- Imagine: f :: Double -> Double
  = do { ifCheckWiredInThing thing; return thing }

  | otherwise
  = do { mod <- case nameModule_maybe name of
                  Just mod -> return mod
                  Nothing  -> if_mod <$> getLclEnv
       ; mb_thing <- loadGlobalName name mod
       ; case mb_thing of
            Failed err      -> failIfM (ppr name <+> pprDiagnostic err)
            Succeeded thing -> return thing }

tcIfaceTyCon :: IfaceTyCon -> IfL TyCon
tcIfaceTyCon (IfaceTyCon name _info)
  = do { thing <- tcIfaceGlobal name
       ; case thing of
              ATyCon tc -> return tc
              AConLike (RealDataCon dc)
                -> do { unless (null (dataConRepArgBinders dc) || dataConHasPromotedRetained dc) $
                          failIfM (text "Invalid retained constructor fields in interface:"
                                    <+> ppr name $$ text "This retained constructor has no supported promoted field telescope.")
                      ; return (promoteDataCon dc) }
              _ -> pprPanic "tcIfaceTyCon" (ppr thing) }

tcIfaceAxiomRule :: IfaceAxiomRule -> [Coercion] -> IfL CoAxiomRule
-- Unlike CoAxioms, which arise from user 'type instance' declarations,
-- built-in rules have stable names in builtInCoAxiomRules.
tcIfaceAxiomRule (IfaceAR_X n) args
  | key == fsLit "AppendSymbolT3"
  = read_deduction "AppendSymbol" append_symbol_rules
  | key `elem` append_symbol_rules
  = read_deduction "AppendSymbol" [key]
  | key == fsLit "CharToNatT1"
  = read_deduction "character conversion" character_rules
  | key `elem` character_rules
  = read_deduction "character conversion" [key]
  | Just axr <- lookupUFM builtInCoAxiomRules key
  = return axr
  | otherwise
  = failIfM (text "Unknown built-in coercion rule in interface:" <+> ppr n)
  where
    key = ifLclNameFS n
    append_symbol_rules = [fsLit "AppendSymbolKnownPrefix", fsLit "AppendSymbolKnownSuffix"]
    character_rules = [fsLit "CharToNatDeduction", fsLit "NatToCharDeduction"]

    -- See Note [Built-in deduction keys in interfaces].
    read_deduction family candidates = case args of
      [arg] | coercionRole arg == Nominal
            , let premise = coercionKind arg ->
        case [ rule
             | candidate <- candidates
             , Just rule@(BuiltInFamInj inj) <- [lookupUFM builtInCoAxiomRules candidate]
             , isJust (bifinj_proves inj premise) ] of
          [rule] -> return rule
          []     -> invalid
          _      -> failIfM (text "Ambiguous legacy" <+> text family
                              <+> text "deduction in interface. Recompile the producer.")
      _ -> invalid
      where
        invalid = failIfM (text "Invalid" <+> text family <+> text "deduction in interface:" <+> ppr n)
tcIfaceAxiomRule (IfaceAR_U name)   _ = do { ax <- tcIfaceUnbranchedAxiom name; return (UnbranchedAxiom ax) }
tcIfaceAxiomRule (IfaceAR_B name i) _ = do { ax <- tcIfaceBranchedAxiom name;   return (BranchedAxiom ax i) }

{- Note [Built-in deduction keys in interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Older modules wrote the same name, AppendSymbolT3, for the two rules that
remove a known prefix or suffix. The rule table kept only one entry. An
optimized client could read the other rule and panic in coercionKind.
CharToNat and NatToChar deductions likewise shared CharToNatT1.

Read the nominal premise before selecting the rule. For AppendSymbol,
substitution can make both rules applicable. It cannot invalidate the
first rule. With two applicable rules the old interface does not specify
which operand the proof equates. Reject it before constructing AxiomCo.

The character conversion rules have disjoint premises. The left endpoint
must have the specified type family head and the right endpoint must
be a literal of the corresponding result kind. The family check in
mkTopUnaryFamDeduction must return Nothing for the other family because
this lookup tries both rules.

Fresh modules write distinct rule names. Older readers reject an unknown
name when they load an affected unfolding. The ordinary interface format
and other rule names remain available for imports.
-}

tcIfaceUnbranchedAxiom :: IfExtName -> IfL (CoAxiom Unbranched)
tcIfaceUnbranchedAxiom name
  = do { thing <- tcIfaceImplicit name
       ; return (toUnbranchedAxiom (tyThingCoAxiom thing)) }

tcIfaceBranchedAxiom :: IfExtName -> IfL (CoAxiom Branched)
tcIfaceBranchedAxiom name
  = do { thing <- tcIfaceImplicit name
       ; return (tyThingCoAxiom thing) }

tcIfaceDataCon :: Name -> IfL DataCon
tcIfaceDataCon name = do { thing <- tcIfaceGlobal name
                         ; case thing of
                                AConLike (RealDataCon dc) -> return dc
                                _       -> pprPanic "tcIfaceDataCon" (ppr name$$ ppr thing) }

tcIfaceExtId :: Name -> IfL Id
tcIfaceExtId name = do { thing <- tcIfaceGlobal name
                       ; case thing of
                          AnId id -> return id
                          _       -> pprPanic "tcIfaceExtId" (ppr name$$ ppr thing) }

-- See Note [Resolving never-exported Names] in GHC.IfaceToCore
tcIfaceImplicit :: Name -> IfL TyThing
tcIfaceImplicit n = do
    lcl_env <- getLclEnv
    case if_implicits_env lcl_env of
        Nothing -> tcIfaceGlobal n
        Just tenv ->
            case lookupTypeEnv tenv n of
                Nothing -> pprPanic "tcIfaceInst" (ppr n $$ ppr tenv)
                Just tything -> return tything

{-
************************************************************************
*                                                                      *
                Bindings
*                                                                      *
************************************************************************
-}

bindIfaceId :: IfaceIdBndr -> (Id -> IfL a) -> IfL a
bindIfaceId (w, fs, ty) thing_inside
  = do  { name <- newIfaceName (mkVarOccFS (ifLclNameFS fs))
        ; ty' <- tcIfaceType ty
        ; w' <- tcIfaceType w
        ; let id = mkLocalIdOrCoVar name w' ty'
          -- We should not have "OrCoVar" here, this is a bug (#17545)
        ; extendIfaceIdEnv [id] (thing_inside id) }

bindIfaceIds :: [IfaceIdBndr] -> ([Id] -> IfL a) -> IfL a
bindIfaceIds [] thing_inside = thing_inside []
bindIfaceIds (b:bs) thing_inside
  = bindIfaceId b   $ \b'  ->
    bindIfaceIds bs $ \bs' ->
    thing_inside (b':bs')

bindIfaceBndr :: IfaceBndr -> (CoreBndr -> IfL a) -> IfL a
bindIfaceBndr (IfaceIdBndr bndr) thing_inside
  = bindIfaceId bndr thing_inside
bindIfaceBndr (IfaceTvBndr bndr) thing_inside
  = bindIfaceTyVar bndr thing_inside

bindIfaceBndrs :: [IfaceBndr] -> ([CoreBndr] -> IfL a) -> IfL a
bindIfaceBndrs []     thing_inside = thing_inside []
bindIfaceBndrs (b:bs) thing_inside
  = bindIfaceBndr b     $ \ b' ->
    bindIfaceBndrs bs   $ \ bs' ->
    thing_inside (b':bs')

-----------------------
bindIfaceForAllBndrs :: [VarBndr IfaceBndr vis] -> ([VarBndr TyCoVar vis] -> IfL a) -> IfL a
bindIfaceForAllBndrs [] thing_inside = thing_inside []
bindIfaceForAllBndrs (bndr:bndrs) thing_inside
  = bindIfaceForAllBndr bndr $ \tv vis ->
    bindIfaceForAllBndrs bndrs $ \bndrs' ->
    thing_inside (Bndr tv vis : bndrs')

bindIfaceForAllBndr :: (VarBndr IfaceBndr vis) -> (TyCoVar -> vis -> IfL a) -> IfL a
bindIfaceForAllBndr (Bndr (IfaceTvBndr tv) vis) thing_inside
  = bindIfaceTyVar tv $ \tv' -> thing_inside tv' vis
bindIfaceForAllBndr (Bndr (IfaceIdBndr tv) vis) thing_inside
  = bindIfaceId tv $ \tv' -> thing_inside tv' vis

bindIfaceTyVar :: IfaceTvBndr -> (TyVar -> IfL a) -> IfL a
bindIfaceTyVar (occ,kind) thing_inside
  = do  { name <- newIfaceName (mkTyVarOccFS (ifLclNameFS occ))
        ; tyvar <- mk_iface_tyvar name kind
        ; extendIfaceTyVarEnv [tyvar] (thing_inside tyvar) }

bindIfaceTyVars :: [IfaceTvBndr] -> ([TyVar] -> IfL a) -> IfL a
bindIfaceTyVars [] thing_inside = thing_inside []
bindIfaceTyVars (bndr:bndrs) thing_inside
  = bindIfaceTyVar bndr   $ \tv  ->
    bindIfaceTyVars bndrs $ \tvs ->
    thing_inside (tv : tvs)

mk_iface_tyvar :: Name -> IfaceKind -> IfL TyVar
mk_iface_tyvar name ifKind
   = do { kind <- tcIfaceType ifKind
        ; return (Var.mkTyVar name kind) }

bindIfaceTyConBinders :: [IfaceTyConBinder]
                      -> ([TyConBinder] -> IfL a) -> IfL a
bindIfaceTyConBinders [] thing_inside = thing_inside []
bindIfaceTyConBinders (b:bs) thing_inside
  = bindIfaceTyConBinderX bindIfaceBndr b $ \ b'  ->
    bindIfaceTyConBinders bs              $ \ bs' ->
    thing_inside (b':bs')

bindIfaceTyConBinders_AT :: [IfaceTyConBinder]
                         -> ([TyConBinder] -> IfL a) -> IfL a
-- Used for type variable in nested associated data/type declarations
-- where some of the type variables are already in scope
--    class C a where { data T a b }
-- Here 'a' is in scope when we look at the 'data T'
bindIfaceTyConBinders_AT [] thing_inside
  = thing_inside []
bindIfaceTyConBinders_AT (b : bs) thing_inside
  = bindIfaceTyConBinderX bind_tv b  $ \b'  ->
    bindIfaceTyConBinders_AT      bs $ \bs' ->
    thing_inside (b':bs')
  where
    bind_tv tv thing
      = do { mb_tv <- lookupIfaceVar tv
           ; case mb_tv of
               Just b' -> thing b'
               Nothing -> bindIfaceBndr tv thing }

bindIfaceTyConBinderX :: (IfaceBndr -> (TyCoVar -> IfL a) -> IfL a)
                      -> IfaceTyConBinder
                      -> (TyConBinder -> IfL a) -> IfL a
bindIfaceTyConBinderX bind_tv (Bndr tv vis) thing_inside
  = bind_tv tv $ \tv' ->
    thing_inside (Bndr tv' vis)

-- CgBreakInfo

hydrateCgBreakInfo :: CgBreakInfo -> IfL ([Maybe (Id, Word)], Type)
hydrateCgBreakInfo CgBreakInfo{..} = do
  bindIfaceTyVars cgb_tyvars $ \_ -> do
    result_ty <- tcIfaceType cgb_resty
    mbVars <- mapM (traverse (\(if_gbl, offset) -> (,offset) <$> bindIfaceId if_gbl return)) cgb_vars
    return (mbVars, result_ty)

-- | This function is only used to construct the environment for GHCi,
-- so we make up fake locations
tcIfaceImport :: IfaceImport -> ImportUserSpec
tcIfaceImport (IfaceImport spec ImpIfaceAll)
  = ImpUserSpec spec ImpUserAll
tcIfaceImport (IfaceImport spec (ImpIfaceEverythingBut ns))
  = ImpUserSpec spec (ImpUserEverythingBut (mkNameSet ns))
tcIfaceImport (IfaceImport spec (ImpIfaceExplicit gre implicit_parents))
  = ImpUserSpec spec (ImpUserExplicit (getDetOrdAvails gre) $ mkNameSet implicit_parents)
tcIfaceImport (IfaceImport spec ImpIfaceDependOnly)
  = ImpUserSpec spec ImpUserDependOnly
