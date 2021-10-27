{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Type checking of type signatures in interface files
-}


{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.IfaceToCore (
        tcLookupImported_maybe,
        importDecl, checkWiredInTyCon, tcHiBootIface, typecheckIface,
        typecheckIfacesForMerging,
        typecheckIfaceForInstantiate,
        tcIfaceDecl, tcIfaceDecls,
        tcIfaceInst, tcIfaceFamInst, tcIfaceRules,
        tcIfaceAnnotations, tcIfaceCompleteMatches,
        tcIfaceExpr,    -- Desired by HERMIT (#7683)
        tcIfaceGlobal,
        tcIfaceOneShot
 ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session

import GHC.Builtin.Types.Literals(typeNatCoAxiomRules)
import GHC.Builtin.Types

import GHC.Iface.Syntax
import GHC.Iface.Load
import GHC.Iface.Env

import GHC.StgToCmm.Types

import GHC.Tc.Errors.Types
import GHC.Tc.TyCl.Build
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType

import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.FVs
import GHC.Core.TyCo.Rep    -- needs to build types & coercions in a knot
import GHC.Core.TyCo.Subst ( substTyCoVars )
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core
import GHC.Core.Unify( RoughMatchTc(..) )
import GHC.Core.Utils
import GHC.Core.Unfold.Make
import GHC.Core.Lint
import GHC.Core.Make
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Opt.OccurAnal ( occurAnalyseExpr )
import GHC.Core.Ppr

import GHC.Unit.Env
import GHC.Unit.External
import GHC.Unit.Module
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Unit.Home.ModInfo

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Logger

import GHC.Data.Bag
import GHC.Data.Maybe
import GHC.Data.FastString
import GHC.Data.List.SetOps

import GHC.Types.Annotations
import GHC.Types.SourceFile
import GHC.Types.SourceText
import GHC.Types.Basic hiding ( SuccessFlag(..) )
import GHC.Types.CompleteMatch
import GHC.Types.SrcLoc
import GHC.Types.TypeEnv
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSet ( mkUniqDSet )
import GHC.Types.Unique.Supply
import GHC.Types.Literal
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Id
import GHC.Types.Id.Make
import GHC.Types.Id.Info
import GHC.Types.Tickish
import GHC.Types.TyThing
import GHC.Types.Error

import GHC.Fingerprint
import qualified GHC.Data.BooleanFormula as BF

import Control.Monad
import GHC.Parser.Annotation
import GHC.Driver.Env.KnotVars

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
  = initIfaceLcl (mi_semantic_module iface) (text "typecheckIface") (mi_boot iface) $ do
        {       -- Get the right set of decls and rules.  If we are compiling without -O
                -- we discard pragmas before typechecking, so that we don't "see"
                -- information that we shouldn't.  From a versioning point of view
                -- It's not actually *wrong* to do so, but in fact GHCi is unable
                -- to handle unboxed tuples, so it must not see unfoldings.
          ignore_prags <- goptM Opt_IgnoreInterfacePragmas

                -- Typecheck the decls.  This is done lazily, so that the knot-tying
                -- within this single module works out right.  It's the callers
                -- job to make sure the knot is tied.
        ; names_w_things <- tcIfaceDecls ignore_prags (mi_decls iface)
        ; let type_env = mkNameEnv names_w_things

                -- Now do those rules, instances and annotations
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
                              , md_insts     = insts
                              , md_fam_insts = fam_insts
                              , md_rules     = rules
                              , md_anns      = anns
                              , md_exports   = exports
                              , md_complete_matches = complete_matches
                              }
    }

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
isAbstractIfaceDecl IfaceFamily{ ifFamFlav = IfaceAbstractClosedSynFamilyTyCon } = True
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
                ifMinDef = BF.mkOr [noLocA bf1, noLocA bf2]
                }
            } `withRolesFrom` d2
    -- It doesn't matter; we'll check for consistency later when
    -- we merge, see 'mergeSignatures'
    | otherwise              = d1 `withRolesFrom` d2

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
    mergeRoles roles1 roles2 = zipWithEqual "mergeRoles" max roles1 roles2

isRepInjectiveIfaceDecl :: IfaceDecl -> Bool
isRepInjectiveIfaceDecl IfaceData{ ifCons = IfDataTyCon _ } = True
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
                            _ -> True ]
        decl_envs = map (mk_decl_env . map snd . mi_decls) ifaces
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
                decls <- tcIfaceDecls ignore_prags (mi_decls iface)
                return (mkNameEnv decls)
        -- But note that we use this type_env to typecheck references to DFun
        -- in 'IfaceInst'
        setImplicitEnvM type_env $ do
        insts     <- mapM tcIfaceInst (mi_insts iface)
        fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
        rules     <- tcIfaceRules ignore_prags (mi_rules iface)
        anns      <- tcIfaceAnnotations (mi_anns iface)
        exports   <- ifaceExportNames (mi_exports iface)
        complete_matches <- tcIfaceCompleteMatches (mi_complete_matches iface)
        return $ ModDetails { md_types     = type_env
                            , md_insts     = insts
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
typecheckIfaceForInstantiate nsubst iface =
  initIfaceLclWithSubst (mi_semantic_module iface)
                        (text "typecheckIfaceForInstantiate")
                        (mi_boot iface) nsubst $ do
    ignore_prags <- goptM Opt_IgnoreInterfacePragmas
    -- See Note [Resolving never-exported Names] in GHC.IfaceToCore
    type_env <- fixM $ \type_env ->
        setImplicitEnvM type_env $ do
            decls     <- tcIfaceDecls ignore_prags (mi_decls iface)
            return (mkNameEnv decls)
    -- See Note [rnIfaceNeverExported]
    setImplicitEnvM type_env $ do
    insts     <- mapM tcIfaceInst (mi_insts iface)
    fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
    rules     <- tcIfaceRules ignore_prags (mi_rules iface)
    anns      <- tcIfaceAnnotations (mi_anns iface)
    exports   <- ifaceExportNames (mi_exports iface)
    complete_matches <- tcIfaceCompleteMatches (mi_complete_matches iface)
    return $ ModDetails { md_types     = type_env
                        , md_insts     = insts
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
          then do { hpt <- getHpt
                 ; case lookupHpt hpt (moduleName mod) of
                      Just info | mi_boot (hm_iface info) == IsBoot
                                -> mkSelfBootInfo (hm_iface info) (hm_details info)
                      _ -> return NoSelfBoot }
          else do

        -- OK, so we're in one-shot mode.
        -- Re #9245, we always check if there is an hi-boot interface
        -- to check consistency against, rather than just when we notice
        -- that an hi-boot is necessary due to a circular import.
        { hsc_env <- getTopEnv
        ; let nc        = hsc_NC hsc_env
        ; let fc        = hsc_FC hsc_env
        ; let mhome_unit = ue_home_unit (hsc_unit_env hsc_env)
        ; let units     = hsc_units hsc_env
        ; let dflags    = hsc_dflags hsc_env
        ; let logger    = hsc_logger hsc_env
        ; let hooks     = hsc_hooks hsc_env
        ; read_result <- liftIO $ findAndReadIface logger nc fc hooks units mhome_unit dflags
                                need (fst (getModuleInstantiation mod)) mod
                                IsBoot  -- Hi-boot file

        ; case read_result of {
            Succeeded (iface, _path) -> do { tc_iface <- initIfaceTcRn $ typecheckIface iface
                                           ; mkSelfBootInfo iface tc_iface } ;
            Failed err               ->

        -- There was no hi-boot file. But if there is circularity in
        -- the module graph, there really should have been one.
        -- Since we've read all the direct imports by now,
        -- eps_is_boot will record if any of our imports mention the
        -- current module, which either means a module loop (not
        -- a SOURCE import) or that our hi-boot file has mysteriously
        -- disappeared.
    do  { eps <- getEps
        ; case lookupUFM (eps_is_boot eps) (moduleName mod) of
            -- The typical case
            Nothing -> return NoSelfBoot
            -- error cases
            Just (GWIB { gwib_isBoot = is_boot }) -> case is_boot of
              IsBoot -> failWithTc (TcRnUnknownMessage $ mkPlainError noHints (elaborate err))
              -- The hi-boot file has mysteriously disappeared.
              NotBoot -> failWithTc (TcRnUnknownMessage $ mkPlainError noHints moduleLoop)
              -- Someone below us imported us!
              -- This is a loop with no hi-boot in the way
    }}}}
  where
    need = text "Need the hi-boot interface for" <+> ppr mod
                 <+> text "to compare against the Real Thing"

    moduleLoop = text "Circular imports: module" <+> quotes (ppr mod)
                     <+> text "depends on itself"

    elaborate err = hang (text "Could not find hi-boot interface for" <+>
                          quotes (ppr mod) <> colon) 4 err


mkSelfBootInfo :: ModIface -> ModDetails -> TcRn SelfBootInfo
mkSelfBootInfo iface mds
  = do -- NB: This is computed DIRECTLY from the ModIface rather
       -- than from the ModDetails, so that we can query 'sb_tcs'
       -- WITHOUT forcing the contents of the interface.
       let tcs = map ifName
                 . filter isIfaceTyCon
                 . map snd
                 $ mi_decls iface
       return $ SelfBoot { sb_mds = mds
                         , sb_tcs = mkNameSet tcs }
  where
    -- | Retuerns @True@ if, when you call 'tcIfaceDecl' on
    -- this 'IfaceDecl', an ATyCon would be returned.
    -- NB: This code assumes that a TyCon cannot be implicit.
    isIfaceTyCon IfaceId{}      = False
    isIfaceTyCon IfaceData{}    = True
    isIfaceTyCon IfaceSynonym{} = True
    isIfaceTyCon IfaceFamily{}  = True
    isIfaceTyCon IfaceClass{}   = True
    isIfaceTyCon IfaceAxiom{}   = False
    isIfaceTyCon IfacePatSyn{}  = False

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
                                       ifIdDetails = details, ifIdInfo = info})
  = do  { ty <- tcIfaceType iface_type
        ; details <- tcIdDetails ty details
        ; info <- tcIdInfo ignore_prags TopLevel name ty info
        ; return (AnId (mkGlobalId details name ty info)) }

tc_iface_decl _ _ (IfaceData {ifName = tc_name,
                          ifCType = cType,
                          ifBinders = binders,
                          ifResKind = res_kind,
                          ifRoles = roles,
                          ifCtxt = ctxt, ifGadtSyntax = gadt_syn,
                          ifCons = rdr_cons,
                          ifParent = mb_parent })
  = bindIfaceTyConBinders_AT binders $ \ binders' -> do
    { res_kind' <- tcIfaceType res_kind

    ; tycon <- fixM $ \ tycon -> do
            { stupid_theta <- tcIfaceCtxt ctxt
            ; parent' <- tc_parent tc_name mb_parent
            ; cons <- tcIfaceDataCons tc_name tycon binders' rdr_cons
            ; return (mkAlgTyCon tc_name binders' res_kind'
                                 roles cType stupid_theta
                                 cons parent' gadt_syn) }
    ; traceIf (text "tcIfaceDecl4" <+> ppr tycon)
    ; return (ATyCon tycon) }
  where
    tc_parent :: Name -> IfaceTyConParent -> IfL AlgTyConFlav
    tc_parent tc_name IfNoParent
      = do { tc_rep_name <- newTyConRepName tc_name
           ; return (VanillaAlgTyCon tc_rep_name) }
    tc_parent _ (IfDataInstance ax_name _ arg_tys)
      = do { ax <- tcIfaceCoAxiom ax_name
           ; let fam_tc  = coAxiomTyCon ax
                 ax_unbr = toUnbranchedAxiom ax
           ; lhs_tys <- tcIfaceAppArgs arg_tys
           ; return (DataFamInstTyCon ax_unbr fam_tc lhs_tys) }

tc_iface_decl _ _ (IfaceSynonym {ifName = tc_name,
                                      ifRoles = roles,
                                      ifSynRhs = rhs_ty,
                                      ifBinders = binders,
                                      ifResKind = res_kind })
   = bindIfaceTyConBinders_AT binders $ \ binders' -> do
     { res_kind' <- tcIfaceType res_kind     -- Note [Synonym kind loop]
     ; rhs      <- forkM (mk_doc tc_name) $
                   tcIfaceType rhs_ty
     ; let tycon = buildSynTyCon tc_name binders' res_kind' roles rhs
     ; return (ATyCon tycon) }
   where
     mk_doc n = text "Type synonym" <+> ppr n

tc_iface_decl parent _ (IfaceFamily {ifName = tc_name,
                                     ifFamFlav = fam_flav,
                                     ifBinders = binders,
                                     ifResKind = res_kind,
                                     ifResVar = res, ifFamInj = inj })
   = bindIfaceTyConBinders_AT binders $ \ binders' -> do
     { res_kind' <- tcIfaceType res_kind    -- Note [Synonym kind loop]
     ; rhs      <- forkM (mk_doc tc_name) $
                   tc_fam_flav tc_name fam_flav
     ; res_name <- traverse (newIfaceName . mkTyVarOccFS) res
     ; let tycon = mkFamilyTyCon tc_name binders' res_kind' res_name rhs parent inj
     ; return (ATyCon tycon) }
   where
     mk_doc n = text "Type synonym" <+> ppr n

     tc_fam_flav :: Name -> IfaceFamTyConFlav -> IfL FamTyConFlav
     tc_fam_flav tc_name IfaceDataFamilyTyCon
       = do { tc_rep_name <- newTyConRepName tc_name
            ; return (DataFamilyTyCon tc_rep_name) }
     tc_fam_flav _ IfaceOpenSynFamilyTyCon= return OpenSynFamilyTyCon
     tc_fam_flav _ (IfaceClosedSynFamilyTyCon mb_ax_name_branches)
       = do { ax <- traverse (tcIfaceCoAxiom . fst) mb_ax_name_branches
            ; return (ClosedSynFamilyTyCon ax) }
     tc_fam_flav _ IfaceAbstractClosedSynFamilyTyCon
         = return AbstractClosedSynFamilyTyCon
     tc_fam_flav _ IfaceBuiltInSynFamTyCon
         = pprPanic "tc_iface_decl"
                    (text "IfaceBuiltInSynFamTyCon in interface file")

tc_iface_decl _parent _ignore_prags
            (IfaceClass {ifName = tc_name,
                         ifRoles = roles,
                         ifBinders = binders,
                         ifFDs = rdr_fds,
                         ifBody = IfAbstractClass})
  = bindIfaceTyConBinders binders $ \ binders' -> do
    { fds  <- mapM tc_fd rdr_fds
    ; cls  <- buildClass tc_name binders' roles fds Nothing
    ; return (ATyCon (classTyCon cls)) }

tc_iface_decl _parent ignore_prags
            (IfaceClass {ifName = tc_name,
                         ifRoles = roles,
                         ifBinders = binders,
                         ifFDs = rdr_fds,
                         ifBody = IfConcreteClass {
                             ifClassCtxt = rdr_ctxt,
                             ifATs = rdr_ats, ifSigs = rdr_sigs,
                             ifMinDef = mindef_occ
                         }})
  = bindIfaceTyConBinders binders $ \ binders' -> do
    { traceIf (text "tc-iface-class1" <+> ppr tc_name)
    ; ctxt <- mapM tc_sc rdr_ctxt
    ; traceIf (text "tc-iface-class2" <+> ppr tc_name)
    ; sigs <- mapM tc_sig rdr_sigs
    ; fds  <- mapM tc_fd rdr_fds
    ; traceIf (text "tc-iface-class3" <+> ppr tc_name)
    ; mindef <- traverse (lookupIfaceTop . mkVarOccFS) mindef_occ
    ; cls  <- fixM $ \ cls -> do
              { ats  <- mapM (tc_at cls) rdr_ats
              ; traceIf (text "tc-iface-class4" <+> ppr tc_name)
              ; buildClass tc_name binders' roles fds (Just (ctxt, ats, sigs, mindef)) }
    ; return (ATyCon (classTyCon cls)) }
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
                                     ; return (Just (tc_def, NoATVI)) }
                  -- Must be done lazily in case the RHS of the defaults mention
                  -- the type constructor being defined here
                  -- e.g.   type AT a; type AT b = AT [b]   #8002
          return (ATI tc mb_def)

   mk_sc_doc pred = text "Superclass" <+> ppr pred
   mk_at_doc tc = text "Associated type" <+> ppr tc
   mk_op_doc op_name op_ty = text "Class op" <+> sep [ppr op_name, ppr op_ty]

tc_iface_decl _ _ (IfaceAxiom { ifName = tc_name, ifTyCon = tc
                              , ifAxBranches = branches, ifRole = role })
  = do { tc_tycon    <- tcIfaceTyCon tc
       -- Must be done lazily, because axioms are forced when checking
       -- for family instance consistency, and the RHS may mention
       -- a hs-boot declared type constructor that is going to be
       -- defined by this module.
       -- e.g. type instance F Int = ToBeDefined
       -- See #13803
       ; tc_branches <- forkM (text "Axiom branches" <+> ppr tc_name)
                      $ tc_ax_branches branches
       ; let axiom = CoAxiom { co_ax_unique   = nameUnique tc_name
                             , co_ax_name     = tc_name
                             , co_ax_tc       = tc_tycon
                             , co_ax_role     = role
                             , co_ax_branches = manyBranches tc_branches
                             , co_ax_implicit = False }
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
       ; builder <- fmapMaybeM tc_pr if_builder
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

tcIfaceDecls :: Bool
          -> [(Fingerprint, IfaceDecl)]
          -> IfL [(Name,TyThing)]
tcIfaceDecls ignore_prags ver_decls
   = concatMapM (tc_iface_decl_fingerprint ignore_prags) ver_decls

tc_iface_decl_fingerprint :: Bool                    -- Don't load pragmas into the decl pool
          -> (Fingerprint, IfaceDecl)
          -> IfL [(Name,TyThing)]   -- The list can be poked eagerly, but the
                                    -- TyThings are forkM'd thunks
tc_iface_decl_fingerprint ignore_prags (_version, decl)
  = do  {       -- Populate the name cache with final versions of all
                -- the names associated with the decl
          let main_name = ifName decl

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

tc_ax_branches :: [IfaceAxBranch] -> IfL [CoAxBranch]
tc_ax_branches if_branches = foldlM tc_ax_branch [] if_branches

tc_ax_branch :: [CoAxBranch] -> IfaceAxBranch -> IfL [CoAxBranch]
tc_ax_branch prev_branches
             (IfaceAxBranch { ifaxbTyVars = tv_bndrs
                            , ifaxbEtaTyVars = eta_tv_bndrs
                            , ifaxbCoVars = cv_bndrs
                            , ifaxbLHS = lhs, ifaxbRHS = rhs
                            , ifaxbRoles = roles, ifaxbIncomps = incomps })
  = bindIfaceTyConBinders_AT
      (map (\b -> Bndr (IfaceTvBndr b) (NamedTCB Inferred)) tv_bndrs) $ \ tvs ->
         -- The _AT variant is needed here; see Note [CoAxBranch type variables] in GHC.Core.Coercion.Axiom
    bindIfaceIds cv_bndrs $ \ cvs -> do
    { tc_lhs   <- tcIfaceAppArgs lhs
    ; tc_rhs   <- tcIfaceType rhs
    ; eta_tvs  <- bindIfaceTyVars eta_tv_bndrs return
    ; this_mod <- getIfModule
    ; let loc = mkGeneralSrcSpan (fsLit "module " `appendFS`
                                  moduleNameFS (moduleName this_mod))
          br = CoAxBranch { cab_loc     = loc
                          , cab_tvs     = binderVars tvs
                          , cab_eta_tvs = eta_tvs
                          , cab_cvs     = cvs
                          , cab_lhs     = tc_lhs
                          , cab_roles   = roles
                          , cab_rhs     = tc_rhs
                          , cab_incomps = map (prev_branches `getNth`) incomps }
    ; return (prev_branches ++ [br]) }

tcIfaceDataCons :: Name -> TyCon -> [TyConBinder] -> IfaceConDecls -> IfL AlgTyConRhs
tcIfaceDataCons tycon_name tycon tc_tybinders if_cons
  = case if_cons of
        IfAbstractTyCon
          -> return AbstractTyCon
        IfDataTyCon cons
          -> do  { data_cons  <- mapM tc_con_decl cons
                 ; return $
                     mkLevPolyDataTyConRhs
                       (isFixedRuntimeRepKind $ tyConResKind tycon)
                       data_cons }
        IfNewTyCon con
          -> do  { data_con  <- tc_con_decl con
                 ; mkNewTyConRhs tycon_name tycon data_con }
  where
    univ_tvs :: [TyVar]
    univ_tvs = binderVars tc_tybinders

    tag_map :: NameEnv ConTag
    tag_map = mkTyConTagMap tycon

    tc_con_decl (IfCon { ifConInfix = is_infix,
                         ifConExTCvs = ex_bndrs,
                         ifConUserTvBinders = user_bndrs,
                         ifConName = dc_name,
                         ifConCtxt = ctxt, ifConEqSpec = spec,
                         ifConArgTys = args, ifConFields = lbl_names,
                         ifConStricts = if_stricts,
                         ifConSrcStricts = if_src_stricts})
     = -- Universally-quantified tyvars are shared with
       -- parent TyCon, and are already in scope
       bindIfaceBndrs ex_bndrs    $ \ ex_tvs -> do
        { traceIf (text "Start interface-file tc_con_decl" <+> ppr dc_name)

          -- By this point, we have bound every universal and existential
          -- tyvar. Because of the dcUserTyVarBinders invariant
          -- (see Note [DataCon user type variable binders]), *every* tyvar in
          -- ifConUserTvBinders has a matching counterpart somewhere in the
          -- bound universals/existentials. As a result, calling tcIfaceTyVar
          -- below is always guaranteed to succeed.
        ; user_tv_bndrs <- mapM (\(Bndr bd vis) ->
                                   case bd of
                                     IfaceIdBndr (_, name, _) ->
                                       Bndr <$> tcIfaceLclId name <*> pure vis
                                     IfaceTvBndr (name, _) ->
                                       Bndr <$> tcIfaceTyVar name <*> pure vis)
                                user_bndrs

        -- Read the context and argument types, but lazily for two reasons
        -- (a) to avoid looking tugging on a recursive use of
        --     the type itself, which is knot-tied
        -- (b) to avoid faulting in the component types unless
        --     they are really needed
        ; ~(eq_spec, theta, arg_tys, stricts) <- forkM (mk_doc dc_name) $
             do { eq_spec <- tcIfaceEqSpec spec
                ; theta   <- tcIfaceCtxt ctxt
                -- This fixes #13710.  The enclosing lazy thunk gets
                -- forced when typechecking record wildcard pattern
                -- matching (it's not completely clear why this
                -- tuple is needed), which causes trouble if one of
                -- the argument types was recursively defined.
                -- See also Note [Tying the knot]
                ; arg_tys <- forkM (mk_doc dc_name <+> text "arg_tys")
                           $ mapM (\(w, ty) -> mkScaled <$> tcIfaceType w <*> tcIfaceType ty) args
                ; stricts <- mapM tc_strict if_stricts
                        -- The IfBang field can mention
                        -- the type itself; hence inside forkM
                ; return (eq_spec, theta, arg_tys, stricts) }

        -- Remember, tycon is the representation tycon
        ; let orig_res_ty = mkFamilyTyConApp tycon
                              (substTyCoVars (mkTvSubstPrs (map eqSpecPair eq_spec))
                                             (binderVars tc_tybinders))

        ; prom_rep_name <- newTyConRepName dc_name

        ; con <- buildDataCon (pprPanic "tcIfaceDataCons: FamInstEnvs" (ppr dc_name))
                       dc_name is_infix prom_rep_name
                       (map src_strict if_src_stricts)
                       (Just stricts)
                       -- Pass the HsImplBangs (i.e. final
                       -- decisions) to buildDataCon; it'll use
                       -- these to guide the construction of a
                       -- worker.
                       -- See Note [Bangs on imported data constructors] in GHC.Types.Id.Make
                       lbl_names
                       univ_tvs ex_tvs user_tv_bndrs
                       eq_spec theta
                       arg_tys orig_res_ty tycon tag_map
        ; traceIf (text "Done interface-file tc_con_decl" <+> ppr dc_name)
        ; return con }
    mk_doc con_name = text "Constructor" <+> ppr con_name

    tc_strict :: IfaceBang -> IfL HsImplBang
    tc_strict IfNoBang = return (HsLazy)
    tc_strict IfStrict = return (HsStrict)
    tc_strict IfUnpack = return (HsUnpack Nothing)
    tc_strict (IfUnpackCo if_co) = do { co <- tcIfaceCo if_co
                                      ; return (HsUnpack (Just co)) }

    src_strict :: IfaceSrcBang -> HsSrcBang
    src_strict (IfSrcBang unpk bang) = HsSrcBang NoSourceText unpk bang

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
tcRoughTyCon (Just tc) = KnownTc (ifaceTyConName tc)
tcRoughTyCon Nothing   = OtherTc

tcIfaceInst :: IfaceClsInst -> IfL ClsInst
tcIfaceInst (IfaceClsInst { ifDFun = dfun_name, ifOFlag = oflag
                          , ifInstCls = cls, ifInstTys = mb_tcs
                          , ifInstOrph = orph })
  = do { dfun <- forkM (text "Dict fun" <+> ppr dfun_name) $
                    fmap tyThingId (tcIfaceImplicit dfun_name)
       ; let mb_tcs' = map tcRoughTyCon mb_tcs
       ; return (mkImportedInstance cls mb_tcs' dfun_name dfun oflag orph) }

tcIfaceFamInst :: IfaceFamInst -> IfL FamInst
tcIfaceFamInst (IfaceFamInst { ifFamInstFam = fam, ifFamInstTys = mb_tcs
                             , ifFamInstAxiom = axiom_name } )
    = do { axiom' <- forkM (text "Axiom" <+> ppr axiom_name) $
                     tcIfaceCoAxiom axiom_name
             -- will panic if branched, but that's OK
         ; let axiom'' = toUnbranchedAxiom axiom'
               mb_tcs' = map tcRoughTyCon mb_tcs
         ; return (mkImportedFamInst fam mb_tcs' axiom'') }

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
tcIfaceRule (IfaceRule {ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
                        ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs,
                        ifRuleAuto = auto, ifRuleOrph = orph })
  = do  { ~(bndrs', args', rhs') <-
                -- Typecheck the payload lazily, in the hope it'll never be looked at
                forkM (text "Rule" <+> pprRuleName name) $
                bindIfaceBndrs bndrs                      $ \ bndrs' ->
                do { args'  <- mapM tcIfaceExpr args
                   ; rhs'   <- tcIfaceExpr rhs
                   ; whenGOptM Opt_DoCoreLinting $ do
                      { dflags <- getDynFlags
                      ; (_, lcl_env) <- getEnvs
                      ; let in_scope :: [Var]
                            in_scope = ((nonDetEltsUFM $ if_tv_env lcl_env) ++
                                        (nonDetEltsUFM $ if_id_env lcl_env) ++
                                        bndrs' ++
                                        exprsFreeIdsList args')
                      ; case lintExpr dflags in_scope rhs' of
                          Nothing   -> return ()
                          Just errs -> do
                            logger <- getLogger
                            liftIO $ displayLintResults logger False doc
                                               (pprCoreExpr rhs')
                                               (emptyBag, errs) }
                   ; return (bndrs', args', rhs') }
        ; let mb_tcs = map ifTopFreeName args
        ; this_mod <- getIfModule
        ; return (Rule { ru_name = name, ru_fn = fn, ru_act = act,
                          ru_bndrs = bndrs', ru_args = args',
                          ru_rhs = occurAnalyseExpr rhs',
                          ru_rough = mb_tcs,
                          ru_origin = this_mod,
                          ru_orphan = orph,
                          ru_auto = auto,
                          ru_local = False }) } -- An imported RULE is never for a local Id
                                                -- or, even if it is (module loop, perhaps)
                                                -- we'll just leave it in the non-local set
  where
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
tcIfaceCompleteMatch (IfaceCompleteMatch ms mtc) = forkM doc $ do -- See Note [Positioning of forkM]
  conlikes <- mkUniqDSet <$> mapM tcIfaceConLike ms
  mtc' <- traverse tcIfaceTyCon mtc
  return (CompleteMatch conlikes mtc')
  where
    doc = text "COMPLETE sig" <+> ppr ms

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

tcIfaceType :: IfaceType -> IfL Type
tcIfaceType = go
  where
    go (IfaceTyVar n)            = TyVarTy <$> tcIfaceTyVar n
    go (IfaceFreeTyVar n)        = pprPanic "tcIfaceType:IfaceFreeTyVar" (ppr n)
    go (IfaceLitTy l)            = LitTy <$> tcIfaceTyLit l
    go (IfaceFunTy flag w t1 t2) = FunTy flag <$> tcIfaceType w <*> go t1 <*> go t2
    go (IfaceTupleTy s i tks)    = tcIfaceTupleTy s i tks
    go (IfaceAppTy t ts)
      = do { t'  <- go t
           ; ts' <- traverse go (appArgsIfaceTypes ts)
           ; pure (foldl' AppTy t' ts') }
    go (IfaceTyConApp tc tks)
      = do { tc' <- tcIfaceTyCon tc
           ; tks' <- mapM go (appArgsIfaceTypes tks)
           ; return (mkTyConApp tc' tks') }
    go (IfaceForAllTy bndr t)
      = bindIfaceForAllBndr bndr $ \ tv' vis ->
        ForAllTy (Bndr tv' vis) <$> go t
    go (IfaceCastTy ty co)   = CastTy <$> go ty <*> tcIfaceCo co
    go (IfaceCoercionTy co)  = CoercionTy <$> tcIfaceCo co

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
tcIfaceTyLit (IfaceStrTyLit n) = return (StrTyLit n)
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
    go (IfaceFunCo r w c1 c2)    = mkFunCo r <$> go w <*> go c1 <*> go c2
    go (IfaceTyConAppCo r tc cs)
      = TyConAppCo r <$> tcIfaceTyCon tc <*> mapM go cs
    go (IfaceAppCo c1 c2)        = AppCo <$> go c1 <*> go c2
    go (IfaceForAllCo tv k c)  = do { k' <- go k
                                      ; bindIfaceBndr tv $ \ tv' ->
                                        ForAllCo tv' k' <$> go c }
    go (IfaceCoVarCo n)          = CoVarCo <$> go_var n
    go (IfaceAxiomInstCo n i cs) = AxiomInstCo <$> tcIfaceCoAxiom n <*> pure i <*> mapM go cs
    go (IfaceUnivCo p r t1 t2)   = UnivCo <$> tcIfaceUnivCoProv p <*> pure r
                                          <*> tcIfaceType t1 <*> tcIfaceType t2
    go (IfaceSymCo c)            = SymCo    <$> go c
    go (IfaceTransCo c1 c2)      = TransCo  <$> go c1
                                            <*> go c2
    go (IfaceInstCo c1 t2)       = InstCo   <$> go c1
                                            <*> go t2
    go (IfaceNthCo d c)          = do { c' <- go c
                                      ; return $ mkNthCo (nthCoRole d c') d c' }
    go (IfaceLRCo lr c)          = LRCo lr  <$> go c
    go (IfaceKindCo c)           = KindCo   <$> go c
    go (IfaceSubCo c)            = SubCo    <$> go c
    go (IfaceAxiomRuleCo ax cos) = AxiomRuleCo <$> tcIfaceCoAxiomRule ax
                                               <*> mapM go cos
    go (IfaceFreeCoVar c)        = pprPanic "tcIfaceCo:IfaceFreeCoVar" (ppr c)
    go (IfaceHoleCo c)           = pprPanic "tcIfaceCo:IfaceHoleCo"    (ppr c)

    go_var :: FastString -> IfL CoVar
    go_var = tcIfaceLclId

tcIfaceUnivCoProv :: IfaceUnivCoProv -> IfL UnivCoProvenance
tcIfaceUnivCoProv (IfacePhantomProv kco)    = PhantomProv <$> tcIfaceCo kco
tcIfaceUnivCoProv (IfaceProofIrrelProv kco) = ProofIrrelProv <$> tcIfaceCo kco
tcIfaceUnivCoProv (IfacePluginProv str)     = return $ PluginProv str
tcIfaceUnivCoProv (IfaceCorePrepProv b)     = return $ CorePrepProv b

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

tcIfaceExpr (IfaceLitRubbish rep)
  = do rep' <- tcIfaceType rep
       return (Lit (LitRubbish rep'))

tcIfaceExpr (IfaceLit lit)
  = do lit' <- tcIfaceLit lit
       return (Lit lit')

tcIfaceExpr (IfaceFCall cc ty) = do
    ty' <- tcIfaceType ty
    u <- newUnique
    dflags <- getDynFlags
    return (Var (mkFCallId dflags u cc ty'))

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

tcIfaceExpr (IfaceApp fun arg)
  = App <$> tcIfaceExpr fun <*> tcIfaceExpr arg

tcIfaceExpr (IfaceECase scrut ty)
  = do { scrut' <- tcIfaceExpr scrut
       ; ty' <- tcIfaceType ty
       ; return (castBottomExpr scrut' ty') }

tcIfaceExpr (IfaceCase scrut case_bndr alts)  = do
    scrut' <- tcIfaceExpr scrut
    case_bndr_name <- newIfaceName (mkVarOccFS case_bndr)
    let
        scrut_ty   = exprType scrut'
        case_mult = Many
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
     alts' <- mapM (tcIfaceAlt scrut' case_mult tc_app) alts
     return (Case scrut' case_bndr' (coreAltsType alts') alts')

tcIfaceExpr (IfaceLet (IfaceNonRec (IfLetBndr fs ty info ji) rhs) body)
  = do  { name    <- newIfaceName (mkVarOccFS fs)
        ; ty'     <- tcIfaceType ty
        ; id_info <- tcIdInfo False {- Don't ignore prags; we are inside one! -}
                              NotTopLevel name ty' info
        ; let id = mkLocalIdWithInfo name Many ty' id_info
                     `asJoinId_maybe` tcJoinInfo ji
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
     = do { name <- newIfaceName (mkVarOccFS fs)
          ; ty'  <- tcIfaceType ty
          ; return (mkLocalId name Many ty' `asJoinId_maybe` tcJoinInfo ji) }
   tc_pair (IfLetBndr _ _ info _, rhs) id
     = do { rhs' <- tcIfaceExpr rhs
          ; id_info <- tcIdInfo False {- Don't ignore prags; we are inside one! -}
                                NotTopLevel (idName id) (idType id) info
          ; return (setIdInfo id id_info, rhs') }

tcIfaceExpr (IfaceTick tickish expr) = do
    expr' <- tcIfaceExpr expr
    -- If debug flag is not set: Ignore source notes
    dbgLvl <- fmap debugLevel getDynFlags
    case tickish of
      IfaceSource{} | dbgLvl == 0
                    -> return expr'
      _otherwise    -> do
        tickish' <- tcIfaceTickish tickish
        return (Tick tickish' expr')

-------------------------
tcIfaceTickish :: IfaceTickish -> IfM lcl CoreTickish
tcIfaceTickish (IfaceHpcTick modl ix)   = return (HpcTick modl ix)
tcIfaceTickish (IfaceSCC  cc tick push) = return (ProfNote cc tick push)
tcIfaceTickish (IfaceSource src name)   = return (SourceNote src name)

-------------------------
tcIfaceLit :: Literal -> IfL Literal
tcIfaceLit lit = return lit

-------------------------
tcIfaceAlt :: CoreExpr -> Mult -> (TyCon, [Type])
           -> IfaceAlt
           -> IfL CoreAlt
tcIfaceAlt _ _ _ (IfaceAlt IfaceDefault names rhs)
  = assert (null names) $ do
    rhs' <- tcIfaceExpr rhs
    return (Alt DEFAULT [] rhs')

tcIfaceAlt _ _ _ (IfaceAlt (IfaceLitAlt lit) names rhs)
  = assert (null names) $ do
    lit' <- tcIfaceLit lit
    rhs' <- tcIfaceExpr rhs
    return (Alt (LitAlt lit') [] rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcIfaceAlt scrut mult (tycon, inst_tys) (IfaceAlt (IfaceDataAlt data_occ) arg_strs rhs)
  = do  { con <- tcIfaceDataCon data_occ
        ; when (debugIsOn && not (con `elem` tyConDataCons tycon))
               (failIfM (ppr scrut $$ ppr con $$ ppr tycon $$ ppr (tyConDataCons tycon)))
        ; tcIfaceDataAlt mult con inst_tys arg_strs rhs }

tcIfaceDataAlt :: Mult -> DataCon -> [Type] -> [FastString] -> IfaceExpr
               -> IfL CoreAlt
tcIfaceDataAlt mult con inst_tys arg_strs rhs
  = do  { us <- newUniqueSupply
        ; let uniqs = uniqsFromSupply us
        ; let (ex_tvs, arg_ids)
                      = dataConRepFSInstPat arg_strs uniqs mult con inst_tys

        ; rhs' <- extendIfaceEnvs  ex_tvs       $
                  extendIfaceIdEnv arg_ids      $
                  tcIfaceExpr rhs
        ; return (Alt (DataAlt con) (ex_tvs ++ arg_ids) rhs') }

{-
************************************************************************
*                                                                      *
                IdInfo
*                                                                      *
************************************************************************
-}

tcIdDetails :: Type -> IfaceIdDetails -> IfL IdDetails
tcIdDetails _  IfVanillaId = return VanillaId
tcIdDetails ty IfDFunId
  = return (DFunId (isNewTyCon (classTyCon cls)))
  where
    (_, _, cls, _) = tcSplitDFunTy ty

tcIdDetails _ (IfRecSelId tc naughty)
  = do { tc' <- either (fmap RecSelData . tcIfaceTyCon)
                       (fmap (RecSelPatSyn . tyThingPatSyn) . tcIfaceDecl False)
                       tc
       ; return (RecSelId { sel_tycon = tc', sel_naughty = naughty }) }
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
    need_prag (HsUnfold _ (IfCompulsory {})) = True
    need_prag _                              = False

    tcPrag :: IdInfo -> IfaceInfoItem -> IfL IdInfo
    tcPrag info HsNoCafRefs        = return (info `setCafInfo`   NoCafRefs)
    tcPrag info (HsArity arity)    = return (info `setArityInfo` arity)
    tcPrag info (HsDmdSig str)     = return (info `setDmdSigInfo` str)
    tcPrag info (HsCprSig cpr)     = return (info `setCprSigInfo` cpr)
    tcPrag info (HsInline prag)    = return (info `setInlinePragInfo` prag)
    tcPrag info HsLevity           = return (info `setNeverRepPoly` ty)
    tcPrag info (HsLFInfo lf_info) = do
      lf_info <- tcLFInfo lf_info
      return (info `setLFInfo` lf_info)

        -- The next two are lazy, so they don't transitively suck stuff in
    tcPrag info (HsUnfold lb if_unf)
      = do { unf <- tcUnfolding toplvl name ty info if_unf
           ; let info1 | lb        = info `setOccInfo` strongLoopBreaker
                       | otherwise = info
           ; return (info1 `setUnfoldingInfo` unf) }

tcJoinInfo :: IfaceJoinInfo -> Maybe JoinArity
tcJoinInfo (IfaceJoinPoint ar) = Just ar
tcJoinInfo IfaceNotJoinPoint   = Nothing

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
tcUnfolding toplvl name _ info (IfCoreUnfold stable if_expr)
  = do  { uf_opts <- unfoldingOpts <$> getDynFlags
        ; mb_expr <- tcPragExpr False toplvl name if_expr
        ; let unf_src | stable    = InlineStable
                      | otherwise = InlineRhs
        ; return $ case mb_expr of
            Nothing -> NoUnfolding
            Just expr -> mkFinalUnfolding uf_opts unf_src strict_sig expr
        }
  where
    -- Strictness should occur before unfolding!
    strict_sig = dmdSigInfo info

tcUnfolding toplvl name _ _ (IfCompulsory if_expr)
  = do  { mb_expr <- tcPragExpr True toplvl name if_expr
        ; return (case mb_expr of
                    Nothing   -> NoUnfolding
                    Just expr -> mkCompulsoryUnfolding' expr) }

tcUnfolding toplvl name _ _ (IfInlineRule arity unsat_ok boring_ok if_expr)
  = do  { mb_expr <- tcPragExpr False toplvl name if_expr
        ; return (case mb_expr of
                    Nothing   -> NoUnfolding
                    Just expr -> mkCoreUnfolding InlineStable True expr guidance )}
  where
    guidance = UnfWhen { ug_arity = arity, ug_unsat_ok = unsat_ok, ug_boring_ok = boring_ok }

tcUnfolding _toplvl name dfun_ty _ (IfDFunUnfold bs ops)
  = bindIfaceBndrs bs $ \ bs' ->
    do { mb_ops1 <- forkM_maybe doc $ mapM tcIfaceExpr ops
       ; return (case mb_ops1 of
                    Nothing   -> noUnfolding
                    Just ops1 -> mkDFunUnfolding bs' (classDataCon cls) ops1) }
  where
    doc = text "Class ops for dfun" <+> ppr name
    (_, _, cls, _) = tcSplitDFunTy dfun_ty

{-
For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.
-}

tcPragExpr :: Bool  -- Is this unfolding compulsory?
                    -- See Note [Checking for representation polymorphism] in GHC.Core.Lint
           -> TopLevelFlag -> Name -> IfaceExpr -> IfL (Maybe CoreExpr)
tcPragExpr is_compulsory toplvl name expr
  = forkM_maybe doc $ do
    core_expr' <- tcIfaceExpr expr

    -- Check for type consistency in the unfolding
    -- See Note [Linting Unfoldings from Interfaces] in GHC.Core.Lint
    when (isTopLevel toplvl) $
      whenGOptM Opt_DoCoreLinting $ do
        in_scope <- get_in_scope
        dflags   <- getDynFlags
        logger   <- getLogger
        case lintUnfolding is_compulsory dflags noSrcLoc in_scope core_expr' of
          Nothing   -> return ()
          Just errs -> liftIO $
            displayLintResults logger False doc
                               (pprCoreExpr core_expr') (emptyBag, errs)
    return core_expr'
  where
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

tcIfaceGlobal :: Name -> IfL TyThing
tcIfaceGlobal name
  | Just thing <- wiredInNameTyThing_maybe name
        -- Wired-in things include TyCons, DataCons, and Ids
        -- Even though we are in an interface file, we want to make
        -- sure the instances and RULES of this thing (particularly TyCon) are loaded
        -- Imagine: f :: Double -> Double
  = do { ifCheckWiredInThing thing; return thing }

  | otherwise
  = do  { env <- getGblEnv
        ; cur_mod <- if_mod <$> getLclEnv
        ; case lookupKnotVars (if_rec_types env) (fromMaybe cur_mod (nameModule_maybe name))  of     -- Note [Tying the knot]
            Just get_type_env
                -> do           -- It's defined in a module in the hs-boot loop
                { type_env <- setLclEnv () get_type_env         -- yuk
                ; case lookupNameEnv type_env name of
                    Just thing -> return thing
                    -- See Note [Knot-tying fallback on boot]
                    Nothing   -> via_external
                }

            _ -> via_external }
  where
    via_external =  do
        { hsc_env <- getTopEnv
        ; mb_thing <- liftIO (lookupType hsc_env name)
        ; case mb_thing of {
            Just thing -> return thing ;
            Nothing    -> do

        { mb_thing <- importDecl name   -- It's imported; go get it
        ; case mb_thing of
            Failed err      -> failIfM err
            Succeeded thing -> return thing
        }}}

-- Note [Tying the knot]
-- ~~~~~~~~~~~~~~~~~~~~~
-- The if_rec_types field is used when we are compiling M.hs, which indirectly
-- imports Foo.hi, which mentions M.T Then we look up M.T in M's type
-- environment, which is splatted into if_rec_types after we've built M's type
-- envt.
--
-- This is a dark and complicated part of GHC type checking, with a lot
-- of moving parts.  Interested readers should also look at:
--
--      * Note [Knot-tying typecheckIface]
--      * Note [DFun knot-tying]
--      * Note [hsc_type_env_var hack]
--      * Note [Knot-tying fallback on boot]
--      * Note [Hydrating Modules]
--
-- There is also a wiki page on the subject, see:
--
--      https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/tying-the-knot

-- Note [Knot-tying fallback on boot]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Suppose that you are typechecking A.hs, which transitively imports,
-- via B.hs, A.hs-boot. When we poke on B.hs and discover that it
-- has a reference to a type T from A, what TyThing should we wire
-- it up with? Clearly, if we have already typechecked T and
-- added it into the type environment, we should go ahead and use that
-- type. But what if we haven't typechecked it yet?
--
-- For the longest time, GHC adopted the policy that this was
-- *an error condition*; that you MUST NEVER poke on B.hs's reference
-- to a T defined in A.hs until A.hs has gotten around to kind-checking
-- T and adding it to the env. However, actually ensuring this is the
-- case has proven to be a bug farm, because it's really difficult to
-- actually ensure this never happens. The problem was especially poignant
-- with type family consistency checks, which eagerly happen before any
-- typechecking takes place.
--
-- Today, we take a different strategy: if we ever try to access
-- an entity from A which doesn't exist, we just fall back on the
-- definition of A from the hs-boot file. This is complicated in
-- its own way: it means that you may end up with a mix of A.hs and
-- A.hs-boot TyThings during the course of typechecking.  We don't
-- think (and have not observed) any cases where this would cause
-- problems, but the hypothetical situation one might worry about
-- is something along these lines in Core:
--
--    case x of
--        A -> e1
--        B -> e2
--
-- If, when typechecking this, we find x :: T, and the T we are hooked
-- up with is the abstract one from the hs-boot file, rather than the
-- one defined in this module with constructors A and B.  But it's hard
-- to see how this could happen, especially because the reference to
-- the constructor (A and B) means that GHC will always typecheck
-- this expression *after* typechecking T.

tcIfaceTyCon :: IfaceTyCon -> IfL TyCon
tcIfaceTyCon (IfaceTyCon name info)
  = do { thing <- tcIfaceGlobal name
       ; return $ case ifaceTyConIsPromoted info of
           NotPromoted -> tyThingTyCon thing
           IsPromoted  -> promoteDataCon $ tyThingDataCon thing }

tcIfaceCoAxiom :: Name -> IfL (CoAxiom Branched)
tcIfaceCoAxiom name = do { thing <- tcIfaceImplicit name
                         ; return (tyThingCoAxiom thing) }


tcIfaceCoAxiomRule :: IfLclName -> IfL CoAxiomRule
-- Unlike CoAxioms, which arise from user 'type instance' declarations,
-- there are a fixed set of CoAxiomRules:
--   - axioms for type-level literals (Nat and Symbol),
--     enumerated in typeNatCoAxiomRules
tcIfaceCoAxiomRule n
  | Just ax <- lookupUFM typeNatCoAxiomRules n
  = return ax
  | otherwise
  = pprPanic "tcIfaceCoAxiomRule" (ppr n)

tcIfaceDataCon :: Name -> IfL DataCon
tcIfaceDataCon name = do { thing <- tcIfaceGlobal name
                         ; case thing of
                                AConLike (RealDataCon dc) -> return dc
                                _       -> pprPanic "tcIfaceDataCon" (ppr name$$ ppr thing) }

tcIfaceConLike :: Name -> IfL ConLike
tcIfaceConLike name = do { thing <- tcIfaceGlobal name
                         ; case thing of
                                AConLike cl -> return cl
                                _           -> pprPanic "tcIfaceConLike" (ppr name$$ ppr thing) }

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
  = do  { name <- newIfaceName (mkVarOccFS fs)
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
  = do  { name <- newIfaceName (mkTyVarOccFS occ)
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
