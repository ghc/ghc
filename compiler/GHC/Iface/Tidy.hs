
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{Tidying up Core}
-}

module GHC.Iface.Tidy (
       mkBootModDetailsTc, tidyProgram
   ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Env

import GHC.Tc.Types
import GHC.Tc.Utils.Env

import GHC.Core
import GHC.Core.Unfold
import GHC.Core.Unfold.Make
import GHC.Core.FVs
import GHC.Core.Tidy
import GHC.Core.Opt.Monad
import GHC.Core.Stats   (coreBindsStats, CoreStats(..))
import GHC.Core.Seq     (seqBinds)
import GHC.Core.Lint
import GHC.Core.Rules
import GHC.Core.Opt.Arity   ( exprArity, typeArity, exprBotStrictness_maybe )
import GHC.Core.InstEnv
import GHC.Core.Type     ( Type, tidyTopType )
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Class

import GHC.Iface.Tidy.StaticPtrTable
import GHC.Iface.Env

import GHC.Utils.Outputable
import GHC.Utils.Misc( filterOut )
import GHC.Utils.Panic
import GHC.Utils.Trace
import GHC.Utils.Logger as Logger
import qualified GHC.Utils.Error as Err

import GHC.Types.ForeignStubs
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.Id.Make ( mkDictSelRhs )
import GHC.Types.Id.Info
import GHC.Types.Demand  ( appIsDeadEnd, isTopSig, isDeadEndSig )
import GHC.Types.Cpr     ( mkCprSig, botCpr )
import GHC.Types.Basic
import GHC.Types.Name hiding (varName)
import GHC.Types.Name.Set
import GHC.Types.Name.Cache
import GHC.Types.Name.Ppr
import GHC.Types.Avail
import GHC.Types.Tickish
import GHC.Types.TypeEnv

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps

import GHC.Data.Maybe

import Control.Monad
import Data.Function
import Data.List        ( sortBy, mapAccumL )
import qualified Data.Set as S
import GHC.Platform.Ways
import GHC.Types.CostCentre

{-
Constructing the TypeEnv, Instances, Rules from which the
ModIface is constructed, and which goes on to subsequent modules in
--make mode.

Most of the interface file is obtained simply by serialising the
TypeEnv.  One important consequence is that if the *interface file*
has pragma info if and only if the final TypeEnv does. This is not so
important for *this* module, but it's essential for ghc --make:
subsequent compilations must not see (e.g.) the arity if the interface
file does not contain arity If they do, they'll exploit the arity;
then the arity might change, but the iface file doesn't change =>
recompilation does not happen => disaster.

For data types, the final TypeEnv will have a TyThing for the TyCon,
plus one for each DataCon; the interface file will contain just one
data type declaration, but it is de-serialised back into a collection
of TyThings.

************************************************************************
*                                                                      *
                Plan A: simpleTidyPgm
*                                                                      *
************************************************************************


Plan A: mkBootModDetails: omit pragmas, make interfaces small
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Ignore the bindings

* Drop all WiredIn things from the TypeEnv
        (we never want them in interface files)

* Retain all TyCons and Classes in the TypeEnv, to avoid
        having to find which ones are mentioned in the
        types of exported Ids

* Trim off the constructors of non-exported TyCons, both
        from the TyCon and from the TypeEnv

* Drop non-exported Ids from the TypeEnv

* Tidy the types of the DFunIds of Instances,
  make them into GlobalIds, (they already have External Names)
  and add them to the TypeEnv

* Tidy the types of the (exported) Ids in the TypeEnv,
  make them into GlobalIds (they already have External Names)

* Drop rules altogether

* Tidy the bindings, to ensure that the Arity
  information is correct for each top-level binder; the
  code generator needs it. And to ensure that local names have
  distinct OccNames in case of object-file splitting

* If this an hsig file, drop the instances altogether too (they'll
  get pulled in by the implicit module import.
-}

-- This is Plan A: make a small type env when typechecking only,
-- or when compiling a hs-boot file, or simply when not using -O
--
-- We don't look at the bindings at all -- there aren't any
-- for hs-boot files

mkBootModDetailsTc :: Logger -> TcGblEnv -> IO ModDetails
mkBootModDetailsTc logger
        TcGblEnv{ tcg_exports          = exports,
                  tcg_type_env         = type_env, -- just for the Ids
                  tcg_tcs              = tcs,
                  tcg_patsyns          = pat_syns,
                  tcg_insts            = insts,
                  tcg_fam_insts        = fam_insts,
                  tcg_complete_matches = complete_matches,
                  tcg_mod              = this_mod
                }
  = -- This timing isn't terribly useful since the result isn't forced, but
    -- the message is useful to locating oneself in the compilation process.
    Err.withTiming logger
                   (text "CoreTidy"<+>brackets (ppr this_mod))
                   (const ()) $
    return (ModDetails { md_types            = type_env'
                       , md_insts            = insts'
                       , md_fam_insts        = fam_insts
                       , md_rules            = []
                       , md_anns             = []
                       , md_exports          = exports
                       , md_complete_matches = complete_matches
                       })
  where
    -- Find the LocalIds in the type env that are exported
    -- Make them into GlobalIds, and tidy their types
    --
    -- It's very important to remove the non-exported ones
    -- because we don't tidy the OccNames, and if we don't remove
    -- the non-exported ones we'll get many things with the
    -- same name in the interface file, giving chaos.
    --
    -- Do make sure that we keep Ids that are already Global.
    -- When typechecking an .hs-boot file, the Ids come through as
    -- GlobalIds.
    final_ids = [ globaliseAndTidyBootId id
                | id <- typeEnvIds type_env
                , keep_it id ]

    final_tcs  = filterOut isWiredIn tcs
                 -- See Note [Drop wired-in things]
    type_env'  = typeEnvFromEntities final_ids final_tcs pat_syns fam_insts
    insts'     = mkFinalClsInsts type_env' insts

    -- Default methods have their export flag set (isExportedId),
    -- but everything else doesn't (yet), because this is
    -- pre-desugaring, so we must test against the exports too.
    keep_it id | isWiredInName id_name           = False
                 -- See Note [Drop wired-in things]
               | isExportedId id                 = True
               | id_name `elemNameSet` exp_names = True
               | otherwise                       = False
               where
                 id_name = idName id

    exp_names = availsToNameSet exports

lookupFinalId :: TypeEnv -> Id -> Id
lookupFinalId type_env id
  = case lookupTypeEnv type_env (idName id) of
      Just (AnId id') -> id'
      _ -> pprPanic "lookup_final_id" (ppr id)

mkFinalClsInsts :: TypeEnv -> [ClsInst] -> [ClsInst]
mkFinalClsInsts env = map (updateClsInstDFun (lookupFinalId env))

globaliseAndTidyBootId :: Id -> Id
-- For a LocalId with an External Name,
-- makes it into a GlobalId
--     * unchanged Name (might be Internal or External)
--     * unchanged details
--     * VanillaIdInfo (makes a conservative assumption about arity)
--     * BootUnfolding (see Note [Inlining and hs-boot files] in GHC.CoreToIface)
globaliseAndTidyBootId id
  = updateIdTypeAndMult tidyTopType (globaliseId id)
                   `setIdUnfolding` BootUnfolding

{-
************************************************************************
*                                                                      *
        Plan B: tidy bindings, make TypeEnv full of IdInfo
*                                                                      *
************************************************************************

Plan B: include pragmas, make interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Step 1: Figure out which Ids are externally visible
          See Note [Choosing external Ids]

* Step 2: Gather the externally visible rules, separately from
          the top-level bindings.
          See Note [Finding external rules]

* Step 3: Tidy the bindings, externalising appropriate Ids
          See Note [Tidy the top-level bindings]

* Drop all Ids from the TypeEnv, and add all the External Ids from
  the bindings.  (This adds their IdInfo to the TypeEnv; and adds
  floated-out Ids that weren't even in the TypeEnv before.)

Note [Choosing external Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also the section "Interface stability" in the
recompilation-avoidance commentary:
  https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance

First we figure out which Ids are "external" Ids.  An
"external" Id is one that is visible from outside the compilation
unit.  These are
  a) the user exported ones
  b) the ones bound to static forms
  c) ones mentioned in the unfoldings, workers, or
     rules of externally-visible ones

While figuring out which Ids are external, we pick a "tidy" OccName
for each one.  That is, we make its OccName distinct from the other
external OccNames in this module, so that in interface files and
object code we can refer to it unambiguously by its OccName.  The
OccName for each binder is prefixed by the name of the exported Id
that references it; e.g. if "f" references "x" in its unfolding, then
"x" is renamed to "f_x".  This helps distinguish the different "x"s
from each other, and means that if "f" is later removed, things that
depend on the other "x"s will not need to be recompiled.  Of course,
if there are multiple "f_x"s, then we have to disambiguate somehow; we
use "f_x0", "f_x1" etc.

As far as possible we should assign names in a deterministic fashion.
Each time this module is compiled with the same options, we should end
up with the same set of external names with the same types.  That is,
the ABI hash in the interface should not change.  This turns out to be
quite tricky, since the order of the bindings going into the tidy
phase is already non-deterministic, as it is based on the ordering of
Uniques, which are assigned unpredictably.

To name things in a stable way, we do a depth-first-search of the
bindings, starting from the exports sorted by name.  This way, as long
as the bindings themselves are deterministic (they sometimes aren't!),
the order in which they are presented to the tidying phase does not
affect the names we assign.

Note [Tidy the top-level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Next we traverse the bindings top to bottom.  For each *top-level*
binder

 1. Make it into a GlobalId; its IdDetails becomes VanillaGlobal,
    reflecting the fact that from now on we regard it as a global,
    not local, Id

 2. Give it a system-wide Unique.
    [Even non-exported things need system-wide Uniques because the
    byte-code generator builds a single Name->BCO symbol table.]

    We use the NameCache kept in the HscEnv as the
    source of such system-wide uniques.

    For external Ids, use the original-name cache in the NameCache
    to ensure that the unique assigned is the same as the Id had
    in any previous compilation run.

 3. Rename top-level Ids according to the names we chose in step 1.
    If it's an external Id, make it have a External Name, otherwise
    make it have an Internal Name.  This is used by the code generator
    to decide whether to make the label externally visible

 4. Give it its UTTERLY FINAL IdInfo; in ptic,
        * its unfolding, if it should have one

        * its arity, computed from the number of visible lambdas


Finally, substitute these new top-level binders consistently
throughout, including in unfoldings.  We also tidy binders in
RHSs, so that they print nicely in interfaces.

Note [Always expose compulsory unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must make absolutely sure that unsafeCoerce# is inlined. You might
think that giving it a compulsory unfolding is enough. However,
unsafeCoerce# is put in an interface file just like any other definition.
So, unless we take special precuations
- If we compiled Unsafe.Coerce with -O0, we might not put the unfolding
  into the interface file.
- If we compile a module M, that imports Unsafe.Coerce, with -O0 we might
  not read the unfolding out of the interface file.

So we need to take care, to ensure that Compulsory unfoldings are written
and read.  That makes sense: they are compulsory, after all. There are
three places this is actioned:

* GHC.Iface.Tidy.addExternal.  Export end: expose compulsory
  unfoldings, even with -O0.

* GHC.IfaceToCore.tcIdInfo.  Import end: when reading in from
  interface file, even with -O0 (fignore-interface-pragmas.)  we must
  load a compulsory unfolding
-}

tidyProgram :: HscEnv -> ModGuts -> IO (CgGuts, ModDetails)
tidyProgram hsc_env  (ModGuts { mg_module           = mod
                              , mg_exports          = exports
                              , mg_rdr_env          = rdr_env
                              , mg_tcs              = tcs
                              , mg_insts            = cls_insts
                              , mg_fam_insts        = fam_insts
                              , mg_binds            = binds
                              , mg_patsyns          = patsyns
                              , mg_rules            = imp_rules
                              , mg_anns             = anns
                              , mg_complete_matches = complete_matches
                              , mg_deps             = deps
                              , mg_foreign          = foreign_stubs
                              , mg_foreign_files    = foreign_files
                              , mg_hpc_info         = hpc_info
                              , mg_modBreaks        = modBreaks
                              })

  = Err.withTiming logger
                   (text "CoreTidy"<+>brackets (ppr mod))
                   (const ()) $
    do  { let { omit_prags = gopt Opt_OmitInterfacePragmas dflags
              ; expose_all = gopt Opt_ExposeAllUnfoldings  dflags
              ; print_unqual = mkPrintUnqualified (hsc_unit_env hsc_env) rdr_env
              ; implicit_binds = concatMap getImplicitBinds tcs
              }

        ; (unfold_env, tidy_occ_env)
              <- chooseExternalIds hsc_env mod omit_prags expose_all
                                   binds implicit_binds imp_rules
        ; let { (trimmed_binds, trimmed_rules)
                    = findExternalRules omit_prags binds imp_rules unfold_env }

        ; let uf_opts = unfoldingOpts dflags
        ; (tidy_env, tidy_binds)
                 <- tidyTopBinds uf_opts unfold_env tidy_occ_env trimmed_binds

          -- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
        ; (spt_entries, tidy_binds') <-
             sptCreateStaticBinds hsc_env mod tidy_binds
        ; let { platform = targetPlatform (hsc_dflags hsc_env)
              ; spt_init_code = sptModuleInitCode platform mod spt_entries
              ; add_spt_init_code =
                  case backend dflags of
                    -- If we are compiling for the interpreter we will insert
                    -- any necessary SPT entries dynamically
                    Interpreter -> id
                    -- otherwise add a C stub to do so
                    _              -> (`appendStubC` spt_init_code)

              -- The completed type environment is gotten from
              --      a) the types and classes defined here (plus implicit things)
              --      b) adding Ids with correct IdInfo, including unfoldings,
              --              gotten from the bindings
              -- From (b) we keep only those Ids with External names;
              --          the CoreTidy pass makes sure these are all and only
              --          the externally-accessible ones
              -- This truncates the type environment to include only the
              -- exported Ids and things needed from them, which saves space
              --
              -- See Note [Don't attempt to trim data types]
              ; final_ids  = [ trimId omit_prags id
                             | id <- bindersOfBinds tidy_binds
                             , isExternalName (idName id)
                             , not (isWiredIn id)
                             ]   -- See Note [Drop wired-in things]

              ; final_tcs      = filterOut isWiredIn tcs
                                 -- See Note [Drop wired-in things]
              ; tidy_type_env  = typeEnvFromEntities final_ids final_tcs patsyns fam_insts
              ; tidy_cls_insts = mkFinalClsInsts tidy_type_env cls_insts
              ; tidy_rules     = tidyRules tidy_env trimmed_rules

              ; -- See Note [Injecting implicit bindings]
                all_tidy_binds = implicit_binds ++ tidy_binds'

              -- Get the TyCons to generate code for.  Careful!  We must use
              -- the untidied TyCons here, because we need
              --  (a) implicit TyCons arising from types and classes defined
              --      in this module
              --  (b) wired-in TyCons, which are normally removed from the
              --      TypeEnv we put in the ModDetails
              --  (c) Constructors even if they are not exported (the
              --      tidied TypeEnv has trimmed these away)
              ; alg_tycons = filter isAlgTyCon tcs


              ; local_ccs
                  | ways dflags `hasWay` WayProf
                        = collectCostCentres mod all_tidy_binds tidy_rules
                  | otherwise
                        = S.empty
              }

        ; endPassIO hsc_env print_unqual CoreTidy all_tidy_binds tidy_rules

          -- If the endPass didn't print the rules, but ddump-rules is
          -- on, print now
        ; unless (logHasDumpFlag logger Opt_D_dump_simpl) $
            Logger.putDumpFileMaybe logger Opt_D_dump_rules
              (showSDoc dflags (ppr CoreTidy <+> text "rules"))
              FormatText
              (pprRulesForUser tidy_rules)

          -- Print one-line size info
        ; let cs = coreBindsStats tidy_binds
        ; Logger.putDumpFileMaybe logger Opt_D_dump_core_stats "Core Stats"
            FormatText
            (text "Tidy size (terms,types,coercions)"
             <+> ppr (moduleName mod) <> colon
             <+> int (cs_tm cs)
             <+> int (cs_ty cs)
             <+> int (cs_co cs) )

        ; return (CgGuts { cg_module   = mod,
                           cg_tycons   = alg_tycons,
                           cg_binds    = all_tidy_binds,
                           cg_ccs      = S.toList local_ccs,
                           cg_foreign  = add_spt_init_code foreign_stubs,
                           cg_foreign_files = foreign_files,
                           cg_dep_pkgs = dep_direct_pkgs deps,
                           cg_hpc_info = hpc_info,
                           cg_modBreaks = modBreaks,
                           cg_spt_entries = spt_entries },

                   ModDetails { md_types            = tidy_type_env,
                                md_rules            = tidy_rules,
                                md_insts            = tidy_cls_insts,
                                md_fam_insts        = fam_insts,
                                md_exports          = exports,
                                md_anns             = anns,      -- are already tidy
                                md_complete_matches = complete_matches
                              })
        }
  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env


------------------------------------------------------------------------------
-- Collecting cost centres
-- ---------------------------------------------------------------------------

-- | Collect cost centres defined in the current module, including those in
-- unfoldings.
collectCostCentres :: Module -> CoreProgram -> [CoreRule] -> S.Set CostCentre
collectCostCentres mod_name binds rules
  = foldl' go_bind (go_rules S.empty) binds
  where
    go cs e = case e of
      Var{} -> cs
      Lit{} -> cs
      App e1 e2 -> go (go cs e1) e2
      Lam _ e -> go cs e
      Let b e -> go (go_bind cs b) e
      Case scrt _ _ alts -> go_alts (go cs scrt) alts
      Cast e _ -> go cs e
      Tick (ProfNote cc _ _) e ->
        go (if ccFromThisModule cc mod_name then S.insert cc cs else cs) e
      Tick _ e -> go cs e
      Type{} -> cs
      Coercion{} -> cs

    go_alts = foldl' (\cs (Alt _con _bndrs e) -> go cs e)

    go_bind :: S.Set CostCentre -> CoreBind -> S.Set CostCentre
    go_bind cs (NonRec b e) =
      go (do_binder cs b) e
    go_bind cs (Rec bs) =
      foldl' (\cs' (b, e) -> go (do_binder cs' b) e) cs bs

    do_binder cs b = maybe cs (go cs) (get_unf b)


    -- Unfoldings may have cost centres that in the original definion are
    -- optimized away, see #5889.
    get_unf = maybeUnfoldingTemplate . realIdUnfolding

    -- Have to look at the RHS of rules as well, as these may contain ticks which
    -- don't appear anywhere else. See #19894
    go_rules cs = foldl' go cs (mapMaybe get_rhs rules)

    get_rhs Rule { ru_rhs } = Just ru_rhs
    get_rhs BuiltinRule {} = Nothing

--------------------------
trimId :: Bool -> Id -> Id
-- With -O0 we now trim off the arity, one-shot-ness, strictness
-- etc which tidyTopIdInfo retains for the benefit of the code generator
-- but which we don't want in the interface file or ModIface for
-- downstream compilations
trimId omit_prags id
  | omit_prags, not (isImplicitId id)
  = id `setIdInfo`      vanillaIdInfo
       `setIdUnfolding` idUnfolding id
       -- We respect the final unfolding chosen by tidyTopIdInfo.
       -- We have already trimmed it if we don't want it for -O0;
       -- see also Note [Always expose compulsory unfoldings]

  | otherwise   -- No trimming
  = id

{- Note [Drop wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never put wired-in TyCons or Ids in an interface file.
They are wired-in, so the compiler knows about them already.

Note [Don't attempt to trim data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For some time GHC tried to avoid exporting the data constructors
of a data type if it wasn't strictly necessary to do so; see #835.
But "strictly necessary" accumulated a longer and longer list
of exceptions, and finally I gave up the battle:

    commit 9a20e540754fc2af74c2e7392f2786a81d8d5f11
    Author: Simon Peyton Jones <simonpj@microsoft.com>
    Date:   Thu Dec 6 16:03:16 2012 +0000

    Stop attempting to "trim" data types in interface files

    Without -O, we previously tried to make interface files smaller
    by not including the data constructors of data types.  But
    there are a lot of exceptions, notably when Template Haskell is
    involved or, more recently, DataKinds.

    However #7445 shows that even without TemplateHaskell, using
    the Data class and invoking Language.Haskell.TH.Quote.dataToExpQ
    is enough to require us to expose the data constructors.

    So I've given up on this "optimisation" -- it's probably not
    important anyway.  Now I'm simply not attempting to trim off
    the data constructors.  The gain in simplicity is worth the
    modest cost in interface file growth, which is limited to the
    bits reqd to describe those data constructors.

************************************************************************
*                                                                      *
        Implicit bindings
*                                                                      *
************************************************************************

Note [Injecting implicit bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We inject the implicit bindings right at the end, in GHC.Core.Tidy.
Some of these bindings, notably record selectors, are not
constructed in an optimised form.  E.g. record selector for
        data T = MkT { x :: {-# UNPACK #-} !Int }
Then the unfolding looks like
        x = \t. case t of MkT x1 -> let x = I# x1 in x
This generates bad code unless it's first simplified a bit.  That is
why GHC.Core.Unfold.mkImplicitUnfolding uses simpleOptExpr to do a bit of
optimisation first.  (Only matters when the selector is used curried;
eg map x ys.)  See #2070.

[Oct 09: in fact, record selectors are no longer implicit Ids at all,
because we really do want to optimise them properly. They are treated
much like any other Id.  But doing "light" optimisation on an implicit
Id still makes sense.]

At one time I tried injecting the implicit bindings *early*, at the
beginning of SimplCore.  But that gave rise to real difficulty,
because GlobalIds are supposed to have *fixed* IdInfo, but the
simplifier and other core-to-core passes mess with IdInfo all the
time.  The straw that broke the camels back was when a class selector
got the wrong arity -- ie the simplifier gave it arity 2, whereas
importing modules were expecting it to have arity 1 (#2844).
It's much safer just to inject them right at the end, after tidying.

Oh: two other reasons for injecting them late:

  - If implicit Ids are already in the bindings when we start tidying,
    we'd have to be careful not to treat them as external Ids (in
    the sense of chooseExternalIds); else the Ids mentioned in *their*
    RHSs will be treated as external and you get an interface file
    saying      a18 = <blah>
    but nothing referring to a18 (because the implicit Id is the
    one that does, and implicit Ids don't appear in interface files).

  - More seriously, the tidied type-envt will include the implicit
    Id replete with a18 in its unfolding; but we won't take account
    of a18 when computing a fingerprint for the class; result chaos.

There is one sort of implicit binding that is injected still later,
namely those for data constructor workers. Reason (I think): it's
really just a code generation trick.... binding itself makes no sense.
See Note [Data constructor workers] in "GHC.CoreToStg.Prep".
-}

getImplicitBinds :: TyCon -> [CoreBind]
getImplicitBinds tc = cls_binds ++ getTyConImplicitBinds tc
  where
    cls_binds = maybe [] getClassImplicitBinds (tyConClass_maybe tc)

getTyConImplicitBinds :: TyCon -> [CoreBind]
getTyConImplicitBinds tc
  | isNewTyCon tc = []  -- See Note [Compulsory newtype unfolding] in GHC.Types.Id.Make
  | otherwise     = map get_defn (mapMaybe dataConWrapId_maybe (tyConDataCons tc))

getClassImplicitBinds :: Class -> [CoreBind]
getClassImplicitBinds cls
  = [ NonRec op (mkDictSelRhs cls val_index)
    | (op, val_index) <- classAllSelIds cls `zip` [0..] ]

get_defn :: Id -> CoreBind
get_defn id = NonRec id (unfoldingTemplate (realIdUnfolding id))

{-
************************************************************************
*                                                                      *
\subsection{Step 1: finding externals}
*                                                                      *
************************************************************************

See Note [Choosing external Ids].
-}

type UnfoldEnv  = IdEnv (Name{-new name-}, Bool {-show unfolding-})
  -- Maps each top-level Id to its new Name (the Id is tidied in step 2)
  -- The Unique is unchanged.  If the new Name is external, it will be
  -- visible in the interface file.
  --
  -- Bool => expose unfolding or not.

chooseExternalIds :: HscEnv
                  -> Module
                  -> Bool -> Bool
                  -> [CoreBind]
                  -> [CoreBind]
                  -> [CoreRule]
                  -> IO (UnfoldEnv, TidyOccEnv)
                  -- Step 1 from the notes above

chooseExternalIds hsc_env mod omit_prags expose_all binds implicit_binds imp_id_rules
  = do { (unfold_env1,occ_env1) <- search init_work_list emptyVarEnv init_occ_env
       ; let internal_ids = filter (not . (`elemVarEnv` unfold_env1)) binders
       ; tidy_internal internal_ids unfold_env1 occ_env1 }
 where
  name_cache = hsc_NC hsc_env

  -- init_ext_ids is the initial list of Ids that should be
  -- externalised.  It serves as the starting point for finding a
  -- deterministic, tidy, renaming for all external Ids in this
  -- module.
  --
  -- It is sorted, so that it has a deterministic order (i.e. it's the
  -- same list every time this module is compiled), in contrast to the
  -- bindings, which are ordered non-deterministically.
  init_work_list = zip init_ext_ids init_ext_ids
  init_ext_ids   = sortBy (compare `on` getOccName) $ filter is_external binders

  -- An Id should be external if either (a) it is exported,
  -- (b) it appears in the RHS of a local rule for an imported Id, or
  -- See Note [Which rules to expose]
  is_external id = isExportedId id || id `elemVarSet` rule_rhs_vars

  rule_rhs_vars
    -- No rules are exposed when omit_prags is enabled see #19836
    -- imp_id_rules are the RULES in /this/ module for /imported/ Ids
    -- If omit_prags is True, these rules won't be put in the interface file.
    -- But if omit_prags is False, so imp_id_rules are in the interface file for
    -- this module, then the local-defined Ids they use must be made external.
    | omit_prags = emptyVarSet
    | otherwise = mapUnionVarSet ruleRhsFreeVars imp_id_rules

  binders          = map fst $ flattenBinds binds
  implicit_binders = bindersOfBinds implicit_binds
  binder_set       = mkVarSet binders

  avoids   = [getOccName name | bndr <- binders ++ implicit_binders,
                                let name = idName bndr,
                                isExternalName name ]
                -- In computing our "avoids" list, we must include
                --      all implicit Ids
                --      all things with global names (assigned once and for
                --                                      all by the renamer)
                -- since their names are "taken".
                -- The type environment is a convenient source of such things.
                -- In particular, the set of binders doesn't include
                -- implicit Ids at this stage.

        -- We also make sure to avoid any exported binders.  Consider
        --      f{-u1-} = 1     -- Local decl
        --      ...
        --      f{-u2-} = 2     -- Exported decl
        --
        -- The second exported decl must 'get' the name 'f', so we
        -- have to put 'f' in the avoids list before we get to the first
        -- decl.  tidyTopId then does a no-op on exported binders.
  init_occ_env = initTidyOccEnv avoids


  search :: [(Id,Id)]    -- The work-list: (external id, referring id)
                         -- Make a tidy, external Name for the external id,
                         --   add it to the UnfoldEnv, and do the same for the
                         --   transitive closure of Ids it refers to
                         -- The referring id is used to generate a tidy
                         ---  name for the external id
         -> UnfoldEnv    -- id -> (new Name, show_unfold)
         -> TidyOccEnv   -- occ env for choosing new Names
         -> IO (UnfoldEnv, TidyOccEnv)

  search [] unfold_env occ_env = return (unfold_env, occ_env)

  search ((idocc,referrer) : rest) unfold_env occ_env
    | idocc `elemVarEnv` unfold_env = search rest unfold_env occ_env
    | otherwise = do
      (occ_env', name') <- tidyTopName mod name_cache (Just referrer) occ_env idocc
      let
          (new_ids, show_unfold) = addExternal omit_prags expose_all refined_id

                -- 'idocc' is an *occurrence*, but we need to see the
                -- unfolding in the *definition*; so look up in binder_set
          refined_id = case lookupVarSet binder_set idocc of
                         Just id -> id
                         Nothing -> warnPprTrace True (ppr idocc) idocc

          unfold_env' = extendVarEnv unfold_env idocc (name',show_unfold)
          referrer' | isExportedId refined_id = refined_id
                    | otherwise               = referrer
      --
      search (zip new_ids (repeat referrer') ++ rest) unfold_env' occ_env'

  tidy_internal :: [Id] -> UnfoldEnv -> TidyOccEnv
                -> IO (UnfoldEnv, TidyOccEnv)
  tidy_internal []       unfold_env occ_env = return (unfold_env,occ_env)
  tidy_internal (id:ids) unfold_env occ_env = do
      (occ_env', name') <- tidyTopName mod name_cache Nothing occ_env id
      let unfold_env' = extendVarEnv unfold_env id (name',False)
      tidy_internal ids unfold_env' occ_env'

addExternal :: Bool -> Bool -> Id -> ([Id], Bool)
addExternal omit_prags expose_all id
  | omit_prags
  , not (isCompulsoryUnfolding unfolding)
  = ([], False)  -- See Note [Always expose compulsory unfoldings]
                 -- in GHC.HsToCore

  | otherwise
  = (new_needed_ids, show_unfold)

  where
    new_needed_ids = bndrFvsInOrder show_unfold id
    idinfo         = idInfo id
    unfolding      = realUnfoldingInfo idinfo
    show_unfold    = show_unfolding unfolding
    never_active   = isNeverActive (inlinePragmaActivation (inlinePragInfo idinfo))
    loop_breaker   = isStrongLoopBreaker (occInfo idinfo)
    bottoming_fn   = isDeadEndSig (dmdSigInfo idinfo)

        -- Stuff to do with the Id's unfolding
        -- We leave the unfolding there even if there is a worker
        -- In GHCi the unfolding is used by importers

    show_unfolding (CoreUnfolding { uf_src = src, uf_guidance = guidance })
       =  expose_all         -- 'expose_all' says to expose all
                             -- unfoldings willy-nilly

       || isStableSource src     -- Always expose things whose
                                 -- source is an inline rule

       || not dont_inline
       where
         dont_inline
            | never_active = True   -- Will never inline
            | loop_breaker = True   -- Ditto
            | otherwise    = case guidance of
                                UnfWhen {}       -> False
                                UnfIfGoodArgs {} -> bottoming_fn
                                UnfNever {}      -> True
         -- bottoming_fn: don't inline bottoming functions, unless the
         -- RHS is very small or trivial (UnfWhen), in which case we
         -- may as well do so For example, a cast might cancel with
         -- the call site.

    show_unfolding (DFunUnfolding {}) = True
    show_unfolding _                  = False

{-
************************************************************************
*                                                                      *
               Deterministic free variables
*                                                                      *
************************************************************************

We want a deterministic free-variable list.  exprFreeVars gives us
a VarSet, which is in a non-deterministic order when converted to a
list.  Hence, here we define a free-variable finder that returns
the free variables in the order that they are encountered.

See Note [Choosing external Ids]
-}

bndrFvsInOrder :: Bool -> Id -> [Id]
bndrFvsInOrder show_unfold id
  = run (dffvLetBndr show_unfold id)

run :: DFFV () -> [Id]
run (DFFV m) = case m emptyVarSet (emptyVarSet, []) of
                 ((_,ids),_) -> ids

newtype DFFV a
  = DFFV (VarSet              -- Envt: non-top-level things that are in scope
                              -- we don't want to record these as free vars
      -> (VarSet, [Var])      -- Input State: (set, list) of free vars so far
      -> ((VarSet,[Var]),a))  -- Output state
    deriving (Functor)

instance Applicative DFFV where
    pure a = DFFV $ \_ st -> (st, a)
    (<*>) = ap

instance Monad DFFV where
  (DFFV m) >>= k = DFFV $ \env st ->
    case m env st of
       (st',a) -> case k a of
                     DFFV f -> f env st'

extendScope :: Var -> DFFV a -> DFFV a
extendScope v (DFFV f) = DFFV (\env st -> f (extendVarSet env v) st)

extendScopeList :: [Var] -> DFFV a -> DFFV a
extendScopeList vs (DFFV f) = DFFV (\env st -> f (extendVarSetList env vs) st)

insert :: Var -> DFFV ()
insert v = DFFV $ \ env (set, ids) ->
           let keep_me = isLocalId v &&
                         not (v `elemVarSet` env) &&
                           not (v `elemVarSet` set)
           in if keep_me
              then ((extendVarSet set v, v:ids), ())
              else ((set,                ids),   ())


dffvExpr :: CoreExpr -> DFFV ()
dffvExpr (Var v)              = insert v
dffvExpr (App e1 e2)          = dffvExpr e1 >> dffvExpr e2
dffvExpr (Lam v e)            = extendScope v (dffvExpr e)
dffvExpr (Tick (Breakpoint _ _ ids) e) = mapM_ insert ids >> dffvExpr e
dffvExpr (Tick _other e)    = dffvExpr e
dffvExpr (Cast e _)           = dffvExpr e
dffvExpr (Let (NonRec x r) e) = dffvBind (x,r) >> extendScope x (dffvExpr e)
dffvExpr (Let (Rec prs) e)    = extendScopeList (map fst prs) $
                                (mapM_ dffvBind prs >> dffvExpr e)
dffvExpr (Case e b _ as)      = dffvExpr e >> extendScope b (mapM_ dffvAlt as)
dffvExpr _other               = return ()

dffvAlt :: CoreAlt -> DFFV ()
dffvAlt (Alt _ xs r) = extendScopeList xs (dffvExpr r)

dffvBind :: (Id, CoreExpr) -> DFFV ()
dffvBind(x,r)
  | not (isId x) = dffvExpr r
  | otherwise    = dffvLetBndr False x >> dffvExpr r
                -- Pass False because we are doing the RHS right here
                -- If you say True you'll get *exponential* behaviour!

dffvLetBndr :: Bool -> Id -> DFFV ()
-- Gather the free vars of the RULES and unfolding of a binder
-- We always get the free vars of a *stable* unfolding, but
-- for a *vanilla* one (InlineRhs), the flag controls what happens:
--   True <=> get fvs of even a *vanilla* unfolding
--   False <=> ignore an InlineRhs
-- For nested bindings (call from dffvBind) we always say "False" because
--       we are taking the fvs of the RHS anyway
-- For top-level bindings (call from addExternal, via bndrFvsInOrder)
--       we say "True" if we are exposing that unfolding
dffvLetBndr vanilla_unfold id
  = do { go_unf (realUnfoldingInfo idinfo)
       ; mapM_ go_rule (ruleInfoRules (ruleInfo idinfo)) }
  where
    idinfo = idInfo id

    go_unf (CoreUnfolding { uf_tmpl = rhs, uf_src = src })
       = case src of
           InlineRhs | vanilla_unfold -> dffvExpr rhs
                     | otherwise      -> return ()
           _                          -> dffvExpr rhs

    go_unf (DFunUnfolding { df_bndrs = bndrs, df_args = args })
             = extendScopeList bndrs $ mapM_ dffvExpr args
    go_unf _ = return ()

    go_rule (BuiltinRule {}) = return ()
    go_rule (Rule { ru_bndrs = bndrs, ru_rhs = rhs })
      = extendScopeList bndrs (dffvExpr rhs)

{-
************************************************************************
*                                                                      *
               findExternalRules
*                                                                      *
************************************************************************

Note [Finding external rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The complete rules are gotten by combining
   a) local rules for imported Ids
   b) rules embedded in the top-level Ids

There are two complications:
  * Note [Which rules to expose]
  * Note [Trimming auto-rules]

Note [Which rules to expose]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function 'expose_rule' filters out rules that mention, on the LHS,
Ids that aren't externally visible; these rules can't fire in a client
module.

The externally-visible binders are computed (by chooseExternalIds)
assuming that all orphan rules are externalised (see init_ext_ids in
function 'search'). So in fact it's a bit conservative and we may
export more than we need.  (It's a sort of mutual recursion.)

Note [Trimming auto-rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Second, with auto-specialisation we may specialise local or imported
dfuns or INLINE functions, and then later inline them.  That may leave
behind something like
   RULE "foo" forall d. f @ Int d = f_spec
where f is either local or imported, and there is no remaining
reference to f_spec except from the RULE.

Now that RULE *might* be useful to an importing module, but that is
purely speculative, and meanwhile the code is taking up space and
codegen time.  I found that binary sizes jumped by 6-10% when I
started to specialise INLINE functions (again, Note [Inline
specialisations] in GHC.Core.Opt.Specialise).

So it seems better to drop the binding for f_spec, and the rule
itself, if the auto-generated rule is the *only* reason that it is
being kept alive.

(The RULE still might have been useful in the past; that is, it was
the right thing to have generated it in the first place.  See Note
[Inline specialisations] in GHC.Core.Opt.Specialise. But now it has
served its purpose, and can be discarded.)

So findExternalRules does this:
  * Remove all bindings that are kept alive *only* by isAutoRule rules
      (this is done in trim_binds)
  * Remove all auto rules that mention bindings that have been removed
      (this is done by filtering by keep_rule)

NB: if a binding is kept alive for some *other* reason (e.g. f_spec is
called in the final code), we keep the rule too.

This stuff is the only reason for the ru_auto field in a Rule.
-}

findExternalRules :: Bool       -- Omit pragmas
                  -> [CoreBind]
                  -> [CoreRule] -- Local rules for imported fns
                  -> UnfoldEnv  -- Ids that are exported, so we need their rules
                  -> ([CoreBind], [CoreRule])
-- See Note [Finding external rules]
findExternalRules omit_prags binds imp_id_rules unfold_env
  = (trimmed_binds, filter keep_rule all_rules)
  where
    imp_rules         = filter expose_rule imp_id_rules
    imp_user_rule_fvs = mapUnionVarSet user_rule_rhs_fvs imp_rules

    user_rule_rhs_fvs rule | isAutoRule rule = emptyVarSet
                           | otherwise       = ruleRhsFreeVars rule

    (trimmed_binds, local_bndrs, _, all_rules) = trim_binds binds

    keep_rule rule = ruleFreeVars rule `subVarSet` local_bndrs
        -- Remove rules that make no sense, because they mention a
        -- local binder (on LHS or RHS) that we have now discarded.
        -- (NB: ruleFreeVars only includes LocalIds)
        --
        -- LHS: we have already filtered out rules that mention internal Ids
        --     on LHS but that isn't enough because we might have by now
        --     discarded a binding with an external Id. (How?
        --     chooseExternalIds is a bit conservative.)
        --
        -- RHS: the auto rules that might mention a binder that has
        --      been discarded; see Note [Trimming auto-rules]

    expose_rule rule
        | omit_prags = False
        | otherwise  = all is_external_id (ruleLhsFreeIdsList rule)
                -- Don't expose a rule whose LHS mentions a locally-defined
                -- Id that is completely internal (i.e. not visible to an
                -- importing module).  NB: ruleLhsFreeIds only returns LocalIds.
                -- See Note [Which rules to expose]

    is_external_id id = case lookupVarEnv unfold_env id of
                          Just (name, _) -> isExternalName name
                          Nothing        -> False

    trim_binds :: [CoreBind]
               -> ( [CoreBind]   -- Trimmed bindings
                  , VarSet       -- Binders of those bindings
                  , VarSet       -- Free vars of those bindings + rhs of user rules
                                 -- (we don't bother to delete the binders)
                  , [CoreRule])  -- All rules, imported + from the bindings
    -- This function removes unnecessary bindings, and gathers up rules from
    -- the bindings we keep.  See Note [Trimming auto-rules]
    trim_binds []  -- Base case, start with imp_user_rule_fvs
       = ([], emptyVarSet, imp_user_rule_fvs, imp_rules)

    trim_binds (bind:binds)
       | any needed bndrs    -- Keep binding
       = ( bind : binds', bndr_set', needed_fvs', local_rules ++ rules )
       | otherwise           -- Discard binding altogether
       = stuff
       where
         stuff@(binds', bndr_set, needed_fvs, rules)
                       = trim_binds binds
         needed bndr   = isExportedId bndr || bndr `elemVarSet` needed_fvs

         bndrs         = bindersOf  bind
         rhss          = rhssOfBind bind
         bndr_set'     = bndr_set `extendVarSetList` bndrs

         needed_fvs'   = needed_fvs                                   `unionVarSet`
                         mapUnionVarSet idUnfoldingVars   bndrs       `unionVarSet`
                              -- Ignore type variables in the type of bndrs
                         mapUnionVarSet exprFreeVars      rhss        `unionVarSet`
                         mapUnionVarSet user_rule_rhs_fvs local_rules
            -- In needed_fvs', we don't bother to delete binders from the fv set

         local_rules  = [ rule
                        | id <- bndrs
                        , is_external_id id   -- Only collect rules for external Ids
                        , rule <- idCoreRules id
                        , expose_rule rule ]  -- and ones that can fire in a client

{-
************************************************************************
*                                                                      *
               tidyTopName
*                                                                      *
************************************************************************

This is where we set names to local/global based on whether they really are
externally visible (see comment at the top of this module).  If the name
was previously local, we have to give it a unique occurrence name if
we intend to externalise it.
-}

tidyTopName :: Module -> NameCache -> Maybe Id -> TidyOccEnv
            -> Id -> IO (TidyOccEnv, Name)
tidyTopName mod name_cache maybe_ref occ_env id
  | global && internal = return (occ_env, localiseName name)

  | global && external = return (occ_env, name)
        -- Global names are assumed to have been allocated by the renamer,
        -- so they already have the "right" unique
        -- And it's a system-wide unique too

  -- Now we get to the real reason that all this is in the IO Monad:
  -- we have to update the name cache in a nice atomic fashion

  | local  && internal = do uniq <- takeUniqFromNameCache name_cache
                            let new_local_name = mkInternalName uniq occ' loc
                            return (occ_env', new_local_name)
        -- Even local, internal names must get a unique occurrence, because
        -- if we do -split-objs we externalise the name later, in the code generator
        --
        -- Similarly, we must make sure it has a system-wide Unique, because
        -- the byte-code generator builds a system-wide Name->BCO symbol table

  | local  && external = do new_external_name <- allocateGlobalBinder name_cache mod occ' loc
                            return (occ_env', new_external_name)
        -- If we want to externalise a currently-local name, check
        -- whether we have already assigned a unique for it.
        -- If so, use it; if not, extend the table.
        -- All this is done by allocateGlobalBinder.
        -- This is needed when *re*-compiling a module in GHCi; we must
        -- use the same name for externally-visible things as we did before.

  | otherwise = panic "tidyTopName"
  where
    name        = idName id
    external    = isJust maybe_ref
    global      = isExternalName name
    local       = not global
    internal    = not external
    loc         = nameSrcSpan name

    old_occ     = nameOccName name
    new_occ | Just ref <- maybe_ref
            , ref /= id
            = mkOccName (occNameSpace old_occ) $
                   let
                       ref_str = occNameString (getOccName ref)
                       occ_str = occNameString old_occ
                   in
                   case occ_str of
                     '$':'w':_ -> occ_str
                        -- workers: the worker for a function already
                        -- includes the occname for its parent, so there's
                        -- no need to prepend the referrer.
                     _other | isSystemName name -> ref_str
                            | otherwise         -> ref_str ++ '_' : occ_str
                        -- If this name was system-generated, then don't bother
                        -- to retain its OccName, just use the referrer.  These
                        -- system-generated names will become "f1", "f2", etc. for
                        -- a referrer "f".
            | otherwise = old_occ

    (occ_env', occ') = tidyOccName occ_env new_occ


{-
************************************************************************
*                                                                      *
\subsection{Step 2: top-level tidying}
*                                                                      *
************************************************************************
-}

-- TopTidyEnv: when tidying we need to know
--   * name_cache: The NameCache, containing a unique supply and any pre-ordained Names.
--        These may have arisen because the
--        renamer read in an interface file mentioning M.$wf, say,
--        and assigned it unique r77.  If, on this compilation, we've
--        invented an Id whose name is $wf (but with a different unique)
--        we want to rename it to have unique r77, so that we can do easy
--        comparisons with stuff from the interface file
--
--   * occ_env: The TidyOccEnv, which tells us which local occurrences
--     are 'used'
--
--   * subst_env: A Var->Var mapping that substitutes the new Var for the old

tidyTopBinds :: UnfoldingOpts
             -> UnfoldEnv
             -> TidyOccEnv
             -> CoreProgram
             -> IO (TidyEnv, CoreProgram)

tidyTopBinds uf_opts unfold_env init_occ_env binds
  = do let result = tidy init_env binds
       seqBinds (snd result) `seq` return result
       -- This seqBinds avoids a spike in space usage (see #13564)
  where
    init_env = (init_occ_env, emptyVarEnv)

    tidy = mapAccumL (tidyTopBind uf_opts unfold_env)

------------------------
tidyTopBind  :: UnfoldingOpts
             -> UnfoldEnv
             -> TidyEnv
             -> CoreBind
             -> (TidyEnv, CoreBind)

tidyTopBind uf_opts unfold_env
            (occ_env,subst1) (NonRec bndr rhs)
  = (tidy_env2,  NonRec bndr' rhs')
  where
    Just (name',show_unfold) = lookupVarEnv unfold_env bndr
    (bndr', rhs') = tidyTopPair uf_opts show_unfold tidy_env2 name' (bndr, rhs)
    subst2        = extendVarEnv subst1 bndr bndr'
    tidy_env2     = (occ_env, subst2)

tidyTopBind uf_opts unfold_env (occ_env, subst1) (Rec prs)
  = (tidy_env2, Rec prs')
  where
    prs' = [ tidyTopPair uf_opts show_unfold tidy_env2 name' (id,rhs)
           | (id,rhs) <- prs,
             let (name',show_unfold) =
                    expectJust "tidyTopBind" $ lookupVarEnv unfold_env id
           ]

    subst2    = extendVarEnvList subst1 (bndrs `zip` map fst prs')
    tidy_env2 = (occ_env, subst2)

    bndrs = map fst prs

-----------------------------------------------------------
tidyTopPair :: UnfoldingOpts
            -> Bool  -- show unfolding
            -> TidyEnv  -- The TidyEnv is used to tidy the IdInfo
                        -- It is knot-tied: don't look at it!
            -> Name             -- New name
            -> (Id, CoreExpr)   -- Binder and RHS before tidying
            -> (Id, CoreExpr)
        -- This function is the heart of Step 2
        -- The rec_tidy_env is the one to use for the IdInfo
        -- It's necessary because when we are dealing with a recursive
        -- group, a variable late in the group might be mentioned
        -- in the IdInfo of one early in the group

tidyTopPair uf_opts show_unfold rhs_tidy_env name' (bndr, rhs)
  = (bndr1, rhs1)
  where
    bndr1    = mkGlobalId details name' ty' idinfo'
    details  = idDetails bndr   -- Preserve the IdDetails
    ty'      = tidyTopType (idType bndr)
    rhs1     = tidyExpr rhs_tidy_env rhs
    idinfo'  = tidyTopIdInfo uf_opts rhs_tidy_env name' ty'
                             rhs rhs1 (idInfo bndr) show_unfold

-- tidyTopIdInfo creates the final IdInfo for top-level
-- binders.  The delicate piece:
--
--  * Arity.  After CoreTidy, this arity must not change any more.
--      Indeed, CorePrep must eta expand where necessary to make
--      the manifest arity equal to the claimed arity.
--
tidyTopIdInfo :: UnfoldingOpts -> TidyEnv -> Name -> Type
              -> CoreExpr -> CoreExpr -> IdInfo -> Bool -> IdInfo
tidyTopIdInfo uf_opts rhs_tidy_env name rhs_ty orig_rhs tidy_rhs idinfo show_unfold
  | not is_external     -- For internal Ids (not externally visible)
  = vanillaIdInfo       -- we only need enough info for code generation
                        -- Arity and strictness info are enough;
                        --      c.f. GHC.Core.Tidy.tidyLetBndr
        `setArityInfo`      arity
        `setDmdSigInfo`     final_sig
        `setCprSigInfo`     final_cpr
        `setUnfoldingInfo`  minimal_unfold_info  -- See note [Preserve evaluatedness]
                                                 -- in GHC.Core.Tidy

  | otherwise           -- Externally-visible Ids get the whole lot
  = vanillaIdInfo
        `setArityInfo`       arity
        `setDmdSigInfo`      final_sig
        `setCprSigInfo`      final_cpr
        `setOccInfo`         robust_occ_info
        `setInlinePragInfo`  inlinePragInfo idinfo
        `setUnfoldingInfo`   unfold_info
                -- NB: we throw away the Rules
                -- They have already been extracted by findExternalRules
  where
    is_external = isExternalName name

    --------- OccInfo ------------
    robust_occ_info = zapFragileOcc (occInfo idinfo)
    -- It's important to keep loop-breaker information
    -- when we are doing -fexpose-all-unfoldings

    --------- Strictness ------------
    mb_bot_str = exprBotStrictness_maybe orig_rhs

    sig = dmdSigInfo idinfo
    final_sig | not $ isTopSig sig
              = warnPprTrace (_bottom_hidden sig) (ppr name) sig
              -- try a cheap-and-cheerful bottom analyser
              | Just (_, nsig) <- mb_bot_str = nsig
              | otherwise                    = sig

    cpr = cprSigInfo idinfo
    final_cpr | Just _ <- mb_bot_str
              = mkCprSig arity botCpr
              | otherwise
              = cpr

    _bottom_hidden id_sig = case mb_bot_str of
                                  Nothing         -> False
                                  Just (arity, _) -> not (appIsDeadEnd id_sig arity)

    --------- Unfolding ------------
    unf_info = realUnfoldingInfo idinfo
    unfold_info
      | isCompulsoryUnfolding unf_info || show_unfold
      = tidyUnfolding rhs_tidy_env unf_info unf_from_rhs
      | otherwise
      = minimal_unfold_info
    minimal_unfold_info = zapUnfolding unf_info
    unf_from_rhs = mkFinalUnfolding uf_opts InlineRhs final_sig tidy_rhs
    -- NB: do *not* expose the worker if show_unfold is off,
    --     because that means this thing is a loop breaker or
    --     marked NOINLINE or something like that
    -- This is important: if you expose the worker for a loop-breaker
    -- then you can make the simplifier go into an infinite loop, because
    -- in effect the unfolding is exposed.  See #1709
    --
    -- You might think that if show_unfold is False, then the thing should
    -- not be w/w'd in the first place.  But a legitimate reason is this:
    --    the function returns bottom
    -- In this case, show_unfold will be false (we don't expose unfoldings
    -- for bottoming functions), but we might still have a worker/wrapper
    -- split (see Note [Worker/wrapper for bottoming functions] in
    -- GHC.Core.Opt.WorkWrap)


    --------- Arity ------------
    -- Usually the Id will have an accurate arity on it, because
    -- the simplifier has just run, but not always.
    -- One case I found was when the last thing the simplifier
    -- did was to let-bind a non-atomic argument and then float
    -- it to the top level. So it seems more robust just to
    -- fix it here.
    arity = exprArity orig_rhs `min` typeArity rhs_ty
            -- orig_rhs: using tidy_rhs would make a black hole, since
            --           exprArity uses the arities of Ids inside the rhs
            -- typeArity: see Note [typeArity invariants]
            --            in GHC.Core.Opt.Arity

{-
************************************************************************
*                                                                      *
                  Old, dead, type-trimming code
*                                                                      *
************************************************************************

We used to try to "trim off" the constructors of data types that are
not exported, to reduce the size of interface files, at least without
-O.  But that is not always possible: see the old Note [When we can't
trim types] below for exceptions.

Then (#7445) I realised that the TH problem arises for any data type
that we have deriving( Data ), because we can invoke
   Language.Haskell.TH.Quote.dataToExpQ
to get a TH Exp representation of a value built from that data type.
You don't even need {-# LANGUAGE TemplateHaskell #-}.

At this point I give up. The pain of trimming constructors just
doesn't seem worth the gain.  So I've dumped all the code, and am just
leaving it here at the end of the module in case something like this
is ever resurrected.


Note [When we can't trim types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of type trimming is to export algebraic data types
abstractly (without their data constructors) when compiling without
-O, unless of course they are explicitly exported by the user.

We always export synonyms, because they can be mentioned in the type
of an exported Id.  We could do a full dependency analysis starting
from the explicit exports, but that's quite painful, and not done for
now.

But there are some times we can't do that, indicated by the 'no_trim_types' flag.

First, Template Haskell.  Consider (#2386) this
        module M(T, makeOne) where
          data T = Yay String
          makeOne = [| Yay "Yep" |]
Notice that T is exported abstractly, but makeOne effectively exports it too!
A module that splices in $(makeOne) will then look for a declaration of Yay,
so it'd better be there.  Hence, brutally but simply, we switch off type
constructor trimming if TH is enabled in this module.

Second, data kinds.  Consider (#5912)
     {-# LANGUAGE DataKinds #-}
     module M() where
     data UnaryTypeC a = UnaryDataC a
     type Bug = 'UnaryDataC
We always export synonyms, so Bug is exposed, and that means that
UnaryTypeC must be too, even though it's not explicitly exported.  In
effect, DataKinds means that we'd need to do a full dependency analysis
to see what data constructors are mentioned.  But we don't do that yet.

In these two cases we just switch off type trimming altogether.

mustExposeTyCon :: Bool         -- Type-trimming flag
                -> NameSet      -- Exports
                -> TyCon        -- The tycon
                -> Bool         -- Can its rep be hidden?
-- We are compiling without -O, and thus trying to write as little as
-- possible into the interface file.  But we must expose the details of
-- any data types whose constructors or fields are exported
mustExposeTyCon no_trim_types exports tc
  | no_trim_types               -- See Note [When we can't trim types]
  = True

  | not (isAlgTyCon tc)         -- Always expose synonyms (otherwise we'd have to
                                -- figure out whether it was mentioned in the type
                                -- of any other exported thing)
  = True

  | isEnumerationTyCon tc       -- For an enumeration, exposing the constructors
  = True                        -- won't lead to the need for further exposure

  | isFamilyTyCon tc            -- Open type family
  = True

  -- Below here we just have data/newtype decls or family instances

  | null data_cons              -- Ditto if there are no data constructors
  = True                        -- (NB: empty data types do not count as enumerations
                                -- see Note [Enumeration types] in GHC.Core.TyCon

  | any exported_con data_cons  -- Expose rep if any datacon or field is exported
  = True

  | isNewTyCon tc && isFFITy (snd (newTyConRhs tc))
  = True   -- Expose the rep for newtypes if the rep is an FFI type.
           -- For a very annoying reason.  'Foreign import' is meant to
           -- be able to look through newtypes transparently, but it
           -- can only do that if it can "see" the newtype representation

  | otherwise
  = False
  where
    data_cons = tyConDataCons tc
    exported_con con = any (`elemNameSet` exports)
                           (dataConName con : dataConFieldLabels con)
-}
>>>>>>> Do arity trimming at bindings, rather than in exprArity
