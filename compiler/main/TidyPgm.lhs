
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Tidying up Core}

\begin{code}
{-# LANGUAGE CPP #-}

module TidyPgm (
       mkBootModDetailsTc, tidyProgram, globaliseAndTidyId
   ) where

#include "HsVersions.h"

import TcRnTypes
import DynFlags
import CoreSyn
import CoreUnfold
import CoreFVs
import CoreTidy
import CoreMonad
import CorePrep
import CoreUtils
import Literal
import Rules
import CoreArity        ( exprArity, exprBotStrictness_maybe )
import VarEnv
import VarSet
import Var
import Id
import MkId             ( mkDictSelRhs )
import IdInfo
import InstEnv
import FamInstEnv
import Type             ( tidyTopType )
import Demand           ( appIsBottom, isNopSig, isBottomingSig )
import BasicTypes
import Name hiding (varName)
import NameSet
import NameEnv
import Avail
import IfaceEnv
import TcEnv
import TcRnMonad
import DataCon
import TyCon
import Class
import Module
import Packages( isDllName )
import HscTypes
import Maybes
import UniqSupply
import ErrUtils (Severity(..))
import Outputable
import FastBool hiding ( fastOr )
import SrcLoc
import Util
import FastString
import qualified ErrUtils as Err

import Control.Monad
import Data.Function
import Data.List        ( sortBy )
import Data.IORef       ( atomicModifyIORef )
\end{code}


Constructing the TypeEnv, Instances, Rules, VectInfo from which the
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

%************************************************************************
%*                                                                      *
                Plan A: simpleTidyPgm
%*                                                                      *
%************************************************************************


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

* Tidy the bindings, to ensure that the Caf and Arity
  information is correct for each top-level binder; the
  code generator needs it. And to ensure that local names have
  distinct OccNames in case of object-file splitting

\begin{code}
-- This is Plan A: make a small type env when typechecking only,
-- or when compiling a hs-boot file, or simply when not using -O
--
-- We don't look at the bindings at all -- there aren't any
-- for hs-boot files

mkBootModDetailsTc :: HscEnv -> TcGblEnv -> IO ModDetails
mkBootModDetailsTc hsc_env
        TcGblEnv{ tcg_exports   = exports,
                  tcg_type_env  = type_env, -- just for the Ids
                  tcg_tcs       = tcs,
                  tcg_insts     = insts,
                  tcg_fam_insts = fam_insts
                }
  = do  { let dflags = hsc_dflags hsc_env
        ; showPass dflags CoreTidy

        ; let { insts'     = map (tidyClsInstDFun globaliseAndTidyId) insts
              ; dfun_ids   = map instanceDFunId insts'
              ; type_env1  = mkBootTypeEnv (availsToNameSet exports)
                                (typeEnvIds type_env) tcs fam_insts
              ; type_env2  = extendTypeEnvWithPatSyns type_env1 (typeEnvPatSyns type_env)
              ; type_env'  = extendTypeEnvWithIds type_env2 dfun_ids
              }
        ; return (ModDetails { md_types     = type_env'
                             , md_insts     = insts'
                             , md_fam_insts = fam_insts
                             , md_rules     = []
                             , md_anns      = []
                             , md_exports   = exports
                             , md_vect_info = noVectInfo
                             })
        }
  where

mkBootTypeEnv :: NameSet -> [Id] -> [TyCon] -> [FamInst] -> TypeEnv
mkBootTypeEnv exports ids tcs fam_insts
  = tidyTypeEnv True $
       typeEnvFromEntities final_ids tcs fam_insts
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
    final_ids = [ if isLocalId id then globaliseAndTidyId id
                                  else id
                | id <- ids
                , keep_it id ]

        -- default methods have their export flag set, but everything
        -- else doesn't (yet), because this is pre-desugaring, so we
        -- must test both.
    keep_it id = isExportedId id || idName id `elemNameSet` exports



globaliseAndTidyId :: Id -> Id
-- Takes an LocalId with an External Name,
-- makes it into a GlobalId
--     * unchanged Name (might be Internal or External)
--     * unchanged details
--     * VanillaIdInfo (makes a conservative assumption about Caf-hood)
globaliseAndTidyId id
  = Id.setIdType (globaliseId id) tidy_type
  where
    tidy_type = tidyTopType (idType id)
\end{code}


%************************************************************************
%*                                                                      *
        Plan B: tidy bindings, make TypeEnv full of IdInfo
%*                                                                      *
%************************************************************************

Plan B: include pragmas, make interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Figure out which Ids are externally visible

* Tidy the bindings, externalising appropriate Ids

* Drop all Ids from the TypeEnv, and add all the External Ids from
  the bindings.  (This adds their IdInfo to the TypeEnv; and adds
  floated-out Ids that weren't even in the TypeEnv before.)

Step 1: Figure out external Ids
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [choosing external names]

See also the section "Interface stability" in the
RecompilationAvoidance commentary:
  http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance

First we figure out which Ids are "external" Ids.  An
"external" Id is one that is visible from outside the compilation
unit.  These are
  a) the user exported ones
  b) ones mentioned in the unfoldings, workers,
     rules of externally-visible ones ,
     or vectorised versions of externally-visible ones

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

Step 2: Tidy the program
~~~~~~~~~~~~~~~~~~~~~~~~
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

        * its CAF info, computed from what is free in its RHS


Finally, substitute these new top-level binders consistently
throughout, including in unfoldings.  We also tidy binders in
RHSs, so that they print nicely in interfaces.

\begin{code}
tidyProgram :: HscEnv -> ModGuts -> IO (CgGuts, ModDetails)
tidyProgram hsc_env  (ModGuts { mg_module    = mod
                              , mg_exports   = exports
                              , mg_tcs       = tcs
                              , mg_insts     = insts
                              , mg_fam_insts = fam_insts
                              , mg_binds     = binds
                              , mg_patsyns   = patsyns
                              , mg_rules     = imp_rules
                              , mg_vect_info = vect_info
                              , mg_anns      = anns
                              , mg_deps      = deps
                              , mg_foreign   = foreign_stubs
                              , mg_hpc_info  = hpc_info
                              , mg_modBreaks = modBreaks
                              })

  = do  { let { dflags     = hsc_dflags hsc_env
              ; omit_prags = gopt Opt_OmitInterfacePragmas dflags
              ; expose_all = gopt Opt_ExposeAllUnfoldings  dflags
              }
        ; showPass dflags CoreTidy

        ; let { type_env = typeEnvFromEntities [] tcs fam_insts

              ; implicit_binds
                  = concatMap getClassImplicitBinds (typeEnvClasses type_env) ++
                    concatMap getTyConImplicitBinds (typeEnvTyCons type_env)
              }

        ; (unfold_env, tidy_occ_env)
              <- chooseExternalIds hsc_env mod omit_prags expose_all
                                   binds implicit_binds imp_rules (vectInfoVar vect_info)
        ; let { ext_rules = findExternalRules omit_prags binds imp_rules unfold_env }
                -- Glom together imp_rules and rules currently attached to binders
                -- Then pick just the ones we need to expose
                -- See Note [Which rules to expose]

        ; (tidy_env, tidy_binds)
                 <- tidyTopBinds hsc_env mod unfold_env tidy_occ_env binds

        ; let { final_ids  = [ id | id <- bindersOfBinds tidy_binds,
                                    isExternalName (idName id)]
              ; final_patsyns = filter (isExternalName . getName) patsyns

              ; type_env' = extendTypeEnvWithIds type_env final_ids
              ; type_env'' = extendTypeEnvWithPatSyns type_env' final_patsyns

              ; tidy_type_env = tidyTypeEnv omit_prags type_env''

              ; tidy_insts    = map (tidyClsInstDFun (lookup_dfun tidy_type_env)) insts
                -- A DFunId will have a binding in tidy_binds, and so
                -- will now be in final_env, replete with IdInfo
                -- Its name will be unchanged since it was born, but
                -- we want Global, IdInfo-rich (or not) DFunId in the
                -- tidy_insts

              ; tidy_rules = tidyRules tidy_env ext_rules
                -- You might worry that the tidy_env contains IdInfo-rich stuff
                -- and indeed it does, but if omit_prags is on, ext_rules is
                -- empty

              ; tidy_vect_info = tidyVectInfo tidy_env vect_info

              -- See Note [Injecting implicit bindings]
              ; all_tidy_binds = implicit_binds ++ tidy_binds

              -- get the TyCons to generate code for.  Careful!  We must use
              -- the untidied TypeEnv here, because we need
              --  (a) implicit TyCons arising from types and classes defined
              --      in this module
              --  (b) wired-in TyCons, which are normally removed from the
              --      TypeEnv we put in the ModDetails
              --  (c) Constructors even if they are not exported (the
              --      tidied TypeEnv has trimmed these away)
              ; alg_tycons = filter isAlgTyCon (typeEnvTyCons type_env)
              }

        ; endPass hsc_env CoreTidy all_tidy_binds tidy_rules

          -- If the endPass didn't print the rules, but ddump-rules is
          -- on, print now
        ; unless (dopt Opt_D_dump_simpl dflags) $
            Err.dumpIfSet_dyn dflags Opt_D_dump_rules
              (showSDoc dflags (ppr CoreTidy <+> ptext (sLit "rules")))
              (pprRulesForUser tidy_rules)

          -- Print one-line size info
        ; let cs = coreBindsStats tidy_binds
        ; when (dopt Opt_D_dump_core_stats dflags)
               (log_action dflags dflags SevDump noSrcSpan defaultDumpStyle
                          (ptext (sLit "Tidy size (terms,types,coercions)")
                           <+> ppr (moduleName mod) <> colon
                           <+> int (cs_tm cs)
                           <+> int (cs_ty cs)
                           <+> int (cs_co cs) ))

        ; return (CgGuts { cg_module   = mod,
                           cg_tycons   = alg_tycons,
                           cg_binds    = all_tidy_binds,
                           cg_foreign  = foreign_stubs,
                           cg_dep_pkgs = map fst $ dep_pkgs deps,
                           cg_hpc_info = hpc_info,
                           cg_modBreaks = modBreaks },

                   ModDetails { md_types     = tidy_type_env,
                                md_rules     = tidy_rules,
                                md_insts     = tidy_insts,
                                md_vect_info = tidy_vect_info,
                                md_fam_insts = fam_insts,
                                md_exports   = exports,
                                md_anns      = anns      -- are already tidy
                              })
        }

lookup_dfun :: TypeEnv -> Var -> Id
lookup_dfun type_env dfun_id
  = case lookupTypeEnv type_env (idName dfun_id) of
        Just (AnId dfun_id') -> dfun_id'
        _other -> pprPanic "lookup_dfun" (ppr dfun_id)

--------------------------
tidyTypeEnv :: Bool       -- Compiling without -O, so omit prags
            -> TypeEnv -> TypeEnv

-- The competed type environment is gotten from
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

tidyTypeEnv omit_prags type_env
 = let
        type_env1 = filterNameEnv (not . isWiredInName . getName) type_env
          -- (1) remove wired-in things
        type_env2 | omit_prags = mapNameEnv trimThing type_env1
                  | otherwise  = type_env1
          -- (2) trimmed if necessary
    in
    type_env2

--------------------------
trimThing :: TyThing -> TyThing
-- Trim off inessentials, for boot files and no -O
trimThing (AnId id)
   | not (isImplicitId id)
   = AnId (id `setIdInfo` vanillaIdInfo)

trimThing other_thing
  = other_thing
\end{code}

\begin{code}
tidyVectInfo :: TidyEnv -> VectInfo -> VectInfo
tidyVectInfo (_, var_env) info@(VectInfo { vectInfoVar          = vars
                                         , vectInfoParallelVars = parallelVars
                                         })
  = info { vectInfoVar          = tidy_vars
         , vectInfoParallelVars = tidy_parallelVars
         }
  where
      -- we only export mappings whose domain and co-domain is exported (otherwise, the iface is
      -- inconsistent)
    tidy_vars = mkVarEnv [ (tidy_var, (tidy_var, tidy_var_v))
                         | (var, var_v) <- varEnvElts vars
                         , let tidy_var   = lookup_var var
                               tidy_var_v = lookup_var var_v
                         , isExternalId tidy_var   && isExportedId tidy_var
                         , isExternalId tidy_var_v && isExportedId tidy_var_v
                         , isDataConWorkId var || not (isImplicitId var)
                         ]

    tidy_parallelVars = mkVarSet [ tidy_var
                                 | var <- varSetElems parallelVars
                                 , let tidy_var = lookup_var var
                                 , isExternalId tidy_var && isExportedId tidy_var
                                 ]

    lookup_var var = lookupWithDefaultVarEnv var_env var var
    
    -- We need to make sure that all names getting into the iface version of 'VectInfo' are
    -- external; otherwise, 'MkIface' will bomb out.
    isExternalId = isExternalName . idName
\end{code}

Note [Don't attempt to trim data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For some time GHC tried to avoid exporting the data constructors
of a data type if it wasn't strictly necessary to do so; see Trac #835.
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
    
    However Trac #7445 shows that even without TemplateHaskell, using
    the Data class and invoking Language.Haskell.TH.Quote.dataToExpQ
    is enough to require us to expose the data constructors.
    
    So I've given up on this "optimisation" -- it's probably not
    important anyway.  Now I'm simply not attempting to trim off
    the data constructors.  The gain in simplicity is worth the
    modest cost in interface file growth, which is limited to the
    bits reqd to describe those data constructors.

%************************************************************************
%*                                                                      *
        Implicit bindings
%*                                                                      *
%************************************************************************

Note [Injecting implicit bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We inject the implict bindings right at the end, in CoreTidy.
Some of these bindings, notably record selectors, are not
constructed in an optimised form.  E.g. record selector for
        data T = MkT { x :: {-# UNPACK #-} !Int }
Then the unfolding looks like
        x = \t. case t of MkT x1 -> let x = I# x1 in x
This generates bad code unless it's first simplified a bit.  That is
why CoreUnfold.mkImplicitUnfolding uses simleExprOpt to do a bit of
optimisation first.  (Only matters when the selector is used curried;
eg map x ys.)  See Trac #2070.

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
importing modules were expecting it to have arity 1 (Trac #2844).
It's much safer just to inject them right at the end, after tidying.

Oh: two other reasons for injecting them late:

  - If implicit Ids are already in the bindings when we start TidyPgm,
    we'd have to be careful not to treat them as external Ids (in
    the sense of findExternalIds); else the Ids mentioned in *their*
    RHSs will be treated as external and you get an interface file
    saying      a18 = <blah>
    but nothing refererring to a18 (because the implicit Id is the
    one that does, and implicit Ids don't appear in interface files).

  - More seriously, the tidied type-envt will include the implicit
    Id replete with a18 in its unfolding; but we won't take account
    of a18 when computing a fingerprint for the class; result chaos.

There is one sort of implicit binding that is injected still later,
namely those for data constructor workers. Reason (I think): it's
really just a code generation trick.... binding itself makes no sense.
See Note [Data constructor workers] in CorePrep.

\begin{code}
getTyConImplicitBinds :: TyCon -> [CoreBind]
getTyConImplicitBinds tc = map get_defn (mapMaybe dataConWrapId_maybe (tyConDataCons tc))

getClassImplicitBinds :: Class -> [CoreBind]
getClassImplicitBinds cls
  = [ NonRec op (mkDictSelRhs cls val_index)
    | (op, val_index) <- classAllSelIds cls `zip` [0..] ]

get_defn :: Id -> CoreBind
get_defn id = NonRec id (unfoldingTemplate (realIdUnfolding id))
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Step 1: finding externals}
%*                                                                      *
%************************************************************************

See Note [Choosing external names].

\begin{code}
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
                  -> VarEnv (Var, Var)
                  -> IO (UnfoldEnv, TidyOccEnv)
                  -- Step 1 from the notes above

chooseExternalIds hsc_env mod omit_prags expose_all binds implicit_binds imp_id_rules vect_vars
  = do { (unfold_env1,occ_env1) <- search init_work_list emptyVarEnv init_occ_env
       ; let internal_ids = filter (not . (`elemVarEnv` unfold_env1)) binders
       ; tidy_internal internal_ids unfold_env1 occ_env1 }
 where
  nc_var = hsc_NC hsc_env

  -- init_ext_ids is the intial list of Ids that should be
  -- externalised.  It serves as the starting point for finding a
  -- deterministic, tidy, renaming for all external Ids in this
  -- module.
  --
  -- It is sorted, so that it has adeterministic order (i.e. it's the
  -- same list every time this module is compiled), in contrast to the
  -- bindings, which are ordered non-deterministically.
  init_work_list = zip init_ext_ids init_ext_ids
  init_ext_ids   = sortBy (compare `on` getOccName) $
                   filter is_external binders

  -- An Id should be external if either (a) it is exported,
  -- (b) it appears in the RHS of a local rule for an imported Id, or
  -- (c) it is the vectorised version of an imported Id
  -- See Note [Which rules to expose]
  is_external id = isExportedId id || id `elemVarSet` rule_rhs_vars || id `elemVarSet` vect_var_vs
  rule_rhs_vars  = foldr (unionVarSet . ruleRhsFreeVars) emptyVarSet imp_id_rules
  vect_var_vs    = mkVarSet [var_v | (var, var_v) <- nameEnvElts vect_vars, isGlobalId var]

  binders          = bindersOfBinds binds
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


  search :: [(Id,Id)]    -- The work-list: (external id, referrring id)
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
      (occ_env', name') <- tidyTopName mod nc_var (Just referrer) occ_env idocc
      let
          (new_ids, show_unfold)
                | omit_prags = ([], False)
                | otherwise  = addExternal expose_all refined_id

                -- add vectorised version if any exists
          new_ids' = new_ids ++ maybeToList (fmap snd $ lookupVarEnv vect_vars idocc)
          
                -- 'idocc' is an *occurrence*, but we need to see the
                -- unfolding in the *definition*; so look up in binder_set
          refined_id = case lookupVarSet binder_set idocc of
                         Just id -> id
                         Nothing -> WARN( True, ppr idocc ) idocc

          unfold_env' = extendVarEnv unfold_env idocc (name',show_unfold)
          referrer' | isExportedId refined_id = refined_id
                    | otherwise               = referrer
      --
      search (zip new_ids' (repeat referrer') ++ rest) unfold_env' occ_env'

  tidy_internal :: [Id] -> UnfoldEnv -> TidyOccEnv
                -> IO (UnfoldEnv, TidyOccEnv)
  tidy_internal []       unfold_env occ_env = return (unfold_env,occ_env)
  tidy_internal (id:ids) unfold_env occ_env = do
      (occ_env', name') <- tidyTopName mod nc_var Nothing occ_env id
      let unfold_env' = extendVarEnv unfold_env id (name',False)
      tidy_internal ids unfold_env' occ_env'

addExternal :: Bool -> Id -> ([Id], Bool)
addExternal expose_all id = (new_needed_ids, show_unfold)
  where
    new_needed_ids = bndrFvsInOrder show_unfold id
    idinfo         = idInfo id
    show_unfold    = show_unfolding (unfoldingInfo idinfo)
    never_active   = isNeverActive (inlinePragmaActivation (inlinePragInfo idinfo))
    loop_breaker   = isStrongLoopBreaker (occInfo idinfo)
    bottoming_fn   = isBottomingSig (strictnessInfo idinfo)

        -- Stuff to do with the Id's unfolding
        -- We leave the unfolding there even if there is a worker
        -- In GHCi the unfolding is used by importers

    show_unfolding (CoreUnfolding { uf_src = src, uf_guidance = guidance })
       =  expose_all         -- 'expose_all' says to expose all
                             -- unfoldings willy-nilly

       || isStableSource src     -- Always expose things whose
                                 -- source is an inline rule

       || not (bottoming_fn      -- No need to inline bottom functions
           || never_active       -- Or ones that say not to
           || loop_breaker       -- Or that are loop breakers
           || neverUnfoldGuidance guidance)
    show_unfolding (DFunUnfolding {}) = True
    show_unfolding _                  = False
\end{code}

%************************************************************************
%*                                                                      *
               Deterministic free variables
%*                                                                      *
%************************************************************************

We want a deterministic free-variable list.  exprFreeVars gives us
a VarSet, which is in a non-deterministic order when converted to a
list.  Hence, here we define a free-variable finder that returns
the free variables in the order that they are encountered.

See Note [Choosing external names]

\begin{code}
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

instance Functor DFFV where
    fmap = liftM

instance Applicative DFFV where
    pure = return
    (<*>) = ap

instance Monad DFFV where
  return a = DFFV $ \_ st -> (st, a)
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
dffvExpr (Tick (Breakpoint _ ids) e) = mapM_ insert ids >> dffvExpr e
dffvExpr (Tick _other e)    = dffvExpr e
dffvExpr (Cast e _)           = dffvExpr e
dffvExpr (Let (NonRec x r) e) = dffvBind (x,r) >> extendScope x (dffvExpr e)
dffvExpr (Let (Rec prs) e)    = extendScopeList (map fst prs) $
                                (mapM_ dffvBind prs >> dffvExpr e)
dffvExpr (Case e b _ as)      = dffvExpr e >> extendScope b (mapM_ dffvAlt as)
dffvExpr _other               = return ()

dffvAlt :: (t, [Var], CoreExpr) -> DFFV ()
dffvAlt (_,xs,r) = extendScopeList xs (dffvExpr r)

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
  = do { go_unf (unfoldingInfo idinfo)
       ; mapM_ go_rule (specInfoRules (specInfo idinfo)) }
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
\end{code}


%************************************************************************
%*                                                                      *
               tidyTopName
%*                                                                      *
%************************************************************************

This is where we set names to local/global based on whether they really are
externally visible (see comment at the top of this module).  If the name
was previously local, we have to give it a unique occurrence name if
we intend to externalise it.

\begin{code}
tidyTopName :: Module -> IORef NameCache -> Maybe Id -> TidyOccEnv
            -> Id -> IO (TidyOccEnv, Name)
tidyTopName mod nc_var maybe_ref occ_env id
  | global && internal = return (occ_env, localiseName name)

  | global && external = return (occ_env, name)
        -- Global names are assumed to have been allocated by the renamer,
        -- so they already have the "right" unique
        -- And it's a system-wide unique too

  -- Now we get to the real reason that all this is in the IO Monad:
  -- we have to update the name cache in a nice atomic fashion

  | local  && internal = do { new_local_name <- atomicModifyIORef nc_var mk_new_local
                            ; return (occ_env', new_local_name) }
        -- Even local, internal names must get a unique occurrence, because
        -- if we do -split-objs we externalise the name later, in the code generator
        --
        -- Similarly, we must make sure it has a system-wide Unique, because
        -- the byte-code generator builds a system-wide Name->BCO symbol table

  | local  && external = do { new_external_name <- atomicModifyIORef nc_var mk_new_external
                            ; return (occ_env', new_external_name) }

  | otherwise = panic "tidyTopName"
  where
    name        = idName id
    external    = isJust maybe_ref
    global      = isExternalName name
    local       = not global
    internal    = not external
    loc         = nameSrcSpan name

    old_occ     = nameOccName name
    new_occ
      | Just ref <- maybe_ref, ref /= id =
          mkOccName (occNameSpace old_occ) $
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

    mk_new_local nc = (nc { nsUniqs = us }, mkInternalName uniq occ' loc)
                    where
                      (uniq, us) = takeUniqFromSupply (nsUniqs nc)

    mk_new_external nc = allocateGlobalBinder nc mod occ' loc
        -- If we want to externalise a currently-local name, check
        -- whether we have already assigned a unique for it.
        -- If so, use it; if not, extend the table.
        -- All this is done by allcoateGlobalBinder.
        -- This is needed when *re*-compiling a module in GHCi; we must
        -- use the same name for externally-visible things as we did before.
\end{code}

\begin{code}
findExternalRules :: Bool       -- Omit pragmas
                  -> [CoreBind]
                  -> [CoreRule] -- Local rules for imported fns
                  -> UnfoldEnv  -- Ids that are exported, so we need their rules
                  -> [CoreRule]
  -- The complete rules are gotten by combining
  --    a) local rules for imported Ids
  --    b) rules embedded in the top-level Ids
findExternalRules omit_prags binds imp_id_rules unfold_env
  | omit_prags = []
  | otherwise  = filterOut internal_rule (imp_id_rules ++ local_rules)
  where
    local_rules  = [ rule
                   | id <- bindersOfBinds binds,
                     external_id id,
                     rule <- idCoreRules id
                   ]

    internal_rule rule
        =  any (not . external_id) (varSetElems (ruleLhsFreeIds rule))
                -- Don't export a rule whose LHS mentions a locally-defined
                --  Id that is completely internal (i.e. not visible to an
                -- importing module)

    external_id id
      | Just (name,_) <- lookupVarEnv unfold_env id = isExternalName name
      | otherwise = False
\end{code}

Note [Which rules to expose]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
findExternalRules filters imp_rules to avoid binders that
aren't externally visible; but the externally-visible binders
are computed (by findExternalIds) assuming that all orphan
rules are externalised (see init_ext_ids in function
'search'). So in fact we may export more than we need.
(It's a sort of mutual recursion.)

%************************************************************************
%*                                                                      *
\subsection{Step 2: top-level tidying}
%*                                                                      *
%************************************************************************


\begin{code}
-- TopTidyEnv: when tidying we need to know
--   * nc_var: The NameCache, containing a unique supply and any pre-ordained Names.
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

tidyTopBinds :: HscEnv
             -> Module
             -> UnfoldEnv
             -> TidyOccEnv
             -> CoreProgram
             -> IO (TidyEnv, CoreProgram)

tidyTopBinds hsc_env this_mod unfold_env init_occ_env binds
  = do mkIntegerId <- lookupMkIntegerName dflags hsc_env
       return $ tidy mkIntegerId init_env binds
  where
    dflags = hsc_dflags hsc_env

    init_env = (init_occ_env, emptyVarEnv)

    this_pkg = thisPackage dflags

    tidy _           env []     = (env, [])
    tidy mkIntegerId env (b:bs) = let (env1, b')  = tidyTopBind dflags this_pkg this_mod mkIntegerId unfold_env env b
                                      (env2, bs') = tidy mkIntegerId env1 bs
                                  in
                                      (env2, b':bs')

------------------------
tidyTopBind  :: DynFlags
             -> PackageId
             -> Module
             -> Id
             -> UnfoldEnv
             -> TidyEnv
             -> CoreBind
             -> (TidyEnv, CoreBind)

tidyTopBind dflags this_pkg this_mod mkIntegerId unfold_env (occ_env,subst1) (NonRec bndr rhs)
  = (tidy_env2,  NonRec bndr' rhs')
  where
    Just (name',show_unfold) = lookupVarEnv unfold_env bndr
    caf_info      = hasCafRefs dflags this_pkg this_mod (mkIntegerId, subst1) (idArity bndr) rhs
    (bndr', rhs') = tidyTopPair dflags show_unfold tidy_env2 caf_info name' (bndr, rhs)
    subst2        = extendVarEnv subst1 bndr bndr'
    tidy_env2     = (occ_env, subst2)

tidyTopBind dflags this_pkg this_mod mkIntegerId unfold_env (occ_env,subst1) (Rec prs)
  = (tidy_env2, Rec prs')
  where
    prs' = [ tidyTopPair dflags show_unfold tidy_env2 caf_info name' (id,rhs)
           | (id,rhs) <- prs,
             let (name',show_unfold) =
                    expectJust "tidyTopBind" $ lookupVarEnv unfold_env id
           ]

    subst2    = extendVarEnvList subst1 (bndrs `zip` map fst prs')
    tidy_env2 = (occ_env, subst2)

    bndrs = map fst prs

        -- the CafInfo for a recursive group says whether *any* rhs in
        -- the group may refer indirectly to a CAF (because then, they all do).
    caf_info
        | or [ mayHaveCafRefs (hasCafRefs dflags this_pkg this_mod (mkIntegerId, subst1) (idArity bndr) rhs)
             | (bndr,rhs) <- prs ] = MayHaveCafRefs
        | otherwise                = NoCafRefs

-----------------------------------------------------------
tidyTopPair :: DynFlags
            -> Bool  -- show unfolding
            -> TidyEnv  -- The TidyEnv is used to tidy the IdInfo
                        -- It is knot-tied: don't look at it!
            -> CafInfo
            -> Name             -- New name
            -> (Id, CoreExpr)   -- Binder and RHS before tidying
            -> (Id, CoreExpr)
        -- This function is the heart of Step 2
        -- The rec_tidy_env is the one to use for the IdInfo
        -- It's necessary because when we are dealing with a recursive
        -- group, a variable late in the group might be mentioned
        -- in the IdInfo of one early in the group

tidyTopPair dflags show_unfold rhs_tidy_env caf_info name' (bndr, rhs)
  = (bndr1, rhs1)
  where
    bndr1    = mkGlobalId details name' ty' idinfo'
    details  = idDetails bndr   -- Preserve the IdDetails
    ty'      = tidyTopType (idType bndr)
    rhs1     = tidyExpr rhs_tidy_env rhs
    idinfo'  = tidyTopIdInfo dflags rhs_tidy_env name' rhs rhs1 (idInfo bndr)
                             show_unfold caf_info

-- tidyTopIdInfo creates the final IdInfo for top-level
-- binders.  There are two delicate pieces:
--
--  * Arity.  After CoreTidy, this arity must not change any more.
--      Indeed, CorePrep must eta expand where necessary to make
--      the manifest arity equal to the claimed arity.
--
--  * CAF info.  This must also remain valid through to code generation.
--      We add the info here so that it propagates to all
--      occurrences of the binders in RHSs, and hence to occurrences in
--      unfoldings, which are inside Ids imported by GHCi. Ditto RULES.
--      CoreToStg makes use of this when constructing SRTs.
tidyTopIdInfo :: DynFlags -> TidyEnv -> Name -> CoreExpr -> CoreExpr
              -> IdInfo -> Bool -> CafInfo -> IdInfo
tidyTopIdInfo dflags rhs_tidy_env name orig_rhs tidy_rhs idinfo show_unfold caf_info
  | not is_external     -- For internal Ids (not externally visible)
  = vanillaIdInfo       -- we only need enough info for code generation
                        -- Arity and strictness info are enough;
                        --      c.f. CoreTidy.tidyLetBndr
        `setCafInfo`        caf_info
        `setArityInfo`      arity
        `setStrictnessInfo` final_sig

  | otherwise           -- Externally-visible Ids get the whole lot
  = vanillaIdInfo
        `setCafInfo`           caf_info
        `setArityInfo`         arity
        `setStrictnessInfo`    final_sig
        `setOccInfo`           robust_occ_info
        `setInlinePragInfo`    (inlinePragInfo idinfo)
        `setUnfoldingInfo`     unfold_info
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

    sig = strictnessInfo idinfo
    final_sig | not $ isNopSig sig
                 = WARN( _bottom_hidden sig , ppr name ) sig 
                 -- try a cheap-and-cheerful bottom analyser
                 | Just (_, nsig) <- mb_bot_str = nsig
                 | otherwise                    = sig

    _bottom_hidden id_sig = case mb_bot_str of
                                  Nothing         -> False
                                  Just (arity, _) -> not (appIsBottom id_sig arity)

    --------- Unfolding ------------
    unf_info = unfoldingInfo idinfo
    unfold_info | show_unfold = tidyUnfolding rhs_tidy_env unf_info unf_from_rhs
                | otherwise   = noUnfolding
    unf_from_rhs = mkTopUnfolding dflags is_bot tidy_rhs
    is_bot = isBottomingSig final_sig
    -- NB: do *not* expose the worker if show_unfold is off,
    --     because that means this thing is a loop breaker or
    --     marked NOINLINE or something like that
    -- This is important: if you expose the worker for a loop-breaker
    -- then you can make the simplifier go into an infinite loop, because
    -- in effect the unfolding is exposed.  See Trac #1709
    --
    -- You might think that if show_unfold is False, then the thing should
    -- not be w/w'd in the first place.  But a legitimate reason is this:
    --    the function returns bottom
    -- In this case, show_unfold will be false (we don't expose unfoldings
    -- for bottoming functions), but we might still have a worker/wrapper
    -- split (see Note [Worker-wrapper for bottoming functions] in WorkWrap.lhs

    --------- Arity ------------
    -- Usually the Id will have an accurate arity on it, because
    -- the simplifier has just run, but not always.
    -- One case I found was when the last thing the simplifier
    -- did was to let-bind a non-atomic argument and then float
    -- it to the top level. So it seems more robust just to
    -- fix it here.
    arity = exprArity orig_rhs
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Figuring out CafInfo for an expression}
%*                                                                      *
%************************************************************************

hasCafRefs decides whether a top-level closure can point into the dynamic heap.
We mark such things as `MayHaveCafRefs' because this information is
used to decide whether a particular closure needs to be referenced
in an SRT or not.

There are two reasons for setting MayHaveCafRefs:
        a) The RHS is a CAF: a top-level updatable thunk.
        b) The RHS refers to something that MayHaveCafRefs

Possible improvement: In an effort to keep the number of CAFs (and
hence the size of the SRTs) down, we could also look at the expression and
decide whether it requires a small bounded amount of heap, so we can ignore
it as a CAF.  In these cases however, we would need to use an additional
CAF list to keep track of non-collectable CAFs.

\begin{code}
hasCafRefs :: DynFlags -> PackageId -> Module
           -> (Id, VarEnv Var) -> Arity -> CoreExpr
           -> CafInfo
hasCafRefs dflags this_pkg this_mod p arity expr
  | is_caf || mentions_cafs = MayHaveCafRefs
  | otherwise               = NoCafRefs
 where
  mentions_cafs = isFastTrue (cafRefsE dflags p expr)
  is_dynamic_name = isDllName dflags this_pkg this_mod
  is_caf = not (arity > 0 || rhsIsStatic (targetPlatform dflags) is_dynamic_name expr)

  -- NB. we pass in the arity of the expression, which is expected
  -- to be calculated by exprArity.  This is because exprArity
  -- knows how much eta expansion is going to be done by
  -- CorePrep later on, and we don't want to duplicate that
  -- knowledge in rhsIsStatic below.

cafRefsE :: DynFlags -> (Id, VarEnv Id) -> Expr a -> FastBool
cafRefsE _      p (Var id)            = cafRefsV p id
cafRefsE dflags p (Lit lit)           = cafRefsL dflags p lit
cafRefsE dflags p (App f a)           = fastOr (cafRefsE dflags p f) (cafRefsE dflags p) a
cafRefsE dflags p (Lam _ e)           = cafRefsE dflags p e
cafRefsE dflags p (Let b e)           = fastOr (cafRefsEs dflags p (rhssOfBind b)) (cafRefsE dflags p) e
cafRefsE dflags p (Case e _bndr _ alts) = fastOr (cafRefsE dflags p e) (cafRefsEs dflags p) (rhssOfAlts alts)
cafRefsE dflags p (Tick _n e)         = cafRefsE dflags p e
cafRefsE dflags p (Cast e _co)        = cafRefsE dflags p e
cafRefsE _      _ (Type _)            = fastBool False
cafRefsE _      _ (Coercion _)        = fastBool False

cafRefsEs :: DynFlags -> (Id, VarEnv Id) -> [Expr a] -> FastBool
cafRefsEs _      _ []     = fastBool False
cafRefsEs dflags p (e:es) = fastOr (cafRefsE dflags p e) (cafRefsEs dflags p) es

cafRefsL :: DynFlags -> (Id, VarEnv Id) -> Literal -> FastBool
-- Don't forget that mk_integer id might have Caf refs!
-- We first need to convert the Integer into its final form, to
-- see whether mkInteger is used.
cafRefsL dflags p@(mk_integer, _) (LitInteger i _) = cafRefsE dflags p (cvtLitInteger dflags mk_integer i)
cafRefsL _      _ _                         = fastBool False

cafRefsV :: (Id, VarEnv Id) -> Id -> FastBool
cafRefsV (_, p) id
  | not (isLocalId id)            = fastBool (mayHaveCafRefs (idCafInfo id))
  | Just id' <- lookupVarEnv p id = fastBool (mayHaveCafRefs (idCafInfo id'))
  | otherwise                     = fastBool False

fastOr :: FastBool -> (a -> FastBool) -> a -> FastBool
-- hack for lazy-or over FastBool.
fastOr a f x = fastBool (isFastTrue a || isFastTrue (f x))
\end{code}


------------------------------------------------------------------------------
--               Old, dead, type-trimming code
-------------------------------------------------------------------------------

We used to try to "trim off" the constructors of data types that are
not exported, to reduce the size of interface files, at least without
-O.  But that is not always possible: see the old Note [When we can't
trim types] below for exceptions.

Then (Trac #7445) I realised that the TH problem arises for any data type
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

First, Template Haskell.  Consider (Trac #2386) this
        module M(T, makeOne) where
          data T = Yay String
          makeOne = [| Yay "Yep" |]
Notice that T is exported abstractly, but makeOne effectively exports it too!
A module that splices in $(makeOne) will then look for a declartion of Yay,
so it'd better be there.  Hence, brutally but simply, we switch off type
constructor trimming if TH is enabled in this module.

Second, data kinds.  Consider (Trac #5912)
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
                                -- see Note [Enumeration types] in TyCon

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
