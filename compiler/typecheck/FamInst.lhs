The @FamInst@ type: family instance heads

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module FamInst ( 
        checkFamInstConsistency, tcExtendLocalFamInstEnv,
	tcLookupFamInst, tcLookupDataFamInst,
        tcGetFamInstEnvs
    ) where

import HscTypes
import FamInstEnv
import LoadIface
import TypeRep
import TcRnMonad
import TyCon
import CoAxiom
import DynFlags
import Module
import Outputable
import UniqFM
import FastString
import Util
import Maybes
import TcMType
import Type
import VarSet (mkVarSet)
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

#include "HsVersions.h"
\end{code}


%************************************************************************
%*									*
	Optimised overlap checking for family instances
%*									*
%************************************************************************

For any two family instance modules that we import directly or indirectly, we
check whether the instances in the two modules are consistent, *unless* we can
be certain that the instances of the two modules have already been checked for
consistency during the compilation of modules that we import.

Why do we need to check?  Consider 
   module X1 where	  	  module X2 where
    data T1			    data T2
    type instance F T1 b = Int	    type instance F a T2 = Char
    f1 :: F T1 a -> Int		    f2 :: Char -> F a T2
    f1 x = x			    f2 x = x

Now if we import both X1 and X2 we could make (f2 . f1) :: Int -> Char.
Notice that neither instance is an orphan.

How do we know which pairs of modules have already been checked?  Any pair of
modules where both modules occur in the `HscTypes.dep_finsts' set (of the
`HscTypes.Dependencies') of one of our directly imported modules must have
already been checked.  Everything else, we check now.  (So that we can be
certain that the modules in our `HscTypes.dep_finsts' are consistent.)

\begin{code}
-- The optimisation of overlap tests is based on determining pairs of modules
-- whose family instances need to be checked for consistency.
--
data ModulePair = ModulePair Module Module

-- canonical order of the components of a module pair
--
canon :: ModulePair -> (Module, Module)
canon (ModulePair m1 m2) | m1 < m2   = (m1, m2)
			 | otherwise = (m2, m1)

instance Eq ModulePair where
  mp1 == mp2 = canon mp1 == canon mp2

instance Ord ModulePair where
  mp1 `compare` mp2 = canon mp1 `compare` canon mp2

instance Outputable ModulePair where
  ppr (ModulePair m1 m2) = angleBrackets (ppr m1 <> comma <+> ppr m2)

-- Sets of module pairs
--
type ModulePairSet = Map ModulePair ()

listToSet :: [ModulePair] -> ModulePairSet
listToSet l = Map.fromList (zip l (repeat ()))

checkFamInstConsistency :: [Module] -> [Module] -> TcM ()
checkFamInstConsistency famInstMods directlyImpMods
  = do { dflags     <- getDynFlags
       ; (eps, hpt) <- getEpsAndHpt

       ; let { -- Fetch the iface of a given module.  Must succeed as
 	       -- all directly imported modules must already have been loaded.
	       modIface mod = 
	         case lookupIfaceByModule dflags hpt (eps_PIT eps) mod of
                   Nothing    -> panic "FamInst.checkFamInstConsistency"
                   Just iface -> iface

             ; hmiModule     = mi_module . hm_iface
	     ; hmiFamInstEnv = extendFamInstEnvList emptyFamInstEnv 
                               . md_fam_insts . hm_details
             ; hpt_fam_insts = mkModuleEnv [ (hmiModule hmi, hmiFamInstEnv hmi) 
			                   | hmi <- eltsUFM hpt]
	     ; groups        = map (dep_finsts . mi_deps . modIface) 
				   directlyImpMods
	     ; okPairs       = listToSet $ concatMap allPairs groups
	         -- instances of okPairs are consistent
	     ; criticalPairs = listToSet $ allPairs famInstMods
	         -- all pairs that we need to consider
             ; toCheckPairs  = Map.keys $ criticalPairs `Map.difference` okPairs
	         -- the difference gives us the pairs we need to check now
	     }

       ; mapM_ (check hpt_fam_insts) toCheckPairs
       }
  where
    allPairs []     = []
    allPairs (m:ms) = map (ModulePair m) ms ++ allPairs ms

    check hpt_fam_insts (ModulePair m1 m2)
      = do { env1 <- getFamInsts hpt_fam_insts m1
           ; env2 <- getFamInsts hpt_fam_insts m2
           ; mapM_ (checkForConflicts (emptyFamInstEnv, env2))   
                   (famInstEnvElts env1) }

getFamInsts :: ModuleEnv FamInstEnv -> Module -> TcM FamInstEnv
getFamInsts hpt_fam_insts mod
  | Just env <- lookupModuleEnv hpt_fam_insts mod = return env
  | otherwise = do { _ <- initIfaceTcRn (loadSysInterface doc mod)
                   ; eps <- getEps
                   ; return (expectJust "checkFamInstConsistency" $
                             lookupModuleEnv (eps_mod_fam_inst_env eps) mod) }
  where
    doc = ppr mod <+> ptext (sLit "is a family-instance module")
\end{code}

%************************************************************************
%*									*
	Lookup
%*									*
%************************************************************************

Look up the instance tycon of a family instance.

The match may be ambiguous (as we know that overlapping instances have
identical right-hand sides under overlapping substitutions - see
'FamInstEnv.lookupFamInstEnvConflicts').  However, the type arguments used
for matching must be equal to or be more specific than those of the family
instance declaration.  We pick one of the matches in case of ambiguity; as
the right-hand sides are identical under the match substitution, the choice
does not matter.

Return the instance tycon and its type instance.  For example, if we have

 tcLookupFamInst 'T' '[Int]' yields (':R42T', 'Int')

then we have a coercion (ie, type instance of family instance coercion)

 :Co:R42T Int :: T [Int] ~ :R42T Int

which implies that :R42T was declared as 'data instance T [a]'.

\begin{code}
tcLookupFamInst :: TyCon -> [Type] -> TcM (Maybe FamInstMatch)
tcLookupFamInst tycon tys
  | not (isFamilyTyCon tycon)
  = return Nothing
  | otherwise
  = do { instEnv <- tcGetFamInstEnvs
       ; let mb_match = lookupFamInstEnv instEnv tycon tys 
--       ; traceTc "lookupFamInst" ((ppr tycon <+> ppr tys) $$ 
--                                  pprTvBndrs (varSetElems (tyVarsOfTypes tys)) $$ 
--                                  ppr mb_match $$ ppr instEnv)
       ; case mb_match of
	   [] -> return Nothing
	   (match:_) 
              -> return $ Just match
       }

tcLookupDataFamInst :: TyCon -> [Type] -> TcM (TyCon, [Type])
-- Find the instance of a data family
-- Note [Looking up family instances for deriving]
tcLookupDataFamInst tycon tys
  | not (isFamilyTyCon tycon)
  = return (tycon, tys)
  | otherwise
  = ASSERT( isAlgTyCon tycon )
    do { maybeFamInst <- tcLookupFamInst tycon tys
       ; case maybeFamInst of
           Nothing             -> famInstNotFound tycon tys
           Just (FamInstMatch { fim_instance = famInst
                              , fim_index    = index
                              , fim_tys      = tys })
             -> ASSERT( index == 0 )
                let tycon' = dataFamInstRepTyCon famInst
                in return (tycon', tys) }

famInstNotFound :: TyCon -> [Type] -> TcM a
famInstNotFound tycon tys 
  = failWithTc (ptext (sLit "No family instance for")
			<+> quotes (pprTypeApp tycon tys))
\end{code}

Note [Looking up family instances for deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcLookupFamInstExact is an auxiliary lookup wrapper which requires
that looked-up family instances exist.  If called with a vanilla
tycon, the old type application is simply returned.

If we have
  data instance F () = ... deriving Eq
  data instance F () = ... deriving Eq
then tcLookupFamInstExact will be confused by the two matches;
but that can't happen because tcInstDecls1 doesn't call tcDeriving
if there are any overlaps.

There are two other things that might go wrong with the lookup.
First, we might see a standalone deriving clause
	deriving Eq (F ())
when there is no data instance F () in scope. 

Note that it's OK to have
  data instance F [a] = ...
  deriving Eq (F [(a,b)])
where the match is not exact; the same holds for ordinary data types
with standalone deriving declrations.


%************************************************************************
%*									*
	Extending the family instance environment
%*									*
%************************************************************************

\begin{code}
-- Add new locally-defined family instances
tcExtendLocalFamInstEnv :: [FamInst br] -> TcM a -> TcM a
tcExtendLocalFamInstEnv fam_insts thing_inside
 = do { env <- getGblEnv
      ; (inst_env', fam_insts') <- foldlM addLocalFamInst  
                                          (tcg_fam_inst_env env, tcg_fam_insts env)
                                          fam_insts
      ; let env' = env { tcg_fam_insts    = fam_insts'
		       , tcg_fam_inst_env = inst_env' }
      ; setGblEnv env' thing_inside 
      }

-- Check that the proposed new instance is OK, 
-- and then add it to the home inst env
-- This must be lazy in the fam_inst arguments, see Note [Lazy axiom match]
-- in FamInstEnv.lhs
addLocalFamInst :: (FamInstEnv,[FamInst Branched]) -> FamInst br -> TcM (FamInstEnv, [FamInst Branched])
addLocalFamInst (home_fie, my_fis) fam_inst
        -- home_fie includes home package and this module
        -- my_fies is just the ones from this module
  = do { traceTc "addLocalFamInst" (ppr fam_inst)

           -- We wish to extend the instance envt with completely
           -- fresh template variables. Otherwise, there may be
           -- problems when we try to unify the template variables
           -- with type family applications.

           -- See also addLocalInst in Inst.lhs
       ; (axBranches', fiBranches')
           <- zipWithAndUnzipM mk_skolem_tyvars (fromBranchList $ coAxiomBranches axiom)
                                                (fromBranchList fiBranches)
       ; let axiom' = axiom { co_ax_branches = toBranchList axBranches' }
             fam_inst' = fam_inst { fi_axiom = axiom'
                                  , fi_branches = toBranchList fiBranches' }

       ; isGHCi <- getIsGHCi
 
           -- In GHCi, we *override* any identical instances
           -- that are also defined in the interactive context
       ; let (home_fie', my_fis') 
               | isGHCi    = ( deleteFromFamInstEnv home_fie fam_inst'
                             , filterOut (identicalFamInst fam_inst') my_fis)
               | otherwise = (home_fie, my_fis)

           -- Load imported instances, so that we report
           -- overlaps correctly
       ; eps <- getEps
       ; let inst_envs  = (eps_fam_inst_env eps, home_fie')
             home_fie'' = extendFamInstEnv home_fie fam_inst'

           -- Check for conflicting instance decls
       ; no_conflict <- checkForConflicts inst_envs fam_inst'
       ; if no_conflict then
            return (home_fie'', fam_inst' : my_fis')
         else 
            return (home_fie,   my_fis) }

  where
    axiom = famInstAxiom fam_inst
    fiBranches = famInstBranches fam_inst

    zipWithAndUnzipM :: Monad m
                     => (a -> b -> m (c, d))
                     -> [a]
                     -> [b]
                     -> m ([c], [d])
    zipWithAndUnzipM f as bs
      = do { cds <- zipWithM f as bs
           ; return $ unzip cds }

    mk_skolem_tyvars :: CoAxBranch -> FamInstBranch
                     -> TcM (CoAxBranch, FamInstBranch)
    mk_skolem_tyvars axb fib
      = do { (subst, skol_tvs) <- tcInstSkolTyVars (coAxBranchTyVars axb)
           ; let axb' = coAxBranchSubst axb skol_tvs subst
                 fib' = famInstBranchSubst fib skol_tvs subst
           ; return (axb', fib') }

    -- substitute the tyvars for a new set of tyvars
    coAxBranchSubst :: CoAxBranch -> [TyVar] -> TvSubst -> CoAxBranch
    coAxBranchSubst (CoAxBranch { cab_lhs = lhs
                                , cab_rhs = rhs }) new_tvs subst
      = CoAxBranch { cab_tvs = new_tvs
                   , cab_lhs = substTys subst lhs
                   , cab_rhs = substTy subst rhs }

    -- substitute the current set of tyvars for another
    famInstBranchSubst :: FamInstBranch -> [TyVar] -> TvSubst -> FamInstBranch
    famInstBranchSubst fib@(FamInstBranch { fib_lhs = lhs
                                          , fib_rhs = rhs }) new_tvs subst
      = fib { fib_tvs = mkVarSet new_tvs
            , fib_lhs = substTys subst lhs
            , fib_rhs = substTy subst rhs }


\end{code}

%************************************************************************
%*									*
	Checking an instance against conflicts with an instance env
%*									*
%************************************************************************

Check whether a single family instance conflicts with those in two instance
environments (one for the EPS and one for the HPT).

\begin{code}
checkForConflicts :: FamInstEnvs -> FamInst Branched -> TcM Bool
checkForConflicts inst_envs fam_inst@(FamInst { fi_branches = branches
                                              , fi_group = group })
  = do { let conflicts = brListMap (lookupFamInstEnvConflicts inst_envs group fam_tc) branches
             no_conflicts = all null conflicts
       ; traceTc "checkForConflicts" (ppr conflicts $$ ppr fam_inst $$ ppr inst_envs)
       ; unless no_conflicts $
	   zipWithM_ (conflictInstErr fam_inst) (fromBranchList branches) conflicts
       ; return no_conflicts }
    where fam_tc = famInstTyCon fam_inst

conflictInstErr :: FamInst Branched -> FamInstBranch -> [FamInstMatch] -> TcRn ()
conflictInstErr fam_inst branch conflictingMatch
  | (FamInstMatch { fim_instance = confInst
                  , fim_index = confIndex }) : _ <- conflictingMatch
  = addFamInstsErr (ptext (sLit "Conflicting family instance declarations:"))
                   [(fam_inst, branch),
                    (confInst, famInstNthBranch confInst confIndex)]
  | otherwise
  = pprPanic "conflictInstErr" (pprFamInstBranch (famInstTyCon fam_inst) branch)

addFamInstsErr :: SDoc -> [(FamInst Branched, FamInstBranch)] -> TcRn ()
addFamInstsErr herald insts
  = setSrcSpan srcSpan $
    addErr (hang herald 2 $ vcat (zipWith pprFamInstBranchHdr
                                          sortedAxioms sortedBranches))
 where
   getSpan = famInstBranchSpan . snd
   sorted = sortWith getSpan insts
   srcSpan = getSpan $ head sorted

   sortedAxioms = map (famInstAxiom . fst) sorted
   sortedBranches = map snd sorted
   -- The sortWith just arranges that instances are dislayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users

tcGetFamInstEnvs :: TcM FamInstEnvs
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetFamInstEnvs 
  = do { eps <- getEps; env <- getGblEnv
       ; return (eps_fam_inst_env eps, tcg_fam_inst_env env) }
\end{code}
