--  $Id$
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  Monad maintaining parallel contexts and substitutions for flattening.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  The flattening transformation needs to perform a fair amount of plumbing.
--  It needs to mainatin a set of variables, called the parallel context for
--  lifting, variable substitutions in case alternatives, and so on.
--  Moreover, we need to manage uniques to create new variables.  The monad
--  defined in this module takes care of maintaining this state.
-- 
--- DOCU ----------------------------------------------------------------------
--
--  Language: Haskell 98
--
--  * a parallel context is a set of variables that get vectorised during a
--    lifting transformations (ie, their type changes from `t' to `[:t:]')
--
--  * all vectorised variables in a parallel context have the same size; we
--    call this also the size of the parallel context
--
--  * we represent contexts by maps that give the lifted version of a variable
--    (remember that in GHC, variables contain type information that changes
--    during lifting)
--
--- TODO ----------------------------------------------------------------------
--
--  * Assumptions currently made that should (if they turn out to be true) be
--    documented in The Commentary:
--
--    - Local bindings can be copied without any need to alpha-rename bound
--      variables (or their uniques).  Such renaming is only necessary when
--      bindings in a recursive group are replicated; implying that this is
--      required in the case of top-level bindings).  (Note: The CoreTidy path
--      generates global uniques before code generation.)
--
--  * One FIXME left to resolve.
--

module FlattenMonad (

  -- monad definition
  --
  Flatten, runFlatten,

  -- variable generation
  --
  newVar, mkBind,
  
  -- context management & query operations
  --
  extendContext, packContext, liftVar, liftConst, intersectWithContext,

  -- construction of prelude functions
  --
  mk'fst, mk'eq, mk'neq, mk'and, mk'or, mk'lengthP, mk'replicateP, mk'mapP,
  mk'bpermuteP, mk'bpermuteDftP, mk'indexOfP
) where

-- standard
import Monad	    (mplus)

-- GHC
import Panic        (panic)
import Outputable   (Outputable(ppr), pprPanic)
import UniqSupply   (UniqSupply, splitUniqSupply, uniqFromSupply)
import OccName	    (UserFS)
import Var          (Var, idType)
import Id	    (Id, mkSysLocal)
import Name	    (Name)
import VarSet       (VarSet, emptyVarSet, extendVarSet, varSetElems )
import VarEnv       (VarEnv, emptyVarEnv, zipVarEnv, plusVarEnv,
		     elemVarEnv, lookupVarEnv, lookupVarEnv_NF, delVarEnvList)
import Type	    (Type, tyConAppTyCon)
import HscTypes	    (HomePackageTable,
		     ExternalPackageState(eps_PTE), HscEnv(hsc_HPT),
		     TyThing(..), lookupType)
import PrelNames    ( fstName, andName, orName,
		     lengthPName, replicatePName, mapPName, bpermutePName,
		     bpermuteDftPName, indexOfPName)
import TysPrim      ( charPrimTyCon, intPrimTyCon, floatPrimTyCon, doublePrimTyCon )
import PrimOp	    ( PrimOp(..) )
import PrelInfo	    ( primOpId )
import CoreSyn      (Expr(..), Bind(..), CoreBndr, CoreExpr, CoreBind, mkApps)
import CoreUtils    (exprType)

-- friends
import NDPCoreUtils (parrElemTy)


-- definition of the monad
-- -----------------------

-- state maintained by the flattening monad
--
data FlattenState = FlattenState {

		      -- our source for uniques
		      --
		      us       :: UniqSupply,

		      -- environment containing all known names (including all
		      -- Prelude functions)
		      --
		      env      :: Name -> Id,

		      -- this variable determines the parallel context; if
		      -- `Nothing', we are in pure vectorisation mode, no
		      -- lifting going on
		      --
		      ctxtVar  :: Maybe Var,

		      -- environment that maps each variable that is
		      -- vectorised in the current parallel context to the
		      -- vectorised version of that variable
		      --
		      ctxtEnv :: VarEnv Var,

		      -- those variables from the *domain* of `ctxtEnv' that
		      -- have been used since the last context restriction (cf.
		      -- `restrictContext') 
		      --
		      usedVars :: VarSet
		    }

-- initial value of the flattening state
--
initialFlattenState :: ExternalPackageState
		    -> HomePackageTable 
		    -> UniqSupply 
		    -> FlattenState
initialFlattenState eps hpt us = 
  FlattenState {
    us	     = us,
    env      = lookup,
    ctxtVar  = Nothing,
    ctxtEnv  = emptyVarEnv,
    usedVars = emptyVarSet
  }
  where
    lookup n = 
      case lookupType hpt (eps_PTE eps) n of
        Just (AnId v) -> v 
	_             -> pprPanic "FlattenMonad: unknown name:" (ppr n)

-- the monad representation (EXPORTED ABSTRACTLY)
--
newtype Flatten a = Flatten {
		      unFlatten :: (FlattenState -> (a, FlattenState))
		    }

instance Monad Flatten where
  return x = Flatten $ \s -> (x, s)
  m >>= n  = Flatten $ \s -> let 
			       (r, s') = unFlatten m s
			     in
			     unFlatten (n r) s'

-- execute the given flattening computation (EXPORTED)
--
runFlatten :: HscEnv
	   -> ExternalPackageState
	   -> UniqSupply 
	   -> Flatten a 
	   -> a    
runFlatten hsc_env eps us m 
  = fst $ unFlatten m (initialFlattenState eps (hsc_HPT hsc_env) us)


-- variable generation
-- -------------------

-- generate a new local variable whose name is based on the given lexeme and
-- whose type is as specified in the second argument (EXPORTED)
--
newVar           :: UserFS -> Type -> Flatten Var
newVar lexeme ty  = Flatten $ \state ->
  let
    (us1, us2) = splitUniqSupply (us state)
    state'     = state {us = us2}
  in
  (mkSysLocal lexeme (uniqFromSupply us1) ty, state')

-- generate a non-recursive binding using a new binder whose name is derived
-- from the given lexeme (EXPORTED)
--
mkBind          :: UserFS -> CoreExpr -> Flatten (CoreBndr, CoreBind)
mkBind lexeme e  =
  do
    v <- newVar lexeme (exprType e)
    return (v, NonRec v e)


-- context management
-- ------------------

-- extend the parallel context by the given set of variables (EXPORTED)
--
--  * if there is no parallel context at the moment, the first element of the
--   variable list will be used to determine the new parallel context
--
--  * the second argument is executed in the current context extended with the
--   given variables
--
--  * the variables must already have been lifted by transforming their type,
--   but they *must* have retained their original name (or, at least, their
--   unique); this is needed so that they match the original variable in
--   variable environments
--
--  * any trace of the given set of variables has to be removed from the state
--   at the end of this operation
--
extendContext      :: [Var] -> Flatten a -> Flatten a
extendContext [] m  = m
extendContext vs m  = Flatten $ \state -> 
  let 
    extState       = state {
		       ctxtVar = ctxtVar state `mplus` Just (head vs),
		       ctxtEnv = ctxtEnv state `plusVarEnv` zipVarEnv vs vs
		     }
    (r, extState') = unFlatten m extState
    resState       = extState' { -- remove `vs' from the result state
		       ctxtVar  = ctxtVar state,
		       ctxtEnv  = ctxtEnv state,
		       usedVars = usedVars extState' `delVarEnvList` vs
		     }
  in
  (r, resState)

-- execute the second argument in a restricted context (EXPORTED)
--
--  * all variables in the current parallel context are packed according to
--   the permutation vector associated with the variable passed as the first
--   argument (ie, all elements of vectorised context variables that are
--   invalid in the restricted context are dropped)
--
--  * the returned list of core binders contains the operations that perform
--   the restriction on all variables in the parallel context that *do* occur
--   during the execution of the second argument (ie, `liftVar' is executed at
--   least once on any such variable)
--
packContext        :: Var -> Flatten a -> Flatten (a, [CoreBind])
packContext perm m  = Flatten $ \state ->
  let
    -- FIXME: To set the packed environment to the unpacked on is a hack of
    --   which I am not sure yet (a) whether it works and (b) whether it's
    --   really worth it.  The one advantages is that, we can use a var set,
    --   after all, instead of a var environment.
    --
    --	 The idea is the following: If we have to pack a variable `x', we
    --	 generate `let{-NonRec-} x = bpermuteP perm x in ...'.  As this is a
    --	 non-recursive binding, the lhs `x' overshadows the rhs `x' in the
    --	 body of the let.
    --
    --   NB: If we leave it like this, `mkCoreBind' can be simplified.
    packedCtxtEnv     = ctxtEnv state
    packedState       = state {
	                  ctxtVar  = fmap
				       (lookupVarEnv_NF packedCtxtEnv)
				       (ctxtVar state),
		          ctxtEnv  = packedCtxtEnv, 
		          usedVars = emptyVarSet
		        }
    (r, packedState') = unFlatten m packedState
    resState	      = state {    -- revert to the unpacked context
			  ctxtVar  = ctxtVar state,
			  ctxtEnv  = ctxtEnv state
		        }
    bndrs	      = map mkCoreBind . varSetElems . usedVars $ packedState'

    -- generate a binding for the packed variant of a context variable
    --
    mkCoreBind var    = let
			  rhs = fst $ unFlatten (mk'bpermuteP (idType var) 
							      (Var perm) 
							      (Var var)
						) state
			in
			NonRec (lookupVarEnv_NF packedCtxtEnv var) $ rhs
		          
  in
  ((r, bndrs), resState)

-- lift a single variable in the current context (EXPORTED)
--
--  * if the variable does not occur in the context, it's value is vectorised to
--   match the size of the current context
--
--  * otherwise, the variable is replaced by whatever the context environment
--   maps it to (this may either be simply the lifted version of the original
--   variable or a packed variant of that variable)
--
--  * the monad keeps track of all lifted variables that occur in the parallel
--   context, so that `packContext' can determine the correct set of core
--   bindings
--
liftVar     :: Var -> Flatten CoreExpr
liftVar var  = Flatten $ \s ->
  let 
    v          = ctxtVarErr s
    v'elemType = parrElemTy . idType $ v
    len        = fst $ unFlatten (mk'lengthP v'elemType (Var v)) s
    replicated = fst $ unFlatten (mk'replicateP (idType var) len (Var var)) s
  in case lookupVarEnv (ctxtEnv s) var of
    Just liftedVar -> (Var liftedVar, 
		       s {usedVars = usedVars s `extendVarSet` var})
    Nothing        -> (replicated, s)

-- lift a constant expression in the current context (EXPORTED)
--
--  * the value of the constant expression is vectorised to match the current
--   parallel context
--
liftConst   :: CoreExpr -> Flatten CoreExpr
liftConst e  = Flatten $ \s ->
  let
     v          = ctxtVarErr s
     v'elemType = parrElemTy . idType $ v
     len        = fst $ unFlatten (mk'lengthP v'elemType (Var v)) s
  in 
  (fst $ unFlatten (mk'replicateP (exprType e) len e ) s, s)

-- pick those variables of the given set that occur (if albeit in lifted form)
-- in the current parallel context (EXPORTED)
--
--  * the variables returned are from the given set and *not* the corresponding
--   context variables
--
intersectWithContext    :: VarSet -> Flatten [Var]
intersectWithContext vs  = Flatten $ \s ->
  let
    vs' = filter (`elemVarEnv` ctxtEnv s) (varSetElems vs)
  in
  (vs', s)


-- construct applications of prelude functions
-- -------------------------------------------

-- NB: keep all the used names listed in `FlattenInfo.namesNeededForFlattening'

-- generate an application of `fst' (EXPORTED)
--
mk'fst           :: Type -> Type -> CoreExpr -> Flatten CoreExpr
mk'fst ty1 ty2 a  = mkFunApp fstName [Type ty1, Type ty2, a]

-- generate an application of `&&' (EXPORTED)
--
mk'and       :: CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'and a1 a2  = mkFunApp andName [a1, a2]

-- generate an application of `||' (EXPORTED)
--
mk'or       :: CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'or a1 a2  = mkFunApp orName [a1, a2]

-- generate an application of `==' where the arguments may only be literals
-- that may occur in a Core case expression (i.e., `Char', `Int', `Float', and
-- `Double') (EXPORTED)
--
mk'eq          :: Type -> CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'eq ty a1 a2  = return (mkApps (Var eqName) [a1, a2])
		  where
		    tc = tyConAppTyCon ty
		    --
		    eqName | tc == charPrimTyCon   = primOpId CharEqOp
			   | tc == intPrimTyCon    = primOpId IntEqOp
			   | tc == floatPrimTyCon  = primOpId FloatEqOp
			   | tc == doublePrimTyCon = primOpId DoubleEqOp
			   | otherwise 		         =
			     pprPanic "FlattenMonad.mk'eq: " (ppr ty)

-- generate an application of `==' where the arguments may only be literals
-- that may occur in a Core case expression (i.e., `Char', `Int', `Float', and
-- `Double') (EXPORTED)
--
mk'neq          :: Type -> CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'neq ty a1 a2  = return (mkApps (Var neqName) [a1, a2])
		   where
		     tc = tyConAppTyCon ty
		     --
		     neqName {-  | name == charPrimTyConName   = neqCharName -}
			     | tc == intPrimTyCon	      = primOpId IntNeOp
			     {-  | name == floatPrimTyConName  = neqFloatName -}
			     {-  | name == doublePrimTyConName = neqDoubleName -}
			     | otherwise		   =
			       pprPanic "FlattenMonad.mk'neq: " (ppr ty)

-- generate an application of `lengthP' (EXPORTED)
--
mk'lengthP      :: Type -> CoreExpr -> Flatten CoreExpr
mk'lengthP ty a  = mkFunApp lengthPName [Type ty, a]

-- generate an application of `replicateP' (EXPORTED)
--
mk'replicateP          :: Type -> CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'replicateP ty a1 a2  = mkFunApp replicatePName [Type ty, a1, a2]

-- generate an application of `replicateP' (EXPORTED)
--
mk'mapP :: Type -> Type -> CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'mapP ty1 ty2 a1 a2  = mkFunApp mapPName [Type ty1, Type ty2, a1, a2]

-- generate an application of `bpermuteP' (EXPORTED)
--
mk'bpermuteP          :: Type -> CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'bpermuteP ty a1 a2  = mkFunApp bpermutePName [Type ty, a1, a2]

-- generate an application of `bpermuteDftP' (EXPORTED)
--
mk'bpermuteDftP :: Type -> CoreExpr -> CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'bpermuteDftP ty a1 a2 a3 = mkFunApp bpermuteDftPName [Type ty, a1, a2, a3]

-- generate an application of `indexOfP' (EXPORTED)
--
mk'indexOfP          :: Type -> CoreExpr -> CoreExpr -> Flatten CoreExpr
mk'indexOfP ty a1 a2  = mkFunApp indexOfPName [Type ty, a1, a2]


-- auxilliary functions
-- --------------------

-- obtain the context variable, aborting if it is not available (as this
-- signals an internal error in the usage of the `Flatten' monad)
--
ctxtVarErr   :: FlattenState -> Var
ctxtVarErr s  = case ctxtVar s of
		  Nothing -> panic "FlattenMonad.ctxtVarErr: No context variable available!"
		  Just v  -> v

-- given the name of a known function and a set of arguments (needs to include
-- all needed type arguments), build a Core expression that applies the named
-- function to those arguments
--
mkFunApp           :: Name -> [CoreExpr] -> Flatten CoreExpr
mkFunApp name args  =
  do
    fun <- lookupName name
    return $ mkApps (Var fun) args

-- get the `Id' of a known `Name'
--
--  * this can be the `Name' of any function that's visible on the toplevel of
--   the current compilation unit
--
lookupName      :: Name -> Flatten Id
lookupName name  = Flatten $ \s ->
  (env s name, s)
