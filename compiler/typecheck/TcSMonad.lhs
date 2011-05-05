\begin{code}
-- Type definitions for the constraint solver
module TcSMonad ( 

       -- Canonical constraints
    CanonicalCts, emptyCCan, andCCan, andCCans, 
    singleCCan, extendCCans, isEmptyCCan, isCTyEqCan, 
    isCDictCan_Maybe, isCIPCan_Maybe, isCFunEqCan_Maybe,
    isCFrozenErr,

    WorkList, unionWorkList, unionWorkLists, isEmptyWorkList, emptyWorkList,
    workListFromEq, workListFromNonEq,
    workListFromEqs, workListFromNonEqs, foldrWorkListM,

    CanonicalCt(..), Xi, tyVarsOfCanonical, tyVarsOfCanonicals, tyVarsOfCDicts, 
    deCanonicalise, mkFrozenError,

    isWanted, isGiven, isDerived,
    isGivenCt, isWantedCt, isDerivedCt, pprFlavorArising,

    isFlexiTcsTv,

    canRewrite, canSolve,
    combineCtLoc, mkGivenFlavor, mkWantedFlavor,
    getWantedLoc,

    TcS, runTcS, failTcS, panicTcS, traceTcS, -- Basic functionality 
    traceFireTcS, bumpStepCountTcS,
    tryTcS, nestImplicTcS, recoverTcS, wrapErrTcS, wrapWarnTcS,
    SimplContext(..), isInteractive, simplEqsOnly, performDefaulting,

       -- Creation of evidence variables
    newEvVar, newCoVar, newGivenCoVar,
    newDerivedId, 
    newIPVar, newDictVar, newKindConstraint,

       -- Setting evidence variables 
    setCoBind, setIPBind, setDictBind, setEvBind,

    setWantedTyBind,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getTcEvBinds, getUntouchables,
    getTcEvBindsBag, getTcSContext, getTcSTyBinds, getTcSTyBindsMap,

    newFlattenSkolemTy,                         -- Flatten skolems 


    instDFunTypes,                              -- Instantiation
    instDFunConstraints,          
    newFlexiTcSTy, instFlexiTcS,

    compatKind,

    TcsUntouchables,
    isTouchableMetaTyVar,
    isTouchableMetaTyVar_InRange, 

    getDefaultInfo, getDynFlags,

    matchClass, matchFam, MatchInstResult (..), 
    checkWellStagedDFun, 
    warnTcS,
    pprEq                                   -- Smaller utils, re-exported from TcM 
                                             -- TODO (DV): these are only really used in the 
                                             -- instance matcher in TcSimplify. I am wondering
                                             -- if the whole instance matcher simply belongs
                                             -- here 
) where 

#include "HsVersions.h"

import HscTypes
import BasicTypes 

import Inst
import InstEnv 
import FamInst 
import FamInstEnv

import qualified TcRnMonad as TcM
import qualified TcMType as TcM
import qualified TcEnv as TcM 
       ( checkWellStaged, topIdLvl, tcLookupFamInst, tcGetDefaultTys )
import TcType
import DynFlags

import Coercion
import Class
import TyCon
import TypeRep 

import Name
import Var
import VarEnv
import Outputable
import Bag
import MonadUtils
import VarSet
import FastString

import HsBinds               -- for TcEvBinds stuff 
import Id 
import TcRnTypes
import Data.IORef
#ifdef DEBUG
import StaticFlags( opt_PprStyle_Debug )
import Control.Monad( when )
#endif
\end{code}


%************************************************************************
%*									*
%*                       Canonical constraints                          *
%*                                                                      *
%*   These are the constraints the low-level simplifier works with      *
%*									*
%************************************************************************

\begin{code}
-- Types without any type functions inside.  However, note that xi
-- types CAN contain unexpanded type synonyms; however, the
-- (transitive) expansions of those type synonyms will not contain any
-- type functions.
type Xi = Type       -- In many comments, "xi" ranges over Xi

type CanonicalCts = Bag CanonicalCt
 
data CanonicalCt
  -- Atomic canonical constraints 
  = CDictCan {  -- e.g.  Num xi
      cc_id     :: EvVar,
      cc_flavor :: CtFlavor, 
      cc_class  :: Class, 
      cc_tyargs :: [Xi]
    }

  | CIPCan {	-- ?x::tau
      -- See note [Canonical implicit parameter constraints].
      cc_id     :: EvVar,
      cc_flavor :: CtFlavor,
      cc_ip_nm  :: IPName Name,
      cc_ip_ty  :: TcTauType
    }

  | CTyEqCan {  -- tv ~ xi	(recall xi means function free)
       -- Invariant: 
       --   * tv not in tvs(xi)   (occurs check)
       --   * typeKind xi `compatKind` typeKind tv
       --       See Note [Spontaneous solving and kind compatibility]
       --   * We prefer unification variables on the left *JUST* for efficiency
      cc_id     :: EvVar, 
      cc_flavor :: CtFlavor, 
      cc_tyvar  :: TcTyVar, 
      cc_rhs    :: Xi
    }

  | CFunEqCan {  -- F xis ~ xi  
                 -- Invariant: * isSynFamilyTyCon cc_fun 
                 --            * typeKind (F xis) `compatKind` typeKind xi
      cc_id     :: EvVar,
      cc_flavor :: CtFlavor, 
      cc_fun    :: TyCon,	-- A type function
      cc_tyargs :: [Xi],	-- Either under-saturated or exactly saturated
      cc_rhs    :: Xi      	--    *never* over-saturated (because if so
      		      		--    we should have decomposed)
                   
    }

  | CFrozenErr {      -- A "frozen error" does not interact with anything
                      -- See Note [Frozen Errors]
      cc_id     :: EvVar,
      cc_flavor :: CtFlavor
    }

mkFrozenError :: CtFlavor -> EvVar -> CanonicalCt
mkFrozenError fl ev = CFrozenErr { cc_id = ev, cc_flavor = fl }

compatKind :: Kind -> Kind -> Bool
compatKind k1 k2 = k1 `isSubKind` k2 || k2 `isSubKind` k1 

deCanonicalise :: CanonicalCt -> FlavoredEvVar
deCanonicalise ct = mkEvVarX (cc_id ct) (cc_flavor ct)

tyVarsOfCanonical :: CanonicalCt -> TcTyVarSet
tyVarsOfCanonical (CTyEqCan { cc_tyvar = tv, cc_rhs = xi })    = extendVarSet (tyVarsOfType xi) tv
tyVarsOfCanonical (CFunEqCan { cc_tyargs = tys, cc_rhs = xi }) = tyVarsOfTypes (xi:tys)
tyVarsOfCanonical (CDictCan { cc_tyargs = tys }) 	       = tyVarsOfTypes tys
tyVarsOfCanonical (CIPCan { cc_ip_ty = ty })                   = tyVarsOfType ty
tyVarsOfCanonical (CFrozenErr { cc_id = ev })                  = tyVarsOfEvVar ev

tyVarsOfCDict :: CanonicalCt -> TcTyVarSet 
tyVarsOfCDict (CDictCan { cc_tyargs = tys }) = tyVarsOfTypes tys
tyVarsOfCDict _ct                            = emptyVarSet 

tyVarsOfCDicts :: CanonicalCts -> TcTyVarSet 
tyVarsOfCDicts = foldrBag (unionVarSet . tyVarsOfCDict) emptyVarSet

tyVarsOfCanonicals :: CanonicalCts -> TcTyVarSet
tyVarsOfCanonicals = foldrBag (unionVarSet . tyVarsOfCanonical) emptyVarSet

instance Outputable CanonicalCt where
  ppr (CDictCan d fl cls tys)     
      = ppr fl <+> ppr d  <+> dcolon <+> pprClassPred cls tys
  ppr (CIPCan ip fl ip_nm ty)     
      = ppr fl <+> ppr ip <+> dcolon <+> parens (ppr ip_nm <> dcolon <> ppr ty)
  ppr (CTyEqCan co fl tv ty)      
      = ppr fl <+> ppr co <+> dcolon <+> pprEqPred (mkTyVarTy tv, ty)
  ppr (CFunEqCan co fl tc tys ty) 
      = ppr fl <+> ppr co <+> dcolon <+> pprEqPred (mkTyConApp tc tys, ty)
  ppr (CFrozenErr co fl)
      = ppr fl <+> pprEvVarWithType co
\end{code}

Note [Canonical implicit parameter constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type in a canonical implicit parameter constraint doesn't need to
be a xi (type-function-free type) since we can defer the flattening
until checking this type for equality with another type.  If we
encounter two IP constraints with the same name, they MUST have the
same type, and at that point we can generate a flattened equality
constraint between the types.  (On the other hand, the types in two
class constraints for the same class MAY be equal, so they need to be
flattened in the first place to facilitate comparing them.)

\begin{code}
singleCCan :: CanonicalCt -> CanonicalCts 
singleCCan = unitBag 

andCCan :: CanonicalCts -> CanonicalCts -> CanonicalCts 
andCCan = unionBags

extendCCans :: CanonicalCts -> CanonicalCt -> CanonicalCts 
extendCCans = snocBag 

andCCans :: [CanonicalCts] -> CanonicalCts 
andCCans = unionManyBags

emptyCCan :: CanonicalCts 
emptyCCan = emptyBag

isEmptyCCan :: CanonicalCts -> Bool
isEmptyCCan = isEmptyBag

isCTyEqCan :: CanonicalCt -> Bool 
isCTyEqCan (CTyEqCan {})  = True 
isCTyEqCan (CFunEqCan {}) = False
isCTyEqCan _              = False 

isCDictCan_Maybe :: CanonicalCt -> Maybe Class
isCDictCan_Maybe (CDictCan {cc_class = cls })  = Just cls
isCDictCan_Maybe _              = Nothing

isCIPCan_Maybe :: CanonicalCt -> Maybe (IPName Name)
isCIPCan_Maybe  (CIPCan {cc_ip_nm = nm }) = Just nm
isCIPCan_Maybe _                = Nothing

isCFunEqCan_Maybe :: CanonicalCt -> Maybe TyCon
isCFunEqCan_Maybe (CFunEqCan { cc_fun = tc }) = Just tc
isCFunEqCan_Maybe _ = Nothing

isCFrozenErr :: CanonicalCt -> Bool
isCFrozenErr (CFrozenErr {}) = True
isCFrozenErr _               = False


-- A mixture of Given, Wanted, and Derived constraints. 
-- We split between equalities and the rest to process equalities first. 
data WorkList = WorkList { weqs  :: CanonicalCts 
                                 -- NB: weqs includes equalities /and/ family equalities
                         , wrest :: CanonicalCts }

unionWorkList :: WorkList -> WorkList -> WorkList
unionWorkList wl1 wl2
  = WorkList { weqs = weqs wl1 `andCCan` weqs wl2
             , wrest = wrest wl1 `andCCan` wrest wl2 }

unionWorkLists :: [WorkList] -> WorkList 
unionWorkLists = foldr unionWorkList emptyWorkList

isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList wl = isEmptyCCan (weqs wl) && isEmptyCCan (wrest wl)

emptyWorkList :: WorkList
emptyWorkList
  = WorkList { weqs = emptyBag, wrest = emptyBag }

workListFromEq :: CanonicalCt -> WorkList
workListFromEq = workListFromEqs . singleCCan

workListFromNonEq :: CanonicalCt -> WorkList
workListFromNonEq = workListFromNonEqs . singleCCan 

workListFromNonEqs :: CanonicalCts -> WorkList
workListFromNonEqs cts
  = WorkList { weqs = emptyCCan, wrest = cts }

workListFromEqs :: CanonicalCts -> WorkList
workListFromEqs cts
  = WorkList { weqs = cts, wrest = emptyCCan }

foldrWorkListM :: (Monad m) => (CanonicalCt -> r -> m r) 
                           -> r -> WorkList -> m r
-- Prioritizes equalities
foldrWorkListM on_ct r (WorkList {weqs = eqs, wrest = rest })
  = do { r1 <- foldrBagM on_ct r eqs
       ; foldrBagM on_ct r1 rest }

instance Outputable WorkList where 
  ppr wl = vcat [ text "WorkList (Equalities) = " <+> ppr (weqs wl)
                , text "WorkList (Other)      = " <+> ppr (wrest wl) ]

\end{code}



%************************************************************************
%*									*
                    CtFlavor
         The "flavor" of a canonical constraint
%*									*
%************************************************************************

\begin{code}
getWantedLoc :: CanonicalCt -> WantedLoc
getWantedLoc ct 
  = ASSERT (isWanted (cc_flavor ct))
    case cc_flavor ct of 
      Wanted wl -> wl 
      _         -> pprPanic "Can't get WantedLoc of non-wanted constraint!" empty

isWantedCt :: CanonicalCt -> Bool
isWantedCt ct = isWanted (cc_flavor ct)
isGivenCt :: CanonicalCt -> Bool
isGivenCt ct = isGiven (cc_flavor ct)
isDerivedCt :: CanonicalCt -> Bool
isDerivedCt ct = isDerived (cc_flavor ct)

canSolve :: CtFlavor -> CtFlavor -> Bool 
-- canSolve ctid1 ctid2 
-- The constraint ctid1 can be used to solve ctid2 
-- "to solve" means a reaction where the active parts of the two constraints match.
--  active(F xis ~ xi) = F xis 
--  active(tv ~ xi)    = tv 
--  active(D xis)      = D xis 
--  active(IP nm ty)   = nm 
--
-- NB:  either (a `canSolve` b) or (b `canSolve` a) must hold
-----------------------------------------
canSolve (Given {})   _            = True 
canSolve (Wanted {})  (Derived {}) = True
canSolve (Wanted {})  (Wanted {})  = True
canSolve (Derived {}) (Derived {}) = True  -- Important: derived can't solve wanted/given
canSolve _ _ = False  	       	     	   -- (There is no *evidence* for a derived.)

canRewrite :: CtFlavor -> CtFlavor -> Bool 
-- canRewrite ctid1 ctid2 
-- The *equality_constraint* ctid1 can be used to rewrite inside ctid2 
canRewrite = canSolve 

combineCtLoc :: CtFlavor -> CtFlavor -> WantedLoc
-- Precondition: At least one of them should be wanted 
combineCtLoc (Wanted loc) _    = loc 
combineCtLoc _ (Wanted loc)    = loc 
combineCtLoc (Derived loc ) _  = loc 
combineCtLoc _ (Derived loc )  = loc 
combineCtLoc _ _ = panic "combineCtLoc: both given"

mkGivenFlavor :: CtFlavor -> SkolemInfo -> CtFlavor
mkGivenFlavor (Wanted  loc) sk  = Given (setCtLocOrigin loc sk)
mkGivenFlavor (Derived loc) sk  = Given (setCtLocOrigin loc sk)
mkGivenFlavor (Given   loc) sk  = Given (setCtLocOrigin loc sk)


mkWantedFlavor :: CtFlavor -> CtFlavor
mkWantedFlavor (Wanted  loc) = Wanted loc
mkWantedFlavor (Derived loc) = Wanted loc
mkWantedFlavor fl@(Given {}) = pprPanic "mkWantedFlavour" (ppr fl)
\end{code}

%************************************************************************
%*									*
%*		The TcS solver monad                                    *
%*									*
%************************************************************************

Note [The TcS monad]
~~~~~~~~~~~~~~~~~~~~
The TcS monad is a weak form of the main Tc monad

All you can do is
    * fail
    * allocate new variables
    * fill in evidence variables

Filling in a dictionary evidence variable means to create a binding
for it, so TcS carries a mutable location where the binding can be
added.  This is initialised from the innermost implication constraint.

\begin{code}
data TcSEnv
  = TcSEnv { 
      tcs_ev_binds :: EvBindsVar,
          -- Evidence bindings

      tcs_ty_binds :: IORef (TyVarEnv (TcTyVar, TcType)),
          -- Global type bindings

      tcs_context :: SimplContext,
                     
      tcs_untch :: TcsUntouchables,

      tcs_ic_depth :: Int,	-- Implication nesting depth
      tcs_count    :: IORef Int	-- Global step count
    }

type TcsUntouchables = (Untouchables,TcTyVarSet)
-- Like the TcM Untouchables, 
-- but records extra TcsTv variables generated during simplification
-- See Note [Extra TcsTv untouchables] in TcSimplify
\end{code}

\begin{code}
data SimplContext
  = SimplInfer SDoc	   -- Inferring type of a let-bound thing
  | SimplRuleLhs RuleName  -- Inferring type of a RULE lhs
  | SimplInteractive	   -- Inferring type at GHCi prompt
  | SimplCheck SDoc	   -- Checking a type signature or RULE rhs

instance Outputable SimplContext where
  ppr (SimplInfer d)   = ptext (sLit "SimplInfer") <+> d
  ppr (SimplCheck d)   = ptext (sLit "SimplCheck") <+> d
  ppr (SimplRuleLhs n) = ptext (sLit "SimplRuleLhs") <+> doubleQuotes (ftext n)
  ppr SimplInteractive = ptext (sLit "SimplInteractive")

isInteractive :: SimplContext -> Bool
isInteractive SimplInteractive = True
isInteractive _                = False

simplEqsOnly :: SimplContext -> Bool
-- Simplify equalities only, not dictionaries
-- This is used for the LHS of rules; ee
-- Note [Simplifying RULE lhs constraints] in TcSimplify
simplEqsOnly (SimplRuleLhs {}) = True
simplEqsOnly _                 = False

performDefaulting :: SimplContext -> Bool
performDefaulting (SimplInfer {})   = False
performDefaulting (SimplRuleLhs {}) = False
performDefaulting SimplInteractive  = True
performDefaulting (SimplCheck {})   = True

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a } 

instance Functor TcS where
  fmap f m = TcS $ fmap f . unTcS m

instance Monad TcS where 
  return x  = TcS (\_ -> return x) 
  fail err  = TcS (\_ -> fail err) 
  m >>= k   = TcS (\ebs -> unTcS m ebs >>= \r -> unTcS (k r) ebs)

-- Basic functionality 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wrapTcS :: TcM a -> TcS a 
-- Do not export wrapTcS, because it promotes an arbitrary TcM to TcS,
-- and TcS is supposed to have limited functionality
wrapTcS = TcS . const -- a TcM action will not use the TcEvBinds

wrapErrTcS :: TcM a -> TcS a 
-- The thing wrapped should just fail
-- There's no static check; it's up to the user
-- Having a variant for each error message is too painful
wrapErrTcS = wrapTcS

wrapWarnTcS :: TcM a -> TcS a 
-- The thing wrapped should just add a warning, or no-op
-- There's no static check; it's up to the user
wrapWarnTcS = wrapTcS

failTcS, panicTcS :: SDoc -> TcS a
failTcS      = wrapTcS . TcM.failWith
panicTcS doc = pprPanic "TcCanonical" doc

traceTcS :: String -> SDoc -> TcS ()
traceTcS herald doc = TcS $ \_env -> TcM.traceTc herald doc

bumpStepCountTcS :: TcS ()
bumpStepCountTcS = TcS $ \env -> do { let ref = tcs_count env
                                    ; n <- TcM.readTcRef ref
                                    ; TcM.writeTcRef ref (n+1) }

traceFireTcS :: Int -> SDoc -> TcS ()
-- Dump a rule-firing trace
traceFireTcS depth doc 
  = TcS $ \env -> 
    TcM.ifDOptM Opt_D_dump_cs_trace $ 
    do { n <- TcM.readTcRef (tcs_count env)
       ; let msg = int n 
                <> text (replicate (tcs_ic_depth env) '>')
                <> brackets (int depth) <+> doc
       ; TcM.dumpTcRn msg }

runTcS :: SimplContext
       -> Untouchables 	       -- Untouchables
       -> TcS a		       -- What to run
       -> TcM (a, Bag EvBind)
runTcS context untouch tcs 
  = do { ty_binds_var <- TcM.newTcRef emptyVarEnv
       ; ev_binds_var@(EvBindsVar evb_ref _) <- TcM.newTcEvBinds
       ; step_count <- TcM.newTcRef 0
       ; let env = TcSEnv { tcs_ev_binds = ev_binds_var
                          , tcs_ty_binds = ty_binds_var
                          , tcs_context  = context
                          , tcs_untch    = (untouch, emptyVarSet) -- No Tcs untouchables yet
			  , tcs_count    = step_count
			  , tcs_ic_depth = 0
                          }

	     -- Run the computation
       ; res <- unTcS tcs env
	     -- Perform the type unifications required
       ; ty_binds <- TcM.readTcRef ty_binds_var
       ; mapM_ do_unification (varEnvElts ty_binds)

#ifdef DEBUG
       ; count <- TcM.readTcRef step_count
       ; when (opt_PprStyle_Debug && count > 0) $
         TcM.debugDumpTcRn (ptext (sLit "Constraint solver steps =") 
                            <+> int count <+> ppr context)
#endif
             -- And return
       ; ev_binds      <- TcM.readTcRef evb_ref
       ; return (res, evBindMapBinds ev_binds) }
  where
    do_unification (tv,ty) = TcM.writeMetaTyVar tv ty

nestImplicTcS :: EvBindsVar -> TcsUntouchables -> TcS a -> TcS a
nestImplicTcS ref (inner_range, inner_tcs) (TcS thing_inside)
  = TcS $ \ TcSEnv { tcs_ty_binds = ty_binds 
    	    	   , tcs_untch = (_outer_range, outer_tcs)
		   , tcs_count = count
		   , tcs_ic_depth = idepth
                   , tcs_context = ctxt } ->
    let 
       inner_untch = (inner_range, outer_tcs `unionVarSet` inner_tcs)
       		   -- The inner_range should be narrower than the outer one
		   -- (thus increasing the set of untouchables) but 
		   -- the inner Tcs-untouchables must be unioned with the
		   -- outer ones!
       nest_env = TcSEnv { tcs_ev_binds = ref
                         , tcs_ty_binds = ty_binds
                         , tcs_untch    = inner_untch
                         , tcs_count    = count
                         , tcs_ic_depth = idepth+1
                         , tcs_context  = ctxtUnderImplic ctxt }
    in 
    thing_inside nest_env

recoverTcS :: TcS a -> TcS a -> TcS a
recoverTcS (TcS recovery_code) (TcS thing_inside)
  = TcS $ \ env ->
    TcM.recoverM (recovery_code env) (thing_inside env)

ctxtUnderImplic :: SimplContext -> SimplContext
-- See Note [Simplifying RULE lhs constraints] in TcSimplify
ctxtUnderImplic (SimplRuleLhs n) = SimplCheck (ptext (sLit "lhs of rule") 
                                               <+> doubleQuotes (ftext n))
ctxtUnderImplic ctxt              = ctxt

tryTcS :: TcS a -> TcS a
-- Like runTcS, but from within the TcS monad 
-- Ignore all the evidence generated, and do not affect caller's evidence!
tryTcS tcs 
  = TcS (\env -> do { ty_binds_var <- TcM.newTcRef emptyVarEnv
                    ; ev_binds_var <- TcM.newTcEvBinds
                    ; let env1 = env { tcs_ev_binds = ev_binds_var
                                     , tcs_ty_binds = ty_binds_var }
                    ; unTcS tcs env1 })

-- Update TcEvBinds 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getDynFlags :: TcS DynFlags
getDynFlags = wrapTcS TcM.getDOpts

getTcSContext :: TcS SimplContext
getTcSContext = TcS (return . tcs_context)

getTcEvBinds :: TcS EvBindsVar
getTcEvBinds = TcS (return . tcs_ev_binds) 

getUntouchables :: TcS TcsUntouchables
getUntouchables = TcS (return . tcs_untch)

getTcSTyBinds :: TcS (IORef (TyVarEnv (TcTyVar, TcType)))
getTcSTyBinds = TcS (return . tcs_ty_binds)

getTcSTyBindsMap :: TcS (TyVarEnv (TcTyVar, TcType))
getTcSTyBindsMap = getTcSTyBinds >>= wrapTcS . (TcM.readTcRef) 


getTcEvBindsBag :: TcS EvBindMap
getTcEvBindsBag
  = do { EvBindsVar ev_ref _ <- getTcEvBinds 
       ; wrapTcS $ TcM.readTcRef ev_ref }

setCoBind :: CoVar -> Coercion -> TcS () 
setCoBind cv co = setEvBind cv (EvCoercion co)

setWantedTyBind :: TcTyVar -> TcType -> TcS () 
-- Add a type binding
-- We never do this twice!
setWantedTyBind tv ty 
  = do { ref <- getTcSTyBinds
       ; wrapTcS $ 
         do { ty_binds <- TcM.readTcRef ref
#ifdef DEBUG
            ; TcM.checkErr (not (tv `elemVarEnv` ty_binds)) $
              vcat [ text "TERRIBLE ERROR: double set of meta type variable"
                   , ppr tv <+> text ":=" <+> ppr ty
                   , text "Old value =" <+> ppr (lookupVarEnv_NF ty_binds tv)]
#endif
            ; TcM.writeTcRef ref (extendVarEnv ty_binds tv (tv,ty)) } }

setIPBind :: EvVar -> EvTerm -> TcS () 
setIPBind = setEvBind 

setDictBind :: EvVar -> EvTerm -> TcS () 
setDictBind = setEvBind 

setEvBind :: EvVar -> EvTerm -> TcS () 
-- Internal
setEvBind ev rhs 
  = do { tc_evbinds <- getTcEvBinds
       ; wrapTcS (TcM.addTcEvBind tc_evbinds ev rhs) }

warnTcS :: CtLoc orig -> Bool -> SDoc -> TcS ()
warnTcS loc warn_if doc 
  | warn_if   = wrapTcS $ TcM.setCtLoc loc $ TcM.addWarnTc doc
  | otherwise = return ()

getDefaultInfo ::  TcS (SimplContext, [Type], (Bool, Bool))
getDefaultInfo 
  = do { ctxt <- getTcSContext
       ; (tys, flags) <- wrapTcS (TcM.tcGetDefaultTys (isInteractive ctxt))
       ; return (ctxt, tys, flags) }

-- Just get some environments needed for instance looking up and matching
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getInstEnvs :: TcS (InstEnv, InstEnv) 
getInstEnvs = wrapTcS $ Inst.tcGetInstEnvs 

getFamInstEnvs :: TcS (FamInstEnv, FamInstEnv) 
getFamInstEnvs = wrapTcS $ FamInst.tcGetFamInstEnvs

getTopEnv :: TcS HscEnv 
getTopEnv = wrapTcS $ TcM.getTopEnv 

getGblEnv :: TcS TcGblEnv 
getGblEnv = wrapTcS $ TcM.getGblEnv 

-- Various smaller utilities [TODO, maybe will be absorbed in the instance matcher]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

checkWellStagedDFun :: PredType -> DFunId -> WantedLoc -> TcS () 
checkWellStagedDFun pred dfun_id loc 
  = wrapTcS $ TcM.setCtLoc loc $ 
    do { use_stage <- TcM.getStage
       ; TcM.checkWellStaged pp_thing bind_lvl (thLevel use_stage) }
  where
    pp_thing = ptext (sLit "instance for") <+> quotes (ppr pred)
    bind_lvl = TcM.topIdLvl dfun_id

pprEq :: TcType -> TcType -> SDoc
pprEq ty1 ty2 = pprPred $ mkEqPred (ty1,ty2)

isTouchableMetaTyVar :: TcTyVar -> TcS Bool
isTouchableMetaTyVar tv 
  = do { untch <- getUntouchables
       ; return $ isTouchableMetaTyVar_InRange untch tv } 

isTouchableMetaTyVar_InRange :: TcsUntouchables -> TcTyVar -> Bool 
isTouchableMetaTyVar_InRange (untch,untch_tcs) tv 
  = case tcTyVarDetails tv of 
      MetaTv TcsTv _ -> not (tv `elemVarSet` untch_tcs)
                        -- See Note [Touchable meta type variables] 
      MetaTv {}      -> inTouchableRange untch tv 
      _              -> False 


\end{code}


Note [Touchable meta type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Meta type variables allocated *by the constraint solver itself* are always
touchable.  Example: 
   instance C a b => D [a] where...
if we use this instance declaration we "make up" a fresh meta type
variable for 'b', which we must later guess.  (Perhaps C has a
functional dependency.)  But since we aren't in the constraint *generator*
we can't allocate a Unique in the touchable range for this implication
constraint.  Instead, we mark it as a "TcsTv", which makes it always-touchable.


\begin{code}
-- Flatten skolems
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newFlattenSkolemTy :: TcType -> TcS TcType
newFlattenSkolemTy ty = mkTyVarTy <$> newFlattenSkolemTyVar ty

newFlattenSkolemTyVar :: TcType -> TcS TcTyVar
newFlattenSkolemTyVar ty
  = do { tv <- wrapTcS $ do { uniq <- TcM.newUnique
                            ; let name = TcM.mkTcTyVarName uniq (fsLit "f")
                            ; return $ mkTcTyVar name (typeKind ty) (FlatSkol ty) } 
       ; traceTcS "New Flatten Skolem Born" $ 
           (ppr tv <+> text "[:= " <+> ppr ty <+> text "]")
       ; return tv }

-- Instantiations 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instDFunTypes :: [Either TyVar TcType] -> TcS [TcType] 
instDFunTypes mb_inst_tys 
  = mapM inst_tv mb_inst_tys
  where
    inst_tv :: Either TyVar TcType -> TcS Type
    inst_tv (Left tv)  = mkTyVarTy <$> instFlexiTcS tv
    inst_tv (Right ty) = return ty 

instDFunConstraints :: TcThetaType -> TcS [EvVar] 
instDFunConstraints preds = wrapTcS $ TcM.newWantedEvVars preds 


instFlexiTcS :: TyVar -> TcS TcTyVar 
-- Like TcM.instMetaTyVar but the variable that is created is always
-- touchable; we are supposed to guess its instantiation. 
-- See Note [Touchable meta type variables] 
instFlexiTcS tv = instFlexiTcSHelper (tyVarName tv) (tyVarKind tv) 

newFlexiTcSTy :: Kind -> TcS TcType  
newFlexiTcSTy knd 
  = wrapTcS $
    do { uniq <- TcM.newUnique 
       ; ref  <- TcM.newMutVar  Flexi 
       ; let name = TcM.mkTcTyVarName uniq (fsLit "uf")
       ; return $ mkTyVarTy (mkTcTyVar name knd (MetaTv TcsTv ref)) }

isFlexiTcsTv :: TyVar -> Bool
isFlexiTcsTv tv
  | not (isTcTyVar tv)                  = False
  | MetaTv TcsTv _ <- tcTyVarDetails tv = True
  | otherwise                           = False

newKindConstraint :: TcTyVar -> Kind -> TcS CoVar
-- Create new wanted CoVar that constrains the type to have the specified kind. 
newKindConstraint tv knd 
  = do { tv_k <- instFlexiTcSHelper (tyVarName tv) knd 
       ; let ty_k = mkTyVarTy tv_k
       ; co_var <- newCoVar (mkTyVarTy tv) ty_k
       ; return co_var }

instFlexiTcSHelper :: Name -> Kind -> TcS TcTyVar
instFlexiTcSHelper tvname tvkind
  = wrapTcS $ 
    do { uniq <- TcM.newUnique 
       ; ref  <- TcM.newMutVar  Flexi 
       ; let name = setNameUnique tvname uniq 
             kind = tvkind 
       ; return (mkTcTyVar name kind (MetaTv TcsTv ref)) }

-- Superclasses and recursive dictionaries 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newEvVar :: TcPredType -> TcS EvVar
newEvVar pty = wrapTcS $ TcM.newEvVar pty

newDerivedId :: TcPredType -> TcS EvVar 
newDerivedId pty = wrapTcS $ TcM.newEvVar pty

newGivenCoVar :: TcType -> TcType -> Coercion -> TcS EvVar 
-- Note we create immutable variables for given or derived, since we
-- must bind them to TcEvBinds (because their evidence may involve 
-- superclasses). However we should be able to override existing
-- 'derived' evidence, even in TcEvBinds 
newGivenCoVar ty1 ty2 co 
  = do { cv <- newCoVar ty1 ty2
       ; setEvBind cv (EvCoercion co) 
       ; return cv } 

newCoVar :: TcType -> TcType -> TcS EvVar
newCoVar ty1 ty2 = wrapTcS $ TcM.newCoVar ty1 ty2 

newIPVar :: IPName Name -> TcType -> TcS EvVar 
newIPVar nm ty = wrapTcS $ TcM.newIP nm ty 

newDictVar :: Class -> [TcType] -> TcS EvVar 
newDictVar cl tys = wrapTcS $ TcM.newDict cl tys 
\end{code} 


\begin{code} 
-- Matching and looking up classes and family instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data MatchInstResult mi
  = MatchInstNo         -- No matching instance 
  | MatchInstSingle mi  -- Single matching instance
  | MatchInstMany       -- Multiple matching instances


matchClass :: Class -> [Type] -> TcS (MatchInstResult (DFunId, [Either TyVar TcType])) 
-- Look up a class constraint in the instance environment
matchClass clas tys
  = do	{ let pred = mkClassPred clas tys 
        ; instEnvs <- getInstEnvs
        ; case lookupInstEnv instEnvs clas tys of {
            ([], unifs)               -- Nothing matches  
                -> do { traceTcS "matchClass not matching"
                                 (vcat [ text "dict" <+> ppr pred, 
                                         text "unifs" <+> ppr unifs ]) 
                      ; return MatchInstNo  
                      } ;  
	    ([(ispec, inst_tys)], []) -- A single match 
		-> do	{ let dfun_id = is_dfun ispec
			; traceTcS "matchClass success"
				   (vcat [text "dict" <+> ppr pred, 
				          text "witness" <+> ppr dfun_id
                                           <+> ppr (idType dfun_id) ])
				  -- Record that this dfun is needed
                        ; return $ MatchInstSingle (dfun_id, inst_tys)
                        } ;
     	    (matches, unifs)          -- More than one matches 
		-> do	{ traceTcS "matchClass multiple matches, deferring choice"
			           (vcat [text "dict" <+> ppr pred,
				   	  text "matches" <+> ppr matches,
				   	  text "unifs" <+> ppr unifs])
                        ; return MatchInstMany 
		        }
	}
        }

matchFam :: TyCon
         -> [Type] 
         -> TcS (MatchInstResult (TyCon, [Type]))
matchFam tycon args
  = do { mb <- wrapTcS $ TcM.tcLookupFamInst tycon args
       ; case mb of 
           Nothing  -> return MatchInstNo 
           Just res -> return $ MatchInstSingle res
       -- DV: We never return MatchInstMany, since tcLookupFamInst never returns 
       -- multiple matches. Check. 
       }
\end{code}
