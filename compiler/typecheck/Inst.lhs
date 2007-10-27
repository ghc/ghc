%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The @Inst@ type: dictionaries or method instances

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Inst ( 
	Inst, 

	pprInstances, pprDictsTheta, pprDictsInFull,	-- User error messages
	showLIE, pprInst, pprInsts, pprInstInFull,	-- Debugging messages

	tidyInsts, tidyMoreInsts,

	newDictBndr, newDictBndrs, newDictBndrsO,
	instCall, instStupidTheta,
	cloneDict, 
	shortCutFracLit, shortCutIntLit, shortCutStringLit, newIPDict, 
	newMethod, newMethodFromName, newMethodWithGivenTy, 
	tcInstClassOp, 
	tcSyntaxName, isHsVar,

	tyVarsOfInst, tyVarsOfInsts, tyVarsOfLIE, 
	ipNamesOfInst, ipNamesOfInsts, fdPredsOfInst, fdPredsOfInsts,
	getDictClassTys, dictPred,

	lookupSimpleInst, LookupInstResult(..), 
	tcExtendLocalInstEnv, tcGetInstEnvs, getOverlapFlag,

	isAbstractableInst, isEqInst,
	isDict, isClassDict, isMethod, isImplicInst,
	isIPDict, isInheritableInst, isMethodOrLit,
	isTyVarDict, isMethodFor, 

	zonkInst, zonkInsts,
	instToId, instToVar, instType, instName, instToDictBind,
	addInstToDictBind,

	InstOrigin(..), InstLoc, pprInstLoc,

	mkWantedCo, mkGivenCo,
	fromWantedCo, fromGivenCo,
	eitherEqInst, mkEqInst, mkEqInsts, mkWantedEqInst,
	finalizeEqInst, writeWantedCoercion,
	eqInstType, updateEqInstCoercion,
	eqInstCoercion,
	eqInstLeftTy, eqInstRightTy
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcPolyExpr )
import {-# SOURCE #-}	TcUnify( boxyUnify, unifyType )

import FastString(FastString)
import HsSyn
import TcHsSyn
import TcRnMonad
import TcEnv
import InstEnv
import FunDeps
import TcMType
import TcType
import Type
import TypeRep
import Class
import Unify
import Module
import Coercion
import HscTypes
import CoreFVs
import DataCon
import Id
import Name
import NameSet
import Literal
import Var      ( Var, TyVar )
import qualified Var
import VarEnv
import VarSet
import TysWiredIn
import PrelNames
import BasicTypes
import SrcLoc
import DynFlags
import Bag
import Maybes
import Util
import Outputable
import Data.List
import TypeRep
import Class

import Control.Monad ( liftM )
\end{code}


Selection
~~~~~~~~~
\begin{code}
instName :: Inst -> Name
instName (EqInst {tci_name = name}) = name
instName inst = Var.varName (instToVar inst)

instToId :: Inst -> TcId
instToId inst = WARN( not (isId id), ppr inst ) 
	      id 
	      where
		id = instToVar inst

instToVar :: Inst -> Var
instToVar (LitInst {tci_name = nm, tci_ty = ty})
  = mkLocalId nm ty
instToVar (Method {tci_id = id}) 
  = id
instToVar (Dict {tci_name = nm, tci_pred = pred})    
  | isEqPred pred = Var.mkCoVar nm (mkPredTy pred)
  | otherwise	  = mkLocalId nm (mkPredTy pred)
instToVar (ImplicInst {tci_name = nm, tci_tyvars = tvs, tci_given = givens,
		       tci_wanted = wanteds})
  = mkLocalId nm (mkImplicTy tvs givens wanteds)
instToVar i@(EqInst {})
  = eitherEqInst i id (\(TyVarTy covar) -> covar)

instType :: Inst -> Type
instType (LitInst {tci_ty = ty})  = ty
instType (Method {tci_id = id})   = idType id
instType (Dict {tci_pred = pred}) = mkPredTy pred
instType imp@(ImplicInst {})      = mkImplicTy (tci_tyvars imp) (tci_given imp)	
					       (tci_wanted imp)
-- instType i@(EqInst {tci_co = co}) = eitherEqInst i TyVarTy id
instType (EqInst {tci_left = ty1, tci_right = ty2}) = mkPredTy (EqPred ty1 ty2)

mkImplicTy tvs givens wanteds	-- The type of an implication constraint
  = ASSERT( all isAbstractableInst givens )
    -- pprTrace "mkImplicTy" (ppr givens) $
    -- See [Equational Constraints in Implication Constraints]
    let dict_wanteds = filter (not . isEqInst) wanteds
    in 
      mkForAllTys tvs $ 
      mkPhiTy (map dictPred givens) $
      if isSingleton dict_wanteds then
  	instType (head dict_wanteds) 
      else
  	mkTupleTy Boxed (length dict_wanteds) (map instType dict_wanteds)

dictPred (Dict {tci_pred = pred}) = pred
dictPred (EqInst {tci_left=ty1,tci_right=ty2}) = EqPred ty1 ty2
dictPred inst		          = pprPanic "dictPred" (ppr inst)

getDictClassTys (Dict {tci_pred = pred}) = getClassPredTys pred
getDictClassTys inst		         = pprPanic "getDictClassTys" (ppr inst)

-- fdPredsOfInst is used to get predicates that contain functional 
-- dependencies *or* might do so.  The "might do" part is because
-- a constraint (C a b) might have a superclass with FDs
-- Leaving these in is really important for the call to fdPredsOfInsts
-- in TcSimplify.inferLoop, because the result is fed to 'grow',
-- which is supposed to be conservative
fdPredsOfInst (Dict {tci_pred = pred}) 	     = [pred]
fdPredsOfInst (Method {tci_theta = theta})   = theta
fdPredsOfInst (ImplicInst {tci_given = gs, 
			   tci_wanted = ws}) = fdPredsOfInsts (gs ++ ws)
fdPredsOfInst (LitInst {})		     = []
fdPredsOfInst (EqInst {})		     = []

fdPredsOfInsts :: [Inst] -> [PredType]
fdPredsOfInsts insts = concatMap fdPredsOfInst insts

isInheritableInst (Dict {tci_pred = pred})     = isInheritablePred pred
isInheritableInst (Method {tci_theta = theta}) = all isInheritablePred theta
isInheritableInst other			       = True


---------------------------------
-- Get the implicit parameters mentioned by these Insts
-- NB: the results of these functions are insensitive to zonking

ipNamesOfInsts :: [Inst] -> [Name]
ipNamesOfInst  :: Inst   -> [Name]
ipNamesOfInsts insts = [n | inst <- insts, n <- ipNamesOfInst inst]

ipNamesOfInst (Dict {tci_pred = IParam n _}) = [ipNameName n]
ipNamesOfInst (Method {tci_theta = theta})   = [ipNameName n | IParam n _ <- theta]
ipNamesOfInst other		    	     = []

---------------------------------
tyVarsOfInst :: Inst -> TcTyVarSet
tyVarsOfInst (LitInst {tci_ty = ty})  = tyVarsOfType  ty
tyVarsOfInst (Dict {tci_pred = pred}) = tyVarsOfPred pred
tyVarsOfInst (Method {tci_oid = id, tci_tys = tys}) = tyVarsOfTypes tys `unionVarSet` varTypeTyVars id
				 -- The id might have free type variables; in the case of
				 -- locally-overloaded class methods, for example
tyVarsOfInst (ImplicInst {tci_tyvars = tvs, tci_given = givens, tci_wanted = wanteds})
  = (tyVarsOfInsts givens `unionVarSet` tyVarsOfInsts wanteds) 
    `minusVarSet` mkVarSet tvs
    `unionVarSet` unionVarSets (map varTypeTyVars tvs)
		-- Remember the free tyvars of a coercion
tyVarsOfInst (EqInst {tci_left = ty1, tci_right = ty2}) = tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2

tyVarsOfInsts insts = foldr (unionVarSet . tyVarsOfInst) emptyVarSet insts
tyVarsOfLIE   lie   = tyVarsOfInsts (lieToList lie)


--------------------------
instToDictBind :: Inst -> LHsExpr TcId -> TcDictBinds
instToDictBind inst rhs 
  = unitBag (L (instSpan inst) (VarBind (instToId inst) rhs))

addInstToDictBind :: TcDictBinds -> Inst -> LHsExpr TcId -> TcDictBinds
addInstToDictBind binds inst rhs = binds `unionBags` instToDictBind inst rhs
\end{code}

Predicates
~~~~~~~~~~
\begin{code}

isAbstractableInst :: Inst -> Bool
isAbstractableInst inst = isDict inst || isEqInst inst

isEqInst :: Inst -> Bool
isEqInst (EqInst {}) = True
isEqInst other       = False

isDict :: Inst -> Bool
isDict (Dict {}) = True
isDict other	 = False

isClassDict :: Inst -> Bool
isClassDict (Dict {tci_pred = pred}) = isClassPred pred
isClassDict other	    	     = False

isTyVarDict :: Inst -> Bool
isTyVarDict (Dict {tci_pred = pred}) = isTyVarClassPred pred
isTyVarDict other	    	     = False

isIPDict :: Inst -> Bool
isIPDict (Dict {tci_pred = pred}) = isIPPred pred
isIPDict other		 	  = False

isImplicInst (ImplicInst {}) = True
isImplicInst other 	     = False

isMethod :: Inst -> Bool
isMethod (Method {}) = True
isMethod other	     = False

isMethodFor :: TcIdSet -> Inst -> Bool
isMethodFor ids (Method {tci_oid = id}) = id `elemVarSet` ids
isMethodFor ids inst			= False

isMethodOrLit :: Inst -> Bool
isMethodOrLit (Method {})  = True
isMethodOrLit (LitInst {}) = True
isMethodOrLit other        = False
\end{code}


%************************************************************************
%*									*
\subsection{Building dictionaries}
%*									*
%************************************************************************

-- newDictBndrs makes a dictionary at a binding site
-- instCall makes a dictionary at an occurrence site
--	and throws it into the LIE

\begin{code}
----------------
newDictBndrsO :: InstOrigin -> TcThetaType -> TcM [Inst]
newDictBndrsO orig theta = do { loc <- getInstLoc orig
			      ; newDictBndrs loc theta }

newDictBndrs :: InstLoc -> TcThetaType -> TcM [Inst]
newDictBndrs inst_loc theta = mapM (newDictBndr inst_loc) theta

newDictBndr :: InstLoc -> TcPredType -> TcM Inst
newDictBndr inst_loc pred@(EqPred ty1 ty2)
  = do { uniq <- newUnique 
	; let name = mkPredName uniq inst_loc pred 
	; return (EqInst {tci_name  = name, 
			  tci_loc   = inst_loc, 
			  tci_left  = ty1, 
			  tci_right = ty2, 
			  tci_co    = mkGivenCo $ TyVarTy (Var.mkCoVar name (PredTy pred))})
       }
newDictBndr inst_loc pred
  = do 	{ uniq <- newUnique 
	; let name = mkPredName uniq inst_loc pred 
	; return (Dict {tci_name = name, tci_pred = pred, tci_loc = inst_loc}) }

----------------
instCall :: InstOrigin -> [TcType] -> TcThetaType -> TcM HsWrapper
-- Instantiate the constraints of a call
--	(instCall o tys theta)
-- (a) Makes fresh dictionaries as necessary for the constraints (theta)
-- (b) Throws these dictionaries into the LIE
-- (c) Returns an HsWrapper ([.] tys dicts)

instCall orig tys theta 
  = do	{ loc <- getInstLoc orig
	; dict_app <- instCallDicts loc theta
	; return (dict_app <.> mkWpTyApps tys) }

----------------
instStupidTheta :: InstOrigin -> TcThetaType -> TcM ()
-- Similar to instCall, but only emit the constraints in the LIE
-- Used exclusively for the 'stupid theta' of a data constructor
instStupidTheta orig theta
  = do	{ loc <- getInstLoc orig
	; _co <- instCallDicts loc theta	-- Discard the coercion
	; return () }

----------------
instCallDicts :: InstLoc -> TcThetaType -> TcM HsWrapper
-- Instantiates the TcTheta, puts all constraints thereby generated
-- into the LIE, and returns a HsWrapper to enclose the call site.
-- This is the key place where equality predicates 
-- are unleashed into the world
instCallDicts loc [] = return idHsWrapper

-- instCallDicts loc (EqPred ty1 ty2 : preds)
--   = do  { unifyType ty1 ty2	-- For now, we insist that they unify right away 
-- 				-- Later on, when we do associated types, 
-- 				-- unifyType :: Type -> Type -> TcM ([Inst], Coercion)
-- 	; (dicts, co_fn) <- instCallDicts loc preds
-- 	; return (dicts, co_fn <.> WpTyApp ty1) }
-- 	-- We use type application to apply the function to the 
-- 	-- coercion; here ty1 *is* the appropriate identity coercion

instCallDicts loc (EqPred ty1 ty2 : preds)
  = do  { traceTc (text "instCallDicts" <+> ppr (EqPred ty1 ty2))
	; coi <- boxyUnify ty1 ty2
--	; coi <- unifyType ty1 ty2
	; let co = fromCoI coi ty1
	; co_fn <- instCallDicts loc preds
	; return (co_fn <.> WpTyApp co) }

instCallDicts loc (pred : preds)
  = do	{ uniq <- newUnique
	; let name = mkPredName uniq loc pred 
	      dict = Dict {tci_name = name, tci_pred = pred, tci_loc = loc}
	; extendLIE dict
	; co_fn <- instCallDicts loc preds
	; return (co_fn <.> WpApp (instToId dict)) }

-------------
cloneDict :: Inst -> TcM Inst
cloneDict dict@(Dict nm ty loc) = do { uniq <- newUnique
				     ; return (dict {tci_name = setNameUnique nm uniq}) }
cloneDict eq@(EqInst {})	= return eq
cloneDict other = pprPanic "cloneDict" (ppr other)

-- For vanilla implicit parameters, there is only one in scope
-- at any time, so we used to use the name of the implicit parameter itself
-- But with splittable implicit parameters there may be many in 
-- scope, so we make up a new namea.
newIPDict :: InstOrigin -> IPName Name -> Type 
	  -> TcM (IPName Id, Inst)
newIPDict orig ip_name ty
  = getInstLoc orig			`thenM` \ inst_loc ->
    newUnique				`thenM` \ uniq ->
    let
	pred = IParam ip_name ty
        name = mkPredName uniq inst_loc pred 
	dict = Dict {tci_name = name, tci_pred = pred, tci_loc = inst_loc}
    in
    returnM (mapIPName (\n -> instToId dict) ip_name, dict)
\end{code}


\begin{code}
mkPredName :: Unique -> InstLoc -> PredType -> Name
mkPredName uniq loc pred_ty
  = mkInternalName uniq occ (instLocSpan loc)
  where
    occ = case pred_ty of
	    ClassP cls _ -> mkDictOcc (getOccName cls)
	    IParam ip  _ -> getOccName (ipNameName ip)
	    EqPred ty  _ -> mkEqPredCoOcc baseOcc
	      where
		-- we use the outermost tycon of the lhs, if there is one, to
		-- improve readability of Core code
	        baseOcc = case splitTyConApp_maybe ty of
			    Nothing      -> mkOccName tcName "$"
                            Just (tc, _) -> getOccName tc
\end{code}

%************************************************************************
%*									*
\subsection{Building methods (calls of overloaded functions)}
%*									*
%************************************************************************


\begin{code}
newMethodFromName :: InstOrigin -> BoxyRhoType -> Name -> TcM TcId
newMethodFromName origin ty name
  = tcLookupId name		`thenM` \ id ->
	-- Use tcLookupId not tcLookupGlobalId; the method is almost
	-- always a class op, but with -fno-implicit-prelude GHC is
	-- meant to find whatever thing is in scope, and that may
	-- be an ordinary function. 
    getInstLoc origin		`thenM` \ loc ->
    tcInstClassOp loc id [ty]	`thenM` \ inst ->
    extendLIE inst		`thenM_`
    returnM (instToId inst)

newMethodWithGivenTy orig id tys
  = getInstLoc orig		`thenM` \ loc ->
    newMethod loc id tys	`thenM` \ inst ->
    extendLIE inst		`thenM_`
    returnM (instToId inst)

--------------------------------------------
-- tcInstClassOp, and newMethod do *not* drop the 
-- Inst into the LIE; they just returns the Inst
-- This is important because they are used by TcSimplify
-- to simplify Insts

-- NB: the kind of the type variable to be instantiated
--     might be a sub-kind of the type to which it is applied,
--     notably when the latter is a type variable of kind ??
--     Hence the call to checkKind
-- A worry: is this needed anywhere else?
tcInstClassOp :: InstLoc -> Id -> [TcType] -> TcM Inst
tcInstClassOp inst_loc sel_id tys
  = let
	(tyvars, _rho) = tcSplitForAllTys (idType sel_id)
    in
    zipWithM_ checkKind tyvars tys	`thenM_` 
    newMethod inst_loc sel_id tys

checkKind :: TyVar -> TcType -> TcM ()
-- Ensure that the type has a sub-kind of the tyvar
checkKind tv ty
  = do	{ let ty1 = ty 
		-- ty1 <- zonkTcType ty
	; if typeKind ty1 `isSubKind` Var.tyVarKind tv
	  then return ()
	  else 

    pprPanic "checkKind: adding kind constraint" 
	     (vcat [ppr tv <+> ppr (Var.tyVarKind tv), 
	            ppr ty <+> ppr ty1 <+> ppr (typeKind ty1)])
	}
--    do	{ tv1 <- tcInstTyVar tv
--	; unifyType ty1 (mkTyVarTy tv1) } }


---------------------------
newMethod inst_loc id tys
  = newUnique		`thenM` \ new_uniq ->
    let
	(theta,tau) = tcSplitPhiTy (applyTys (idType id) tys)
	meth_id	    = mkUserLocal (mkMethodOcc (getOccName id)) new_uniq tau loc
	inst        = Method {tci_id = meth_id, tci_oid = id, tci_tys = tys,
			      tci_theta = theta, tci_loc = inst_loc}
	loc         = instLocSpan inst_loc
    in
    returnM inst
\end{code}

\begin{code}
shortCutIntLit :: Integer -> TcType -> Maybe (HsExpr TcId)
shortCutIntLit i ty
  | isIntTy ty && inIntRange i 		-- Short cut for Int
  = Just (HsLit (HsInt i))
  | isIntegerTy ty 			-- Short cut for Integer
  = Just (HsLit (HsInteger i ty))
  | otherwise = Nothing

shortCutFracLit :: Rational -> TcType -> Maybe (HsExpr TcId)
shortCutFracLit f ty
  | isFloatTy ty 
  = Just (mk_lit floatDataCon (HsFloatPrim f))
  | isDoubleTy ty
  = Just (mk_lit doubleDataCon (HsDoublePrim f))
  | otherwise = Nothing
  where
    mk_lit con lit = HsApp (nlHsVar (dataConWrapId con)) (nlHsLit lit)

shortCutStringLit :: FastString -> TcType -> Maybe (HsExpr TcId)
shortCutStringLit s ty
  | isStringTy ty 			-- Short cut for String
  = Just (HsLit (HsString s))
  | otherwise = Nothing

mkIntegerLit :: Integer -> TcM (LHsExpr TcId)
mkIntegerLit i
  = tcMetaTy integerTyConName 	`thenM` \ integer_ty ->
    getSrcSpanM			`thenM` \ span -> 
    returnM (L span $ HsLit (HsInteger i integer_ty))

mkRatLit :: Rational -> TcM (LHsExpr TcId)
mkRatLit r
  = tcMetaTy rationalTyConName 	`thenM` \ rat_ty ->
    getSrcSpanM			`thenM` \ span -> 
    returnM (L span $ HsLit (HsRat r rat_ty))

mkStrLit :: FastString -> TcM (LHsExpr TcId)
mkStrLit s
  = --tcMetaTy stringTyConName 	`thenM` \ string_ty ->
    getSrcSpanM			`thenM` \ span -> 
    returnM (L span $ HsLit (HsString s))

isHsVar :: HsExpr Name -> Name -> Bool
isHsVar (HsVar f) g = f==g
isHsVar other 	  g = False
\end{code}


%************************************************************************
%*									*
\subsection{Zonking}
%*									*
%************************************************************************

Zonking makes sure that the instance types are fully zonked.

\begin{code}
zonkInst :: Inst -> TcM Inst
zonkInst dict@(Dict { tci_pred = pred})
  = zonkTcPredType pred			`thenM` \ new_pred ->
    returnM (dict {tci_pred = new_pred})

zonkInst meth@(Method {tci_oid = id, tci_tys = tys, tci_theta = theta}) 
  = zonkId id			`thenM` \ new_id ->
	-- Essential to zonk the id in case it's a local variable
	-- Can't use zonkIdOcc because the id might itself be
	-- an InstId, in which case it won't be in scope

    zonkTcTypes tys		`thenM` \ new_tys ->
    zonkTcThetaType theta	`thenM` \ new_theta ->
    returnM (meth { tci_oid = new_id, tci_tys = new_tys, tci_theta = new_theta })
	-- No need to zonk the tci_id

zonkInst lit@(LitInst {tci_ty = ty})
  = zonkTcType ty			`thenM` \ new_ty ->
    returnM (lit {tci_ty = new_ty})

zonkInst implic@(ImplicInst {})
  = ASSERT( all isImmutableTyVar (tci_tyvars implic) )
    do 	{ givens'  <- zonkInsts (tci_given  implic)
	; wanteds' <- zonkInsts (tci_wanted implic)
	; return (implic {tci_given = givens',tci_wanted = wanteds'}) }

zonkInst eqinst@(EqInst {tci_left = ty1, tci_right = ty2})
  = do { co' <- eitherEqInst eqinst 
		  (\covar -> return (mkWantedCo covar)) 
		  (\co    -> liftM mkGivenCo $ zonkTcType co)
       ; ty1' <- zonkTcType ty1
       ; ty2' <- zonkTcType ty2
       ; return (eqinst {tci_co = co', tci_left= ty1', tci_right = ty2' })
       }

zonkInsts insts = mappM zonkInst insts
\end{code}


%************************************************************************
%*									*
\subsection{Printing}
%*									*
%************************************************************************

ToDo: improve these pretty-printing things.  The ``origin'' is really only
relevant in error messages.

\begin{code}
instance Outputable Inst where
    ppr inst = pprInst inst

pprDictsTheta :: [Inst] -> SDoc
-- Print in type-like fashion (Eq a, Show b)
-- The Inst can be an implication constraint, but not a Method or LitInst
pprDictsTheta insts = parens (sep (punctuate comma (map (ppr . instType) insts)))

pprDictsInFull :: [Inst] -> SDoc
-- Print in type-like fashion, but with source location
pprDictsInFull dicts 
  = vcat (map go dicts)
  where
    go dict = sep [quotes (ppr (instType dict)), nest 2 (pprInstArising dict)]

pprInsts :: [Inst] -> SDoc
-- Debugging: print the evidence :: type
pprInsts insts = brackets (interpp'SP insts)

pprInst, pprInstInFull :: Inst -> SDoc
-- Debugging: print the evidence :: type
pprInst i@(EqInst {tci_left = ty1, tci_right = ty2, tci_co = co}) 
	= eitherEqInst i
		(\covar -> text "Wanted" <+> ppr (TyVarTy covar) <+> dcolon <+> ppr (EqPred ty1 ty2))
		(\co    -> text "Given"  <+> ppr co              <+> dcolon <+> ppr (EqPred ty1 ty2))
pprInst inst = ppr (instName inst) <+> dcolon 
		<+> (braces (ppr (instType inst)) $$
		     ifPprDebug implic_stuff)
  where
    implic_stuff | isImplicInst inst = ppr (tci_reft inst)
		 | otherwise	     = empty

pprInstInFull inst@(EqInst {}) = pprInst inst
pprInstInFull inst = sep [quotes (pprInst inst), nest 2 (pprInstArising inst)]

tidyInst :: TidyEnv -> Inst -> Inst
tidyInst env eq@(EqInst {tci_left = lty, tci_right = rty, tci_co = co}) =
  eq { tci_left  = tidyType env lty
     , tci_right = tidyType env rty
     , tci_co    = either Left (Right . tidyType env) co
     }
tidyInst env lit@(LitInst {tci_ty = ty})   = lit {tci_ty = tidyType env ty}
tidyInst env dict@(Dict {tci_pred = pred}) = dict {tci_pred = tidyPred env pred}
tidyInst env meth@(Method {tci_tys = tys}) = meth {tci_tys = tidyTypes env tys}
tidyInst env implic@(ImplicInst {})
  = implic { tci_tyvars = tvs' 
	   , tci_given  = map (tidyInst env') (tci_given  implic)
	   , tci_wanted = map (tidyInst env') (tci_wanted implic) }
  where
    (env', tvs') = mapAccumL tidyTyVarBndr env (tci_tyvars implic)

tidyMoreInsts :: TidyEnv -> [Inst] -> (TidyEnv, [Inst])
-- This function doesn't assume that the tyvars are in scope
-- so it works like tidyOpenType, returning a TidyEnv
tidyMoreInsts env insts
  = (env', map (tidyInst env') insts)
  where
    env' = tidyFreeTyVars env (tyVarsOfInsts insts)

tidyInsts :: [Inst] -> (TidyEnv, [Inst])
tidyInsts insts = tidyMoreInsts emptyTidyEnv insts

showLIE :: SDoc -> TcM ()	-- Debugging
showLIE str
  = do { lie_var <- getLIEVar ;
	 lie <- readMutVar lie_var ;
	 traceTc (str <+> vcat (map pprInstInFull (lieToList lie))) }
\end{code}


%************************************************************************
%*									*
	Extending the instance environment
%*									*
%************************************************************************

\begin{code}
tcExtendLocalInstEnv :: [Instance] -> TcM a -> TcM a
  -- Add new locally-defined instances
tcExtendLocalInstEnv dfuns thing_inside
 = do { traceDFuns dfuns
      ; env <- getGblEnv
      ; inst_env' <- foldlM addLocalInst (tcg_inst_env env) dfuns
      ; let env' = env { tcg_insts = dfuns ++ tcg_insts env,
			 tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

addLocalInst :: InstEnv -> Instance -> TcM InstEnv
-- Check that the proposed new instance is OK, 
-- and then add it to the home inst env
addLocalInst home_ie ispec
  = do	{ 	-- Instantiate the dfun type so that we extend the instance
		-- envt with completely fresh template variables
		-- This is important because the template variables must
		-- not overlap with anything in the things being looked up
		-- (since we do unification).  
		-- We use tcInstSkolType because we don't want to allocate fresh
		--  *meta* type variables.  
	  let dfun = instanceDFunId ispec
	; (tvs', theta', tau') <- tcInstSkolType InstSkol (idType dfun)
	; let	(cls, tys') = tcSplitDFunHead tau'
		dfun' 	    = setIdType dfun (mkSigmaTy tvs' theta' tau')	    
	  	ispec'      = setInstanceDFunId ispec dfun'

		-- Load imported instances, so that we report
		-- duplicates correctly
	; eps <- getEps
	; let inst_envs = (eps_inst_env eps, home_ie)

		-- Check functional dependencies
	; case checkFunDeps inst_envs ispec' of
		Just specs -> funDepErr ispec' specs
		Nothing    -> return ()

		-- Check for duplicate instance decls
	; let { (matches, _) = lookupInstEnv inst_envs cls tys'
	      ;	dup_ispecs = [ dup_ispec 
			     | (dup_ispec, _) <- matches
			     , let (_,_,_,dup_tys) = instanceHead dup_ispec
			     , isJust (tcMatchTys (mkVarSet tvs') tys' dup_tys)] }
		-- Find memebers of the match list which ispec itself matches.
		-- If the match is 2-way, it's a duplicate
	; case dup_ispecs of
	    dup_ispec : _ -> dupInstErr ispec' dup_ispec
	    []            -> return ()

		-- OK, now extend the envt
	; return (extendInstEnv home_ie ispec') }

getOverlapFlag :: TcM OverlapFlag
getOverlapFlag 
  = do 	{ dflags <- getDOpts
	; let overlap_ok    = dopt Opt_OverlappingInstances dflags
	      incoherent_ok = dopt Opt_IncoherentInstances  dflags
	      overlap_flag | incoherent_ok = Incoherent
			   | overlap_ok    = OverlapOk
			   | otherwise     = NoOverlap
			   
	; return overlap_flag }

traceDFuns ispecs
  = traceTc (hang (text "Adding instances:") 2 (vcat (map pp ispecs)))
  where
    pp ispec = ppr (instanceDFunId ispec) <+> colon <+> ppr ispec
	-- Print the dfun name itself too

funDepErr ispec ispecs
  = addDictLoc ispec $
    addErr (hang (ptext SLIT("Functional dependencies conflict between instance declarations:"))
	       2 (pprInstances (ispec:ispecs)))
dupInstErr ispec dup_ispec
  = addDictLoc ispec $
    addErr (hang (ptext SLIT("Duplicate instance declarations:"))
	       2 (pprInstances [ispec, dup_ispec]))

addDictLoc ispec thing_inside
  = setSrcSpan (mkSrcSpan loc loc) thing_inside
  where
   loc = getSrcLoc ispec
\end{code}
    

%************************************************************************
%*									*
\subsection{Looking up Insts}
%*									*
%************************************************************************

\begin{code}
data LookupInstResult
  = NoInstance
  | GenInst [Inst] (LHsExpr TcId)	-- The expression and its needed insts

lookupSimpleInst :: Inst -> TcM LookupInstResult
-- This is "simple" in that it returns NoInstance for implication constraints

-- It's important that lookupInst does not put any new stuff into
-- the LIE.  Instead, any Insts needed by the lookup are returned in
-- the LookupInstResult, where they can be further processed by tcSimplify

lookupSimpleInst (EqInst {}) = return NoInstance

--------------------- Implications ------------------------
lookupSimpleInst (ImplicInst {}) = return NoInstance

--------------------- Methods ------------------------
lookupSimpleInst (Method {tci_oid = id, tci_tys = tys, tci_theta = theta, tci_loc = loc})
  = do	{ (dict_app, dicts) <- getLIE $ instCallDicts loc theta
	; let co_fn = dict_app <.> mkWpTyApps tys
	; return (GenInst dicts (L span $ HsWrap co_fn (HsVar id))) }
  where
    span = instLocSpan loc

--------------------- Literals ------------------------
-- Look for short cuts first: if the literal is *definitely* a 
-- int, integer, float or a double, generate the real thing here.
-- This is essential (see nofib/spectral/nucleic).
-- [Same shortcut as in newOverloadedLit, but we
--  may have done some unification by now] 		

lookupSimpleInst (LitInst {tci_lit = HsIntegral i from_integer_name _, tci_ty = ty, tci_loc = loc})
  | Just expr <- shortCutIntLit i ty
  = returnM (GenInst [] (noLoc expr))
  | otherwise
  = ASSERT( from_integer_name `isHsVar` fromIntegerName )	-- A LitInst invariant
    tcLookupId fromIntegerName			`thenM` \ from_integer ->
    tcInstClassOp loc from_integer [ty]		`thenM` \ method_inst ->
    mkIntegerLit i				`thenM` \ integer_lit ->
    returnM (GenInst [method_inst]
		     (mkHsApp (L (instLocSpan loc)
			   	 (HsVar (instToId method_inst))) integer_lit))

lookupSimpleInst (LitInst {tci_lit = HsFractional f from_rat_name _, tci_ty = ty, tci_loc = loc})
  | Just expr <- shortCutFracLit f ty
  = returnM (GenInst [] (noLoc expr))

  | otherwise
  = ASSERT( from_rat_name `isHsVar` fromRationalName )	-- A LitInst invariant
    tcLookupId fromRationalName			`thenM` \ from_rational ->
    tcInstClassOp loc from_rational [ty]	`thenM` \ method_inst ->
    mkRatLit f					`thenM` \ rat_lit ->
    returnM (GenInst [method_inst] (mkHsApp (L (instLocSpan loc) 
					       (HsVar (instToId method_inst))) rat_lit))

lookupSimpleInst (LitInst {tci_lit = HsIsString s from_string_name _, tci_ty = ty, tci_loc = loc})
  | Just expr <- shortCutStringLit s ty
  = returnM (GenInst [] (noLoc expr))
  | otherwise
  = ASSERT( from_string_name `isHsVar` fromStringName )	-- A LitInst invariant
    tcLookupId fromStringName			`thenM` \ from_string ->
    tcInstClassOp loc from_string [ty]		`thenM` \ method_inst ->
    mkStrLit s					`thenM` \ string_lit ->
    returnM (GenInst [method_inst]
		     (mkHsApp (L (instLocSpan loc)
			   	 (HsVar (instToId method_inst))) string_lit))

--------------------- Dictionaries ------------------------
lookupSimpleInst (Dict {tci_pred = pred, tci_loc = loc})
  = do 	{ mb_result <- lookupPred pred
	; case mb_result of {
	    Nothing -> return NoInstance ;
	    Just (dfun_id, mb_inst_tys) -> do

    { use_stage <- getStage
    ; checkWellStaged (ptext SLIT("instance for") <+> quotes (ppr pred))
    		      (topIdLvl dfun_id) use_stage

 	-- It's possible that not all the tyvars are in
	-- the substitution, tenv. For example:
	--	instance C X a => D X where ...
	-- (presumably there's a functional dependency in class C)
	-- Hence mb_inst_tys :: Either TyVar TcType 

    ; let inst_tv (Left tv)  = do { tv' <- tcInstTyVar tv; return (mkTyVarTy tv') }
	  inst_tv (Right ty) = return ty
    ; tys <- mappM inst_tv mb_inst_tys
    ; let
    	(theta, _) = tcSplitPhiTy (applyTys (idType dfun_id) tys)
	src_loc	   = instLocSpan loc
	dfun	   = HsVar dfun_id
    ; if null theta then
    	returnM (GenInst [] (L src_loc $ HsWrap (mkWpTyApps tys) dfun))
      else do
    { (dict_app, dicts) <- getLIE $ instCallDicts loc theta -- !!!
    ; let co_fn = dict_app <.> mkWpTyApps tys
    ; returnM (GenInst dicts (L src_loc $ HsWrap co_fn dfun))
    }}}}

---------------
lookupPred :: TcPredType -> TcM (Maybe (DFunId, [Either TyVar TcType]))
-- Look up a class constraint in the instance environment
lookupPred pred@(ClassP clas tys)
  = do	{ eps     <- getEps
	; tcg_env <- getGblEnv
	; let inst_envs = (eps_inst_env eps, tcg_inst_env tcg_env)
	; case lookupInstEnv inst_envs clas tys of {
	    ([(ispec, inst_tys)], []) 
		-> do	{ let dfun_id = is_dfun ispec
			; traceTc (text "lookupInst success" <+> 
				   vcat [text "dict" <+> ppr pred, 
				         text "witness" <+> ppr dfun_id
					 <+> ppr (idType dfun_id) ])
				-- Record that this dfun is needed
			; record_dfun_usage dfun_id
			; return (Just (dfun_id, inst_tys)) } ;

     	    (matches, unifs)
		-> do	{ traceTc (text "lookupInst fail" <+> 
				   vcat [text "dict" <+> ppr pred,
				   	 text "matches" <+> ppr matches,
				   	 text "unifs" <+> ppr unifs])
		-- In the case of overlap (multiple matches) we report
		-- NoInstance here.  That has the effect of making the 
		-- context-simplifier return the dict as an irreducible one.
		-- Then it'll be given to addNoInstanceErrs, which will do another
		-- lookupInstEnv to get the detailed info about what went wrong.
			; return Nothing }
	}}

lookupPred ip_pred = return Nothing	-- Implicit parameters

record_dfun_usage dfun_id 
  = do	{ hsc_env <- getTopEnv
	; let  dfun_name = idName dfun_id
	       dfun_mod  = nameModule dfun_name
	; if isInternalName dfun_name ||    -- Internal name => defined in this module
	     modulePackageId dfun_mod /= thisPackage (hsc_dflags hsc_env)
	  then return () -- internal, or in another package
	   else do { tcg_env <- getGblEnv
	  	   ; updMutVar (tcg_inst_uses tcg_env)
			       (`addOneToNameSet` idName dfun_id) }}


tcGetInstEnvs :: TcM (InstEnv, InstEnv)
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetInstEnvs = do { eps <- getEps; env <- getGblEnv;
		     return (eps_inst_env eps, tcg_inst_env env) }
\end{code}



%************************************************************************
%*									*
		Re-mappable syntax
%*									*
%************************************************************************

Suppose we are doing the -fno-implicit-prelude thing, and we encounter
a do-expression.  We have to find (>>) in the current environment, which is
done by the rename. Then we have to check that it has the same type as
Control.Monad.(>>).  Or, more precisely, a compatible type. One 'customer' had
this:

  (>>) :: HB m n mn => m a -> n b -> mn b

So the idea is to generate a local binding for (>>), thus:

	let then72 :: forall a b. m a -> m b -> m b
	    then72 = ...something involving the user's (>>)...
	in
	...the do-expression...

Now the do-expression can proceed using then72, which has exactly
the expected type.

In fact tcSyntaxName just generates the RHS for then72, because we only
want an actual binding in the do-expression case. For literals, we can 
just use the expression inline.

\begin{code}
tcSyntaxName :: InstOrigin
	     -> TcType			-- Type to instantiate it at
	     -> (Name, HsExpr Name)	-- (Standard name, user name)
	     -> TcM (Name, HsExpr TcId)	-- (Standard name, suitable expression)
--	*** NOW USED ONLY FOR CmdTop (sigh) ***
-- NB: tcSyntaxName calls tcExpr, and hence can do unification.
-- So we do not call it from lookupInst, which is called from tcSimplify

tcSyntaxName orig ty (std_nm, HsVar user_nm)
  | std_nm == user_nm
  = newMethodFromName orig ty std_nm	`thenM` \ id ->
    returnM (std_nm, HsVar id)

tcSyntaxName orig ty (std_nm, user_nm_expr)
  = tcLookupId std_nm		`thenM` \ std_id ->
    let	
	-- C.f. newMethodAtLoc
	([tv], _, tau)  = tcSplitSigmaTy (idType std_id)
 	sigma1		= substTyWith [tv] [ty] tau
	-- Actually, the "tau-type" might be a sigma-type in the
	-- case of locally-polymorphic methods.
    in
    addErrCtxtM (syntaxNameCtxt user_nm_expr orig sigma1)	$

	-- Check that the user-supplied thing has the
	-- same type as the standard one.  
	-- Tiresome jiggling because tcCheckSigma takes a located expression
    getSrcSpanM					`thenM` \ span -> 
    tcPolyExpr (L span user_nm_expr) sigma1	`thenM` \ expr ->
    returnM (std_nm, unLoc expr)

syntaxNameCtxt name orig ty tidy_env
  = getInstLoc orig		`thenM` \ inst_loc ->
    let
	msg = vcat [ptext SLIT("When checking that") <+> quotes (ppr name) <+> 
				ptext SLIT("(needed by a syntactic construct)"),
		    nest 2 (ptext SLIT("has the required type:") <+> ppr (tidyType tidy_env ty)),
		    nest 2 (ptext SLIT("arising from") <+> pprInstLoc inst_loc)]
    in
    returnM (tidy_env, msg)
\end{code}

%************************************************************************
%*									*
		EqInsts
%*									*
%************************************************************************

\begin{code}
mkGivenCo   :: Coercion -> Either TcTyVar Coercion
mkGivenCo   =  Right

mkWantedCo  :: TcTyVar  -> Either TcTyVar Coercion
mkWantedCo  =  Left

fromGivenCo :: Either TcTyVar Coercion -> Coercion
fromGivenCo (Right co) 	 = co
fromGivenCo _		 = panic "fromGivenCo: not a wanted coercion"

fromWantedCo :: String -> Either TcTyVar Coercion -> TcTyVar
fromWantedCo _ (Left covar) = covar
fromWantedCo msg _	    = panic ("fromWantedCo: not a wanted coercion: " ++ msg)

eitherEqInst :: Inst 	            -- given or wanted EqInst
	     -> (TcTyVar  -> a)     -- 	result if wanted
	     -> (Coercion -> a)     --	result if given
	     -> a		
eitherEqInst (EqInst {tci_co = either_co}) withWanted withGiven
	= case either_co of
		Left  covar -> withWanted covar
		Right co    -> withGiven  co

mkEqInsts :: [PredType] -> [Either TcTyVar Coercion] -> TcM [Inst]
mkEqInsts preds cos = zipWithM mkEqInst preds cos

mkEqInst :: PredType -> Either TcTyVar Coercion -> TcM Inst
mkEqInst (EqPred ty1 ty2) co
	= do { uniq <- newUnique
	     ; src_span <- getSrcSpanM
	     ; err_ctxt <- getErrCtxt
	     ; let loc  = InstLoc EqOrigin src_span err_ctxt
	           name = mkName uniq src_span
	           inst = EqInst {tci_left = ty1, tci_right = ty2, tci_co = co, tci_loc = loc, tci_name = name} 
	     ; return inst
	     }
	where mkName uniq src_span = mkInternalName uniq (mkVarOcc "co") src_span

mkWantedEqInst :: PredType -> TcM Inst
mkWantedEqInst pred@(EqPred ty1 ty2)
  = do { cotv <- newMetaCoVar ty1 ty2
       ; mkEqInst pred (Left cotv)
       }

-- type inference:
--	We want to promote the wanted EqInst to a given EqInst
--	in the signature context.
--	This means we have to give the coercion a name
--	and fill it in as its own name.
finalizeEqInst 
	:: Inst			-- wanted
	-> TcM Inst		-- given
finalizeEqInst wanted@(EqInst {tci_left = ty1, tci_right = ty2, tci_name = name})
  	= do { let var = Var.mkCoVar name (PredTy $ EqPred ty1 ty2)
       	     ; writeWantedCoercion wanted (TyVarTy var)
	     ; let given = wanted { tci_co = mkGivenCo $ TyVarTy var }
	     ; return given
             }

writeWantedCoercion 
	:: Inst 	-- wanted EqInst
	-> Coercion 	-- coercion to fill the hole with
	-> TcM ()	
writeWantedCoercion wanted co
  	= do { let cotv = fromWantedCo "writeWantedCoercion" $ tci_co wanted
	     ; writeMetaTyVar cotv co
	     }

eqInstType :: Inst -> TcType
eqInstType inst = eitherEqInst inst mkTyVarTy id

eqInstCoercion :: Inst -> Either TcTyVar Coercion
eqInstCoercion = tci_co

eqInstLeftTy, eqInstRightTy :: Inst -> TcType
eqInstLeftTy  = tci_left
eqInstRightTy = tci_right

updateEqInstCoercion :: (Either TcTyVar Coercion -> Either TcTyVar Coercion) -> Inst -> Inst
updateEqInstCoercion f inst = inst {tci_co = f $ tci_co inst}
\end{code}
