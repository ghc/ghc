%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
module Inst ( 
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, zonkLIE,
	plusLIEs, mkLIE, isEmptyLIE,

	Inst, OverloadedLit(..),
	pprInst, pprInsts, pprInstsInFull, tidyInst, tidyInsts,

        InstanceMapper,

	newDictFromOld, newDicts, newDictsAtLoc, 
	newMethod, newMethodWithGivenTy, newOverloadedLit, instOverloadedFun,

	tyVarsOfInst, instLoc, getDictClassTys,

	lookupInst, lookupSimpleInst, LookupInstResult(..),

	isDict, isTyVarDict, isStdClassTyVarDict, isMethodFor,
	instBindingRequired, instCanBeGeneralised,

	zonkInst, instToId, instToIdBndr,

	InstOrigin(..), InstLoc, pprInstLoc
    ) where

#include "HsVersions.h"

import HsSyn	( HsLit(..), HsExpr(..) )
import RnHsSyn	( RenamedArithSeqInfo, RenamedHsExpr, RenamedPat )
import TcHsSyn	( TcExpr, TcId, 
		  mkHsTyApp, mkHsDictApp, zonkId
		)
import TcMonad
import TcEnv	( TcIdSet, tcLookupValueByKey, tcLookupTyConByKey )
import TcType	( TcThetaType,
		  TcType, TcTauType, TcTyVarSet,
		  zonkTcType, zonkTcTypes, 
		  zonkTcThetaType
		)
import Bag
import Class	( classInstEnv, Class )
import Id	( Id, idFreeTyVars, idType, mkUserLocal, mkSysLocal )
import VarSet	( elemVarSet )
import PrelInfo	( isStandardClass, isCcallishClass, isNoDictClass )
import Name	( OccName, Name, mkDictOcc, mkMethodOcc, getOccName )
import PprType	( pprConstraint )	
import InstEnv	( InstEnv, lookupInstEnv )
import SrcLoc	( SrcLoc )
import Type	( Type, ThetaType,
		  mkTyVarTy, isTyVarTy, mkDictTy, splitForAllTys, splitSigmaTy,
		  splitRhoTy, tyVarsOfType, tyVarsOfTypes,
		  mkSynTy, tidyOpenType, tidyOpenTypes
		)
import InstEnv	( InstEnv )
import Subst	( emptyInScopeSet, mkSubst,
		  substTy, substTheta, mkTyVarSubst, mkTopTyVarSubst
		)
import TyCon	( TyCon )
import VarEnv	( lookupVarEnv, TidyEnv,
		  lookupSubstEnv, SubstResult(..)
		)
import VarSet	( unionVarSet )
import TysPrim	  ( intPrimTy, floatPrimTy, doublePrimTy )
import TysWiredIn ( intDataCon, isIntTy, inIntRange,
		    floatDataCon, isFloatTy,
		    doubleDataCon, isDoubleTy,
		    integerTy, isIntegerTy
		  ) 
import Unique	( fromRationalClassOpKey, rationalTyConKey,
		  fromIntClassOpKey, fromIntegerClassOpKey, Unique
		)
import Maybes	( expectJust )
import Util	( thenCmp, zipWithEqual, mapAccumL )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Inst-collections]{LIE: a collection of Insts}
%*									*
%************************************************************************

\begin{code}
type LIE = Bag Inst

isEmptyLIE	  = isEmptyBag
emptyLIE          = emptyBag
unitLIE inst 	  = unitBag inst
mkLIE insts	  = listToBag insts
plusLIE lie1 lie2 = lie1 `unionBags` lie2
consLIE inst lie  = inst `consBag` lie
plusLIEs lies	  = unionManyBags lies

zonkLIE :: LIE -> NF_TcM s LIE
zonkLIE lie = mapBagNF_Tc zonkInst lie

pprInsts :: [Inst] -> SDoc
pprInsts insts = parens (sep (punctuate comma (map pprInst insts)))


pprInstsInFull insts
  = vcat (map go insts)
  where
    go inst = quotes (ppr inst) <+> pprInstLoc (instLoc inst)
\end{code}

%************************************************************************
%*									*
\subsection[Inst-types]{@Inst@ types}
%*									*
%************************************************************************

An @Inst@ is either a dictionary, an instance of an overloaded
literal, or an instance of an overloaded value.  We call the latter a
``method'' even though it may not correspond to a class operation.
For example, we might have an instance of the @double@ function at
type Int, represented by

	Method 34 doubleId [Int] origin

\begin{code}
data Inst
  = Dict
	Unique
	Class		-- The type of the dict is (c ts), where
	[TcType]	-- c is the class and ts the types;
	InstLoc

  | Method
	Unique

	TcId	-- The overloaded function
			-- This function will be a global, local, or ClassOpId;
			--   inside instance decls (only) it can also be an InstId!
			-- The id needn't be completely polymorphic.
			-- You'll probably find its name (for documentation purposes)
			--	  inside the InstOrigin

	[TcType]	-- The types to which its polymorphic tyvars
			--	should be instantiated.
			-- These types must saturate the Id's foralls.

	TcThetaType	-- The (types of the) dictionaries to which the function
			-- must be applied to get the method

	TcTauType	-- The type of the method

	InstLoc

	-- INVARIANT: in (Method u f tys theta tau loc)
	--	type of (f tys dicts(from theta)) = tau

  | LitInst
	Unique
	OverloadedLit
	TcType		-- The type at which the literal is used
	InstLoc

data OverloadedLit
  = OverloadedIntegral	 Integer	-- The number
  | OverloadedFractional Rational	-- The number
\end{code}

Ordering
~~~~~~~~
@Insts@ are ordered by their class/type info, rather than by their
unique.  This allows the context-reduction mechanism to use standard finite
maps to do their stuff.

\begin{code}
instance Ord Inst where
  compare = cmpInst

instance Eq Inst where
  (==) i1 i2 = case i1 `cmpInst` i2 of
	         EQ    -> True
		 other -> False

cmpInst  (Dict _ clas1 tys1 _) (Dict _ clas2 tys2 _)
  = (clas1 `compare` clas2) `thenCmp` (tys1 `compare` tys2)
cmpInst (Dict _ _ _ _) other
  = LT


cmpInst (Method _ _ _ _ _ _) (Dict _ _ _ _)
  = GT
cmpInst (Method _ id1 tys1 _ _ _) (Method _ id2 tys2 _ _ _)
  = (id1 `compare` id2) `thenCmp` (tys1 `compare` tys2)
cmpInst (Method _ _ _ _ _ _) other
  = LT

cmpInst (LitInst _ lit1 ty1 _) (LitInst _ lit2 ty2 _)
  = (lit1 `cmpOverLit` lit2) `thenCmp` (ty1 `compare` ty2)
cmpInst (LitInst _ _ _ _) other
  = GT

cmpOverLit (OverloadedIntegral   i1) (OverloadedIntegral   i2) = i1 `compare` i2
cmpOverLit (OverloadedFractional f1) (OverloadedFractional f2) = f1 `compare` f2
cmpOverLit (OverloadedIntegral _)    (OverloadedFractional _)  = LT
cmpOverLit (OverloadedFractional _)  (OverloadedIntegral _)    = GT
\end{code}


Selection
~~~~~~~~~
\begin{code}
instLoc (Dict   u clas tys  loc) = loc
instLoc (Method u _ _ _ _   loc) = loc
instLoc (LitInst u lit ty   loc) = loc

getDictClassTys (Dict u clas tys _) = (clas, tys)

tyVarsOfInst :: Inst -> TcTyVarSet
tyVarsOfInst (Dict _ _ tys _)        = tyVarsOfTypes  tys
tyVarsOfInst (Method _ id tys _ _ _) = tyVarsOfTypes tys `unionVarSet` idFreeTyVars id
					 -- The id might have free type variables; in the case of
					 -- locally-overloaded class methods, for example
tyVarsOfInst (LitInst _ _ ty _)      = tyVarsOfType  ty
\end{code}

Predicates
~~~~~~~~~~
\begin{code}
isDict :: Inst -> Bool
isDict (Dict _ _ _ _) = True
isDict other	      = False

isMethodFor :: TcIdSet -> Inst -> Bool
isMethodFor ids (Method uniq id tys _ _ loc) 
  = id `elemVarSet` ids
isMethodFor ids inst 
  = False

isTyVarDict :: Inst -> Bool
isTyVarDict (Dict _ _ tys _) = all isTyVarTy tys
isTyVarDict other 	     = False

isStdClassTyVarDict (Dict _ clas [ty] _) = isStandardClass clas && isTyVarTy ty
isStdClassTyVarDict other		 = False
\end{code}

Two predicates which deal with the case where class constraints don't
necessarily result in bindings.  The first tells whether an @Inst@
must be witnessed by an actual binding; the second tells whether an
@Inst@ can be generalised over.

\begin{code}
instBindingRequired :: Inst -> Bool
instBindingRequired (Dict _ clas _ _) = not (isNoDictClass clas)
instBindingRequired other	      = True

instCanBeGeneralised :: Inst -> Bool
instCanBeGeneralised (Dict _ clas _ _) = not (isCcallishClass clas)
instCanBeGeneralised other	       = True
\end{code}


Construction
~~~~~~~~~~~~

\begin{code}
newDicts :: InstOrigin
	 -> TcThetaType
	 -> NF_TcM s (LIE, [TcId])
newDicts orig theta
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    newDictsAtLoc loc theta	`thenNF_Tc` \ (dicts, ids) ->
    returnNF_Tc (listToBag dicts, ids)

-- Local function, similar to newDicts, 
-- but with slightly different interface
newDictsAtLoc :: InstLoc
 	      -> TcThetaType
	      -> NF_TcM s ([Inst], [TcId])
newDictsAtLoc loc theta =
 tcGetUniques (length theta)		`thenNF_Tc` \ new_uniqs ->
 let
  mk_dict u (clas, tys) = Dict u clas tys loc
  dicts = zipWithEqual "newDictsAtLoc" mk_dict new_uniqs theta
 in
 returnNF_Tc (dicts, map instToId dicts)

newDictFromOld :: Inst -> Class -> [TcType] -> NF_TcM s Inst
newDictFromOld (Dict _ _ _ loc) clas tys
  = tcGetUnique	      `thenNF_Tc` \ uniq ->
    returnNF_Tc (Dict uniq clas tys loc)


newMethod :: InstOrigin
	  -> TcId
	  -> [TcType]
	  -> NF_TcM s (LIE, TcId)
newMethod orig id tys
  =   	-- Get the Id type and instantiate it at the specified types
    let
	(tyvars, rho) = splitForAllTys (idType id)
	rho_ty	      = substTy (mkTyVarSubst tyvars tys) rho
	(theta, tau)  = splitRhoTy rho_ty
    in
    newMethodWithGivenTy orig id tys theta tau	`thenNF_Tc` \ meth_inst ->
    returnNF_Tc (unitLIE meth_inst, instToId meth_inst)

instOverloadedFun orig (HsVar v) arg_tys theta tau
  = newMethodWithGivenTy orig v arg_tys theta tau	`thenNF_Tc` \ inst ->
    returnNF_Tc (HsVar (instToId inst), unitLIE inst)

newMethodWithGivenTy orig id tys theta tau
  = tcGetInstLoc orig	`thenNF_Tc` \ loc ->
    tcGetUnique		`thenNF_Tc` \ new_uniq ->
    let
	meth_inst = Method new_uniq id tys theta tau loc
    in
    returnNF_Tc meth_inst

newMethodAtLoc :: InstLoc
	       -> Id -> [TcType]
	       -> NF_TcM s (Inst, TcId)
newMethodAtLoc loc real_id tys		-- Local function, similar to newMethod but with 
					-- slightly different interface
  =   	-- Get the Id type and instantiate it at the specified types
    tcGetUnique					`thenNF_Tc` \ new_uniq ->
    let
	(tyvars,rho) = splitForAllTys (idType real_id)
	rho_ty	      = ASSERT( length tyvars == length tys )
			substTy (mkTopTyVarSubst tyvars tys) rho
	(theta, tau)  = splitRhoTy rho_ty
	meth_inst     = Method new_uniq real_id tys theta tau loc
    in
    returnNF_Tc (meth_inst, instToId meth_inst)
\end{code}

In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

\begin{code}
newOverloadedLit :: InstOrigin
		 -> OverloadedLit
		 -> TcType
		 -> NF_TcM s (TcExpr, LIE)
newOverloadedLit orig (OverloadedIntegral i) ty
  | isIntTy ty && inIntRange i		-- Short cut for Int
  = returnNF_Tc (int_lit, emptyLIE)

  | isIntegerTy ty 			-- Short cut for Integer
  = returnNF_Tc (integer_lit, emptyLIE)

  where
    intprim_lit    = HsLitOut (HsIntPrim i) intPrimTy
    integer_lit    = HsLitOut (HsInt i) integerTy
    int_lit        = HsCon intDataCon [] [intprim_lit]

newOverloadedLit orig lit ty		-- The general case
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    tcGetUnique			`thenNF_Tc` \ new_uniq ->
    let
	lit_inst = LitInst new_uniq lit ty loc
    in
    returnNF_Tc (HsVar (instToId lit_inst), unitLIE lit_inst)
\end{code}


\begin{code}
instToId :: Inst -> TcId
instToId inst = instToIdBndr inst

instToIdBndr :: Inst -> TcId
instToIdBndr (Dict u clas ty (_,loc,_))
  = mkUserLocal (mkDictOcc (getOccName clas)) u (mkDictTy clas ty) loc

instToIdBndr (Method u id tys theta tau (_,loc,_))
  = mkUserLocal (mkMethodOcc (getOccName id)) u tau loc
    
instToIdBndr (LitInst u list ty loc)
  = mkSysLocal SLIT("lit") u ty
\end{code}


Zonking
~~~~~~~
Zonking makes sure that the instance types are fully zonked,
but doesn't do the same for the Id in a Method.  There's no
need, and it's a lot of extra work.

\begin{code}
zonkInst :: Inst -> NF_TcM s Inst
zonkInst (Dict u clas tys loc)
  = zonkTcTypes	tys			`thenNF_Tc` \ new_tys ->
    returnNF_Tc (Dict u clas new_tys loc)

zonkInst (Method u id tys theta tau loc) 
  = zonkId id			`thenNF_Tc` \ new_id ->
	-- Essential to zonk the id in case it's a local variable
	-- Can't use zonkIdOcc because the id might itself be
	-- an InstId, in which case it won't be in scope

    zonkTcTypes tys		`thenNF_Tc` \ new_tys ->
    zonkTcThetaType theta	`thenNF_Tc` \ new_theta ->
    zonkTcType tau		`thenNF_Tc` \ new_tau ->
    returnNF_Tc (Method u new_id new_tys new_theta new_tau loc)

zonkInst (LitInst u lit ty loc)
  = zonkTcType ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (LitInst u lit new_ty loc)
\end{code}


Printing
~~~~~~~~
ToDo: improve these pretty-printing things.  The ``origin'' is really only
relevant in error messages.

\begin{code}
instance Outputable Inst where
    ppr inst = pprInst inst

pprInst (LitInst u lit ty loc)
  = hsep [case lit of
	      OverloadedIntegral   i -> integer i
	      OverloadedFractional f -> rational f,
	   ptext SLIT("at"),
	   ppr ty,
	   show_uniq u]

pprInst (Dict u clas tys loc) = pprConstraint clas tys <+> show_uniq u

pprInst (Method u id tys _ _ loc)
  = hsep [ppr id, ptext SLIT("at"), 
	  brackets (interppSP tys),
	  show_uniq u]

tidyInst :: TidyEnv -> Inst -> (TidyEnv, Inst)
tidyInst env (LitInst u lit ty loc)
  = (env', LitInst u lit ty' loc)
  where
    (env', ty') = tidyOpenType env ty

tidyInst env (Dict u clas tys loc)
  = (env', Dict u clas tys' loc)
  where
    (env', tys') = tidyOpenTypes env tys

tidyInst env (Method u id tys theta tau loc)
  = (env', Method u id tys' theta tau loc)
		-- Leave theta, tau alone cos we don't print them
  where
    (env', tys') = tidyOpenTypes env tys
    
tidyInsts env insts = mapAccumL tidyInst env insts

show_uniq u = ifPprDebug (text "{-" <> ppr u <> text "-}")
\end{code}


%************************************************************************
%*									*
\subsection[InstEnv-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InstanceMapper = Class -> InstEnv
\end{code}

A @ClassInstEnv@ lives inside a class, and identifies all the instances
of that class.  The @Id@ inside a ClassInstEnv mapping is the dfun for
that instance.  

There is an important consistency constraint between the @MatchEnv@s
in and the dfun @Id@s inside them: the free type variables of the
@Type@ key in the @MatchEnv@ must be a subset of the universally-quantified
type variables of the dfun.  Thus, the @ClassInstEnv@ for @Eq@ might
contain the following entry:
@
	[a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
@
The "a" in the pattern must be one of the forall'd variables in
the dfun type.

\begin{code}
data LookupInstResult s
  = NoInstance
  | SimpleInst TcExpr		-- Just a variable, type application, or literal
  | GenInst    [Inst] TcExpr	-- The expression and its needed insts

lookupInst :: Inst 
	   -> NF_TcM s (LookupInstResult s)

-- Dictionaries

lookupInst dict@(Dict _ clas tys loc)
  = case lookupInstEnv (ppr clas) (classInstEnv clas) tys of

      Just (tenv, dfun_id)
	-> let
		subst	      = mkSubst (tyVarsOfTypes tys) tenv
		(tyvars, rho) = splitForAllTys (idType dfun_id)
		ty_args	      = map subst_tv tyvars
		dfun_rho      = substTy subst rho
		(theta, tau)  = splitRhoTy dfun_rho
		ty_app        = mkHsTyApp (HsVar dfun_id) ty_args
		subst_tv tv   = case lookupSubstEnv tenv tv of
				   Just (DoneTy ty)  -> ty
					-- tenv should bind all the tyvars
	   in
	   if null theta then
		returnNF_Tc (SimpleInst ty_app)
	   else
	   newDictsAtLoc loc theta	`thenNF_Tc` \ (dicts, dict_ids) ->
	   let 
		rhs = mkHsDictApp ty_app dict_ids
	   in
	   returnNF_Tc (GenInst dicts rhs)
			     
      Nothing	-> returnNF_Tc NoInstance

-- Methods

lookupInst inst@(Method _ id tys theta _ loc)
  = newDictsAtLoc loc theta		`thenNF_Tc` \ (dicts, dict_ids) ->
    returnNF_Tc (GenInst dicts (mkHsDictApp (mkHsTyApp (HsVar id) tys) dict_ids))

-- Literals

lookupInst inst@(LitInst u (OverloadedIntegral i) ty loc)
  | isIntTy ty && in_int_range			-- Short cut for Int
  = returnNF_Tc (GenInst [] int_lit)
	-- GenInst, not SimpleInst, because int_lit is actually a constructor application

  | isIntegerTy ty				-- Short cut for Integer
  = returnNF_Tc (GenInst [] integer_lit)

  | in_int_range				-- It's overloaded but small enough to fit into an Int
  = tcLookupValueByKey fromIntClassOpKey	`thenNF_Tc` \ from_int ->
    newMethodAtLoc loc from_int [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) int_lit))

  | otherwise   				-- Alas, it is overloaded and a big literal!
  = tcLookupValueByKey fromIntegerClassOpKey	`thenNF_Tc` \ from_integer ->
    newMethodAtLoc loc from_integer [ty]	`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) integer_lit))
  where
    in_int_range   = inIntRange i
    intprim_lit    = HsLitOut (HsIntPrim i) intPrimTy
    integer_lit    = HsLitOut (HsInt i) integerTy
    int_lit        = HsCon intDataCon [] [intprim_lit]

-- similar idea for overloaded floating point literals: if the literal is
-- *definitely* a float or a double, generate the real thing here.
-- This is essential  (see nofib/spectral/nucleic).

lookupInst inst@(LitInst u (OverloadedFractional f) ty loc)
  | isFloatTy ty    = returnNF_Tc (GenInst [] float_lit)
  | isDoubleTy ty   = returnNF_Tc (GenInst [] double_lit)

  | otherwise 
	  = tcLookupValueByKey fromRationalClassOpKey	`thenNF_Tc` \ from_rational ->

	-- The type Rational isn't wired in so we have to conjure it up
    tcLookupTyConByKey rationalTyConKey	`thenNF_Tc` \ rational_tycon ->
    let
	rational_ty  = mkSynTy rational_tycon []
	rational_lit = HsLitOut (HsFrac f) rational_ty
    in
    newMethodAtLoc loc from_rational [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) rational_lit))

  where
    floatprim_lit  = HsLitOut (HsFloatPrim f) floatPrimTy
    float_lit      = HsCon floatDataCon [] [floatprim_lit]
    doubleprim_lit = HsLitOut (HsDoublePrim f) doublePrimTy
    double_lit     = HsCon doubleDataCon [] [doubleprim_lit]

\end{code}

There is a second, simpler interface, when you want an instance of a
class at a given nullary type constructor.  It just returns the
appropriate dictionary if it exists.  It is used only when resolving
ambiguous dictionaries.

\begin{code}
lookupSimpleInst :: InstEnv
		 -> Class
		 -> [Type]			-- Look up (c,t)
	         -> NF_TcM s (Maybe ThetaType)		-- Here are the needed (c,t)s

lookupSimpleInst class_inst_env clas tys
  = case lookupInstEnv (ppr clas) class_inst_env tys of
      Nothing	 -> returnNF_Tc Nothing

      Just (tenv, dfun)
	-> returnNF_Tc (Just (substTheta (mkSubst emptyInScopeSet tenv) theta))
        where
	   (_, theta, _) = splitSigmaTy (idType dfun)
\end{code}
