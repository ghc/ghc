%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
#include "HsVersions.h"

module Inst (
	Inst(..), 	-- Visible only to TcSimplify

	InstOrigin(..), OverloadedLit(..),
	LIE(..), emptyLIE, unitLIE, plusLIE, consLIE, zonkLIE, plusLIEs,

        InstanceMapper(..),

	newDicts, newDictsAtLoc, newMethod, newMethodWithGivenTy, newOverloadedLit,

	instType, tyVarsOfInst, lookupInst, lookupSimpleInst,

	isDict, isTyVarDict, 

	zonkInst, instToId,

	matchesInst,
	instBindingRequired, instCanBeGeneralised,
	
	pprInst
    ) where

IMP_Ubiq()

import HsSyn	( HsLit(..), HsExpr(..), HsBinds, 
		  InPat, OutPat, Stmt, Qualifier, Match,
		  ArithSeqInfo, PolyType, Fake )
import RnHsSyn	( RenamedArithSeqInfo(..), RenamedHsExpr(..) )
import TcHsSyn	( TcIdOcc(..), TcExpr(..), TcIdBndr(..),
		  mkHsTyApp, mkHsDictApp, tcIdTyVars )

import TcMonad	hiding ( rnMtoTcM )
import TcEnv	( tcLookupGlobalValueByKey, tcLookupTyConByKey )
import TcType	( TcType(..), TcRhoType(..), TcMaybe, TcTyVarSet(..),
		  tcInstType, zonkTcType )

import Bag	( emptyBag, unitBag, unionBags, unionManyBags, listToBag, consBag )
import Class	( isCcallishClass, isNoDictClass, classInstEnv,
		  SYN_IE(Class), GenClass, SYN_IE(ClassInstEnv)
		)
import ErrUtils ( addErrLoc, SYN_IE(Error) )
import Id	( GenId, idType, mkInstId )
import MatchEnv	( lookupMEnv, insertMEnv )
import Name	( mkLocalName, getLocalName, Name )
import Outputable
import PprType	( GenClass, TyCon, GenType, GenTyVar )	
import PprStyle	( PprStyle(..) )
import Pretty
import RnHsSyn	( RnName{-instance NamedThing-} )
import SpecEnv	( SYN_IE(SpecEnv) )
import SrcLoc	( SrcLoc, mkUnknownSrcLoc )
import Type	( GenType, eqSimpleTy, instantiateTy,
		  isTyVarTy, mkDictTy, splitForAllTy, splitSigmaTy,
		  splitRhoTy, matchTy, tyVarsOfType, tyVarsOfTypes,
		  mkSynTy
		)
import TyVar	( unionTyVarSets, GenTyVar )
import TysPrim	  ( intPrimTy )
import TysWiredIn ( intDataCon, integerTy )
import Unique	( showUnique, fromRationalClassOpKey, rationalTyConKey,
		  fromIntClassOpKey, fromIntegerClassOpKey, Unique
		)
import Util	( panic, zipEqual, zipWithEqual, assoc, assertPanic, pprTrace{-ToDo:rm-} )
\end{code}

%************************************************************************
%*									*
\subsection[Inst-collections]{LIE: a collection of Insts}
%*									*
%************************************************************************

\begin{code}
type LIE s = Bag (Inst s)

emptyLIE          = emptyBag
unitLIE inst 	  = unitBag inst
plusLIE lie1 lie2 = lie1 `unionBags` lie2
consLIE inst lie  = inst `consBag` lie
plusLIEs lies	  = unionManyBags lies

zonkLIE :: LIE s -> NF_TcM s (LIE s)
zonkLIE lie = mapBagNF_Tc zonkInst lie
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
data Inst s
  = Dict
	Unique
	Class		-- The type of the dict is (c t), where
	(TcType s)	-- c is the class and t the type;
	(InstOrigin s)
	SrcLoc

  | Method
	Unique

	(TcIdOcc s)	-- The overloaded function
			-- This function will be a global, local, or ClassOpId;
			--   inside instance decls (only) it can also be an InstId!
			-- The id needn't be completely polymorphic.
			-- You'll probably find its name (for documentation purposes)
			--	  inside the InstOrigin

	[TcType s]	-- The types to which its polymorphic tyvars
			--	should be instantiated.
			-- These types must saturate the Id's foralls.

	(TcRhoType s)	-- Cached: (type-of-id applied to inst_tys)
			-- If this type is (theta => tau) then the type of the Method
			-- is tau, and the method can be built by saying 
			--	id inst_tys dicts
			-- where dicts are constructed from theta

	(InstOrigin s)
	SrcLoc

  | LitInst
	Unique
	OverloadedLit
	(TcType s)	-- The type at which the literal is used
	(InstOrigin s)	-- Always a literal; but more convenient to carry this around
	SrcLoc

data OverloadedLit
  = OverloadedIntegral	 Integer	-- The number
  | OverloadedFractional Rational	-- The number

getInstOrigin (Dict   u clas ty     origin loc) = origin
getInstOrigin (Method u clas ty rho origin loc) = origin
getInstOrigin (LitInst u lit ty     origin loc) = origin
\end{code}

Construction
~~~~~~~~~~~~

\begin{code}
newDicts :: InstOrigin s
	 -> [(Class, TcType s)]
	 -> NF_TcM s (LIE s, [TcIdOcc s])
newDicts orig theta
  = tcGetSrcLoc				`thenNF_Tc` \ loc ->
    tcGetUniques (length theta)		`thenNF_Tc` \ new_uniqs ->
    let
	mk_dict u (clas, ty) = Dict u clas ty orig loc
	dicts = zipWithEqual "newDicts" mk_dict new_uniqs theta
    in
    returnNF_Tc (listToBag dicts, map instToId dicts)

newDictsAtLoc orig loc theta	-- Local function, similar to newDicts, 
				-- but with slightly different interface
  = tcGetUniques (length theta)		`thenNF_Tc` \ new_uniqs ->
    let
	mk_dict u (clas, ty) = Dict u clas ty orig loc
	dicts = zipWithEqual "newDictsAtLoc" mk_dict new_uniqs theta
    in
    returnNF_Tc (dicts, map instToId dicts)

newMethod :: InstOrigin s
	  -> TcIdOcc s
	  -> [TcType s]
	  -> NF_TcM s (LIE s, TcIdOcc s)
newMethod orig id tys
  =   	-- Get the Id type and instantiate it at the specified types
    (case id of
       RealId id -> let (tyvars, rho) = splitForAllTy (idType id)
		    in
		    (if length tyvars /= length tys then pprTrace "newMethod" (ppr PprDebug (idType id)) else \x->x) $
		    tcInstType (zip{-Equal "newMethod"-} tyvars tys) rho
       TcId   id -> let (tyvars, rho) = splitForAllTy (idType id)
		    in returnNF_Tc (instantiateTy (zipEqual "newMethod(2)" tyvars tys) rho)
    )						`thenNF_Tc` \ rho_ty ->
	 -- Our friend does the rest
    newMethodWithGivenTy orig id tys rho_ty


newMethodWithGivenTy orig id tys rho_ty
  = tcGetSrcLoc		`thenNF_Tc` \ loc ->
    tcGetUnique		`thenNF_Tc` \ new_uniq ->
    let
	meth_inst = Method new_uniq id tys rho_ty orig loc
    in
    returnNF_Tc (unitLIE meth_inst, instToId meth_inst)

newMethodAtLoc :: InstOrigin s -> SrcLoc -> Id -> [TcType s] -> NF_TcM s (Inst s, TcIdOcc s)
newMethodAtLoc orig loc real_id tys	-- Local function, similar to newMethod but with 
					-- slightly different interface
  =   	-- Get the Id type and instantiate it at the specified types
    let
	 (tyvars,rho) = splitForAllTy (idType real_id)
    in
    tcInstType (zipEqual "newMethodAtLoc" tyvars tys) rho `thenNF_Tc` \ rho_ty ->
    tcGetUnique						  `thenNF_Tc` \ new_uniq ->
    let
	meth_inst = Method new_uniq (RealId real_id) tys rho_ty orig loc
    in
    returnNF_Tc (meth_inst, instToId meth_inst)

newOverloadedLit :: InstOrigin s
		 -> OverloadedLit
		 -> TcType s
		 -> NF_TcM s (LIE s, TcIdOcc s)
newOverloadedLit orig lit ty
  = tcGetSrcLoc			`thenNF_Tc` \ loc ->
    tcGetUnique			`thenNF_Tc` \ new_uniq ->
    let
	lit_inst = LitInst new_uniq lit ty orig loc
    in
    returnNF_Tc (unitLIE lit_inst, instToId lit_inst)
\end{code}


\begin{code}
instToId :: Inst s -> TcIdOcc s
instToId (Dict u clas ty orig loc)
  = TcId (mkInstId u (mkDictTy clas ty) (mkLocalName u str False{-emph name-} loc))
  where
    str = SLIT("d.") _APPEND_ (getLocalName clas)
instToId (Method u id tys rho_ty orig loc)
  = TcId (mkInstId u tau_ty (mkLocalName u str False{-emph name-} loc))
  where
    (_, tau_ty) = splitRhoTy rho_ty	-- NB The method Id has just the tau type
    str = SLIT("m.") _APPEND_ (getLocalName id)

instToId (LitInst u list ty orig loc)
  = TcId (mkInstId u ty (mkLocalName u SLIT("lit") True{-emph uniq-} loc))
\end{code}

\begin{code}
instType :: Inst s -> TcType s
instType (Dict _ clas ty _ _)     = mkDictTy clas ty
instType (LitInst _ _ ty _ _)     = ty
instType (Method _ id tys ty _ _) = ty
\end{code}


Zonking
~~~~~~~
Zonking makes sure that the instance types are fully zonked,
but doesn't do the same for the Id in a Method.  There's no
need, and it's a lot of extra work.

\begin{code}
zonkInst :: Inst s -> NF_TcM s (Inst s)
zonkInst (Dict u clas ty orig loc)
  = zonkTcType	ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (Dict u clas new_ty orig loc)

zonkInst (Method u id tys rho orig loc) 		-- Doesn't zonk the id!
  = mapNF_Tc zonkTcType tys		`thenNF_Tc` \ new_tys ->
    zonkTcType rho			`thenNF_Tc` \ new_rho ->
    returnNF_Tc (Method u id new_tys new_rho orig loc)

zonkInst (LitInst u lit ty orig loc)
  = zonkTcType ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (LitInst u lit new_ty orig loc)
\end{code}


\begin{code}
tyVarsOfInst :: Inst s -> TcTyVarSet s
tyVarsOfInst (Dict _ _ ty _ _)        = tyVarsOfType  ty
tyVarsOfInst (Method _ id tys rho _ _) = tyVarsOfTypes tys `unionTyVarSets` tcIdTyVars id
					 -- The id might not be a RealId; in the case of
					 -- locally-overloaded class methods, for example
tyVarsOfInst (LitInst _ _ ty _ _)     = tyVarsOfType  ty
\end{code}

@matchesInst@ checks when two @Inst@s are instances of the same
thing at the same type, even if their uniques differ.

\begin{code}
matchesInst :: Inst s -> Inst s -> Bool

matchesInst (Dict _ clas1 ty1 _ _) (Dict _ clas2 ty2 _ _)
  = clas1 == clas2 && ty1 `eqSimpleTy` ty2

matchesInst (Method _ id1 tys1 _ _ _) (Method _ id2 tys2 _ _ _)
  =  id1 == id2
  && and (zipWith eqSimpleTy tys1 tys2)
  && length tys1 == length tys2

matchesInst (LitInst _ lit1 ty1 _ _) (LitInst _ lit2 ty2 _ _)
  = lit1 `eq` lit2 && ty1 `eqSimpleTy` ty2
  where
    (OverloadedIntegral   i1) `eq` (OverloadedIntegral   i2) = i1 == i2
    (OverloadedFractional f1) `eq` (OverloadedFractional f2) = f1 == f2
    _			      `eq` _			     = False

matchesInst other1 other2 = False
\end{code}


Predicates
~~~~~~~~~~
\begin{code}
isDict :: Inst s -> Bool
isDict (Dict _ _ _ _ _) = True
isDict other	        = False

isTyVarDict :: Inst s -> Bool
isTyVarDict (Dict _ _ ty _ _) = isTyVarTy ty
isTyVarDict other 	      = False
\end{code}

Two predicates which deal with the case where class constraints don't
necessarily result in bindings.  The first tells whether an @Inst@
must be witnessed by an actual binding; the second tells whether an
@Inst@ can be generalised over.

\begin{code}
instBindingRequired :: Inst s -> Bool
instBindingRequired (Dict _ clas _ _ _) = not (isNoDictClass clas)
instBindingRequired other		= True

instCanBeGeneralised :: Inst s -> Bool
instCanBeGeneralised (Dict _ clas _ _ _) = not (isCcallishClass clas)
instCanBeGeneralised other		 = True
\end{code}


Printing
~~~~~~~~
ToDo: improve these pretty-printing things.  The ``origin'' is really only
relevant in error messages.

\begin{code}
instance Outputable (Inst s) where
    ppr sty inst = ppr_inst sty ppNil (\ o l -> ppNil) inst

pprInst sty hdr inst = ppr_inst sty hdr (\ o l -> pprOrigin hdr o l sty) inst

ppr_inst sty hdr ppr_orig (LitInst u lit ty orig loc)
  = ppHang (ppr_orig orig loc)
	 4 (ppCat [case lit of
		      OverloadedIntegral   i -> ppInteger i
		      OverloadedFractional f -> ppRational f,
		   ppStr "at",
		   ppr sty ty,
		   show_uniq sty u])

ppr_inst sty hdr ppr_orig (Dict u clas ty orig loc)
  = ppHang (ppr_orig orig loc)
	 4 (ppCat [ppr sty clas, ppr sty ty, show_uniq sty u])

ppr_inst sty hdr ppr_orig (Method u id tys rho orig loc)
  = ppHang (ppr_orig orig loc)
	 4 (ppCat [ppr sty id, ppStr "at", interppSP sty tys, show_uniq sty u])

show_uniq PprDebug u = ppr PprDebug u
show_uniq sty	   u = ppNil
\end{code}

Printing in error messages

\begin{code}
noInstanceErr inst sty = ppHang (ppPStr SLIT("No instance for:")) 4 (ppr sty inst)
\end{code}

%************************************************************************
%*									*
\subsection[InstEnv-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InstanceMapper = Class -> (ClassInstEnv, ClassOp -> SpecEnv)
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
lookupInst :: Inst s 
	   -> TcM s ([Inst s], 
		     (TcIdOcc s, TcExpr s))	-- The new binding

-- Dictionaries

lookupInst dict@(Dict _ clas ty orig loc)
  = case lookupMEnv matchTy (get_inst_env clas orig) ty of
      Nothing	-> tcAddSrcLoc loc		 $
		   tcAddErrCtxt (pprOrigin ""{-hdr-} orig loc) $
		   failTc (noInstanceErr dict)

      Just (dfun_id, tenv) 
	-> let
		(tyvars, rho) = splitForAllTy (idType dfun_id)
		ty_args	      = map (assoc "lookupInst" tenv) tyvars
		-- tenv should bind all the tyvars
	   in
	   tcInstType tenv rho		`thenNF_Tc` \ dfun_rho ->
	   let
		(theta, tau) = splitRhoTy dfun_rho
	   in
	   newDictsAtLoc orig loc theta	`thenNF_Tc` \ (dicts, dict_ids) ->
	   let 
		rhs = mkHsDictApp (mkHsTyApp (HsVar (RealId dfun_id)) ty_args) dict_ids
	   in
	   returnTc (dicts, (instToId dict, rhs))
			     

-- Methods

lookupInst inst@(Method _ id tys rho orig loc)
  = newDictsAtLoc orig loc theta	`thenNF_Tc` \ (dicts, dict_ids) ->
    returnTc (dicts, (instToId inst, mkHsDictApp (mkHsTyApp (HsVar id) tys) dict_ids))
  where
    (theta,_) = splitRhoTy rho

-- Literals

lookupInst inst@(LitInst u (OverloadedIntegral i) ty orig loc)
  | i >= toInteger minInt && i <= toInteger maxInt
  =	-- It's overloaded but small enough to fit into an Int
    tcLookupGlobalValueByKey fromIntClassOpKey	`thenNF_Tc` \ from_int ->
    newMethodAtLoc orig loc from_int [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnTc ([method_inst], (instToId inst, HsApp (HsVar method_id) int_lit))

  | otherwise 
  =     -- Alas, it is overloaded and a big literal!
    tcLookupGlobalValueByKey fromIntegerClassOpKey	`thenNF_Tc` \ from_integer ->
    newMethodAtLoc orig loc from_integer [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnTc ([method_inst], (instToId inst, HsApp (HsVar method_id) (HsLitOut (HsInt i) integerTy)))
  where
    intprim_lit    = HsLitOut (HsIntPrim i) intPrimTy
    int_lit        = HsApp (HsVar (RealId intDataCon)) intprim_lit

lookupInst inst@(LitInst u (OverloadedFractional f) ty orig loc)
  = tcLookupGlobalValueByKey fromRationalClassOpKey	`thenNF_Tc` \ from_rational ->

	-- The type Rational isn't wired in so we have to conjure it up
    tcLookupTyConByKey rationalTyConKey	`thenNF_Tc` \ rational_tycon ->
    let
	rational_ty  = mkSynTy rational_tycon []
	rational_lit = HsLitOut (HsFrac f) rational_ty
    in
    newMethodAtLoc orig loc from_rational [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnTc ([method_inst], (instToId inst, HsApp (HsVar method_id) rational_lit))
\end{code}

There is a second, simpler interface, when you want an instance of a
class at a given nullary type constructor.  It just returns the
appropriate dictionary if it exists.  It is used only when resolving
ambiguous dictionaries.

\begin{code}
lookupSimpleInst :: ClassInstEnv
		 -> Class
		 -> Type			-- Look up (c,t)
	         -> TcM s [(Class,Type)]	-- Here are the needed (c,t)s

lookupSimpleInst class_inst_env clas ty
  = case (lookupMEnv matchTy class_inst_env ty) of
      Nothing	       -> failTc (noSimpleInst clas ty)
      Just (dfun,tenv) -> returnTc [(c,instantiateTy tenv t) | (c,t) <- theta]
		       where
		          (_, theta, _) = splitSigmaTy (idType dfun)

noSimpleInst clas ty sty
  = ppSep [ppStr "No instance for class", ppQuote (ppr sty clas),
	   ppStr "at type", ppQuote (ppr sty ty)]
\end{code}


@mkInstSpecEnv@ is used to construct the @SpecEnv@ for a dfun.
It does it by filtering the class's @InstEnv@.  All pretty shady stuff.

\begin{code}
mkInstSpecEnv clas inst_ty inst_tvs inst_theta = panic "mkInstSpecEnv"
\end{code}

\begin{pseudocode}
mkInstSpecEnv :: Class			-- class
	      -> Type			-- instance type
	      -> [TyVarTemplate]	-- instance tyvars
	      -> ThetaType		-- superclasses dicts
	      -> SpecEnv		-- specenv for dfun of instance

mkInstSpecEnv clas inst_ty inst_tvs inst_theta
  = mkSpecEnv (catMaybes (map maybe_spec_info matches))
  where
    matches = matchMEnv matchTy (classInstEnv clas) inst_ty

    maybe_spec_info (_, match_info, MkInstTemplate dfun _ [])
      = Just (SpecInfo (map (assocMaybe match_info) inst_tvs) (length inst_theta) dfun)
    maybe_spec_info (_, match_info, _)
      = Nothing
\end{pseudocode}


\begin{code}
addClassInst
    :: ClassInstEnv		-- Incoming envt
    -> Type			-- The instance type: inst_ty
    -> Id			-- Dict fun id to apply. Free tyvars of inst_ty must
				-- be the same as the forall'd tyvars of the dfun id.
    -> MaybeErr
	  ClassInstEnv		-- Success
	  (Type, Id)		-- Offending overlap

addClassInst inst_env inst_ty dfun_id = insertMEnv matchTy inst_env inst_ty dfun_id
\end{code}



%************************************************************************
%*									*
\subsection[Inst-origin]{The @InstOrigin@ type}
%*									*
%************************************************************************

The @InstOrigin@ type gives information about where a dictionary came from.
This is important for decent error message reporting because dictionaries
don't appear in the original source code.  Doubtless this type will evolve...

\begin{code}
data InstOrigin s
  = OccurrenceOf (TcIdOcc s)	-- Occurrence of an overloaded identifier
  | OccurrenceOfCon Id		-- Occurrence of a data constructor

  | RecordUpdOrigin

  | DataDeclOrigin		-- Typechecking a data declaration

  | InstanceDeclOrigin		-- Typechecking an instance decl

  | LiteralOrigin	HsLit	-- Occurrence of a literal

  | ArithSeqOrigin	RenamedArithSeqInfo -- [x..], [x..y] etc

  | SignatureOrigin		-- A dict created from a type signature

  | DoOrigin			-- The monad for a do expression

  | ClassDeclOrigin		-- Manufactured during a class decl

-- 	NO MORE!
--  | DerivingOrigin	InstanceMapper
--			Class
--			TyCon

	-- During "deriving" operations we have an ever changing
	-- mapping of classes to instances, so we record it inside the
	-- origin information.  This is a bit of a hack, but it works
	-- fine.  (Simon is to blame [WDP].)

  | InstanceSpecOrigin	InstanceMapper
			Class	-- in a SPECIALIZE instance pragma
			Type

	-- When specialising instances the instance info attached to
	-- each class is not yet ready, so we record it inside the
	-- origin information.  This is a bit of a hack, but it works
	-- fine.  (Patrick is to blame [WDP].)

--  | DefaultDeclOrigin		-- Related to a `default' declaration

  | ValSpecOrigin	Name	-- in a SPECIALIZE pragma for a value

	-- Argument or result of a ccall
	-- Dictionaries with this origin aren't actually mentioned in the
	-- translated term, and so need not be bound.  Nor should they
	-- be abstracted over.

  | CCallOrigin		String			-- CCall label
			(Maybe RenamedHsExpr)	-- Nothing if it's the result
						-- Just arg, for an argument

  | LitLitOrigin	String	-- the litlit

  | UnknownOrigin	-- Help! I give up...
\end{code}

\begin{code}
-- During deriving and instance specialisation operations
-- we can't get the instances of the class from inside the
-- class, because the latter ain't ready yet.  Instead we
-- find a mapping from classes to envts inside the dict origin.

get_inst_env :: Class -> InstOrigin s -> ClassInstEnv
-- get_inst_env clas (DerivingOrigin inst_mapper _ _)
--  = fst (inst_mapper clas)
get_inst_env clas (InstanceSpecOrigin inst_mapper _ _)
  = fst (inst_mapper clas)
get_inst_env clas other_orig = classInstEnv clas


pprOrigin :: String -> InstOrigin s -> SrcLoc -> Error

pprOrigin hdr orig locn
  = addErrLoc locn hdr $ \ sty ->
    case orig of
      OccurrenceOf id ->
        ppBesides [ppPStr SLIT("at a use of an overloaded identifier: `"),
		   ppr sty id, ppChar '\'']
      OccurrenceOfCon id ->
        ppBesides [ppPStr SLIT("at a use of an overloaded constructor: `"),
		   ppr sty id, ppChar '\'']
      InstanceDeclOrigin ->
	ppStr "in an instance declaration"
      LiteralOrigin lit ->
	ppCat [ppStr "at an overloaded literal:", ppr sty lit]
      ArithSeqOrigin seq ->
	ppCat [ppStr "at an arithmetic sequence:", ppr sty seq]
      SignatureOrigin ->
	ppStr "in a type signature"
      DoOrigin ->
	ppStr "in a do statement"
      ClassDeclOrigin ->
	ppStr "in a class declaration"
      InstanceSpecOrigin _ clas ty ->
	ppBesides [ppStr "in a SPECIALIZE instance pragma; class \"",
	 	   ppr sty clas, ppStr "\" type: ", ppr sty ty]
      ValSpecOrigin name ->
	ppBesides [ppStr "in a SPECIALIZE user-pragma for `",
		   ppr sty name, ppStr "'"]
      CCallOrigin clabel Nothing{-ccall result-} ->
	ppBesides [ppStr "in the result of the _ccall_ to `",
		   ppStr clabel, ppStr "'"]
      CCallOrigin clabel (Just arg_expr) ->
	ppBesides [ppStr "in an argument in the _ccall_ to `",
		  ppStr clabel, ppStr "', namely: ", ppr sty arg_expr]
      LitLitOrigin s ->
	ppBesides [ppStr "in this ``literal-literal'': ", ppStr s]
      UnknownOrigin ->
	ppStr "in... oops -- I don't know where the overloading came from!"
\end{code}
