%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
module Inst (
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, zonkLIE, plusLIEs, mkLIE,
	pprInsts, pprInstsInFull,

	Inst, OverloadedLit(..), pprInst,

        InstanceMapper,

	newDictFromOld, newDicts, newDictsAtLoc, 
	newMethod, newMethodWithGivenTy, newOverloadedLit,

	tyVarsOfInst, instLoc, getDictClassTys,

	lookupInst, lookupSimpleInst, LookupInstResult(..),

	isDict, isTyVarDict, isStdClassTyVarDict, isMethodFor,
	instBindingRequired, instCanBeGeneralised,

	zonkInst, instToId,

	InstOrigin(..), pprOrigin
    ) where

#include "HsVersions.h"

import CmdLineOpts ( opt_AllowOverlappingInstances )
import HsSyn	( HsLit(..), HsExpr(..), MonoBinds )
import RnHsSyn	( RenamedArithSeqInfo, RenamedHsExpr )
import TcHsSyn	( TcExpr, TcIdOcc(..), TcIdBndr, 
		  mkHsTyApp, mkHsDictApp, tcIdTyVars, zonkTcId
		)
import TcMonad
import TcEnv	( tcLookupGlobalValueByKey, tcLookupTyConByKey )
import TcType	( TcThetaType,
		  TcType, TcTauType, TcMaybe, TcTyVarSet,
		  tcInstType, zonkTcType, zonkTcTypes, tcSplitForAllTy,
		  zonkTcThetaType
		)
import Bag	( emptyBag, unitBag, unionBags, unionManyBags,
		  listToBag, consBag, Bag )
import Class	( classInstEnv,
		  Class, ClassInstEnv 
		)
import MkId	( mkUserLocal, mkSysLocal )
import Id	( Id, idType, mkId,
		  GenIdSet, elementOfIdSet
		)
import PrelInfo	( isStandardClass, isCcallishClass, isNoDictClass )
import Name	( OccName(..), Name, occNameString, getOccName )
import PprType	( TyCon, pprConstraint )	
import SpecEnv	( SpecEnv, lookupSpecEnv )
import SrcLoc	( SrcLoc )
import Type	( Type, ThetaType, instantiateTy, instantiateThetaTy,
		  isTyVarTy, mkDictTy, splitForAllTys, splitSigmaTy,
		  splitRhoTy, tyVarsOfType, tyVarsOfTypes,
		  mkSynTy
		)
import TyVar	( zipTyVarEnv, lookupTyVarEnv, unionTyVarSets )
import TysPrim	  ( intPrimTy )
import TysWiredIn ( intDataCon, integerTy, isIntTy, isIntegerTy, inIntRange )
import Unique	( fromRationalClassOpKey, rationalTyConKey,
		  fromIntClassOpKey, fromIntegerClassOpKey, Unique
		)
import Maybes	( MaybeErr, expectJust )
import Util	( thenCmp, zipWithEqual )
import Outputable
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
mkLIE insts	  = listToBag insts
plusLIE lie1 lie2 = lie1 `unionBags` lie2
consLIE inst lie  = inst `consBag` lie
plusLIEs lies	  = unionManyBags lies

zonkLIE :: LIE s -> NF_TcM s (LIE s)
zonkLIE lie = mapBagNF_Tc zonkInst lie

pprInsts :: [Inst s] -> SDoc
pprInsts insts = parens (hsep (punctuate comma (map pprInst insts)))


pprInstsInFull insts
  = vcat (map go insts)
  where
    go inst = quotes (ppr inst) <+> pprOrigin inst
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
	Class		-- The type of the dict is (c ts), where
	[TcType s]	-- c is the class and ts the types;
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

	(TcThetaType s)	-- The (types of the) dictionaries to which the function
			-- must be applied to get the method

	(TcTauType s)	-- The type of the method

	(InstOrigin s)
	SrcLoc

	-- INVARIANT: in (Method u f tys theta tau loc)
	--	type of (f tys dicts(from theta)) = tau

  | LitInst
	Unique
	OverloadedLit
	(TcType s)	-- The type at which the literal is used
	(InstOrigin s)	-- Always a literal; but more convenient to carry this around
	SrcLoc

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
instance Ord (Inst s) where
  compare = cmpInst

instance Eq (Inst s) where
  (==) i1 i2 = case i1 `cmpInst` i2 of
	         EQ    -> True
		 other -> False

cmpInst  (Dict _ clas1 tys1 _ _) (Dict _ clas2 tys2 _ _)
  = (clas1 `compare` clas2) `thenCmp` (tys1 `compare` tys2)
cmpInst (Dict _ _ _ _ _) other
  = LT


cmpInst (Method _ _ _ _ _ _ _) (Dict _ _ _ _ _)
  = GT
cmpInst (Method _ id1 tys1 _ _ _ _) (Method _ id2 tys2 _ _ _ _)
  = (id1 `compare` id2) `thenCmp` (tys1 `compare` tys2)
cmpInst (Method _ _ _ _ _ _ _) other
  = LT

cmpInst (LitInst _ lit1 ty1 _ _) (LitInst _ lit2 ty2 _ _)
  = (lit1 `cmpOverLit` lit2) `thenCmp` (ty1 `compare` ty2)
cmpInst (LitInst _ _ _ _ _) other
  = GT

cmpOverLit (OverloadedIntegral   i1) (OverloadedIntegral   i2) = i1 `compare` i2
cmpOverLit (OverloadedFractional f1) (OverloadedFractional f2) = f1 `compare` f2
cmpOverLit (OverloadedIntegral _)    (OverloadedFractional _)  = LT
cmpOverLit (OverloadedFractional _)  (OverloadedIntegral _)    = GT
\end{code}


Selection
~~~~~~~~~
\begin{code}
instOrigin (Dict   u clas tys    origin loc) = origin
instOrigin (Method u clas ty _ _ origin loc) = origin
instOrigin (LitInst u lit ty     origin loc) = origin

instLoc (Dict   u clas tys    origin loc) = loc
instLoc (Method u clas ty _ _ origin loc) = loc
instLoc (LitInst u lit ty     origin loc) = loc

getDictClassTys (Dict u clas tys _ _) = (clas, tys)

tyVarsOfInst :: Inst s -> TcTyVarSet s
tyVarsOfInst (Dict _ _ tys _ _)        = tyVarsOfTypes  tys
tyVarsOfInst (Method _ id tys _ _ _ _) = tyVarsOfTypes tys `unionTyVarSets` tcIdTyVars id
					 -- The id might not be a RealId; in the case of
					 -- locally-overloaded class methods, for example
tyVarsOfInst (LitInst _ _ ty _ _)     = tyVarsOfType  ty
\end{code}

Predicates
~~~~~~~~~~
\begin{code}
isDict :: Inst s -> Bool
isDict (Dict _ _ _ _ _) = True
isDict other	        = False

isMethodFor :: GenIdSet (TcType s) -> Inst s -> Bool
isMethodFor ids (Method uniq (TcId id) tys _ _ orig loc) 
  = id `elementOfIdSet` ids
isMethodFor ids inst 
  = False

isTyVarDict :: Inst s -> Bool
isTyVarDict (Dict _ _ tys _ _) = all isTyVarTy tys
isTyVarDict other 	       = False

isStdClassTyVarDict (Dict _ clas [ty] _ _) = isStandardClass clas && isTyVarTy ty
isStdClassTyVarDict other		   = False
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


Construction
~~~~~~~~~~~~

\begin{code}
newDicts :: InstOrigin s
	 -> TcThetaType s
	 -> NF_TcM s (LIE s, [TcIdOcc s])
newDicts orig theta
  = tcGetSrcLoc				`thenNF_Tc` \ loc ->
    newDictsAtLoc orig loc theta        `thenNF_Tc` \ (dicts, ids) ->
    returnNF_Tc (listToBag dicts, ids)

-- Local function, similar to newDicts, 
-- but with slightly different interface
newDictsAtLoc :: InstOrigin s
              -> SrcLoc
 	      -> TcThetaType s
	      -> NF_TcM s ([Inst s], [TcIdOcc s])
newDictsAtLoc orig loc theta =
 tcGetUniques (length theta)		`thenNF_Tc` \ new_uniqs ->
 let
  mk_dict u (clas, tys) = Dict u clas tys orig loc
  dicts = zipWithEqual "newDictsAtLoc" mk_dict new_uniqs theta
 in
 returnNF_Tc (dicts, map instToId dicts)

newDictFromOld :: Inst s -> Class -> [TcType s] -> NF_TcM s (Inst s)
newDictFromOld (Dict _ _ _ orig loc) clas tys
  = tcGetUnique	      `thenNF_Tc` \ uniq ->
    returnNF_Tc (Dict uniq clas tys orig loc)


newMethod :: InstOrigin s
	  -> TcIdOcc s
	  -> [TcType s]
	  -> NF_TcM s (LIE s, TcIdOcc s)
newMethod orig id tys
  =   	-- Get the Id type and instantiate it at the specified types
    (case id of
       RealId id -> let (tyvars, rho) = splitForAllTys (idType id)
		    in
		    ASSERT( length tyvars == length tys)
		    tcInstType (zipTyVarEnv tyvars tys) rho

       TcId   id -> tcSplitForAllTy (idType id) 	`thenNF_Tc` \ (tyvars, rho) -> 
		    returnNF_Tc (instantiateTy (zipTyVarEnv tyvars tys) rho)
    )						`thenNF_Tc` \ rho_ty ->
    let
	(theta, tau) = splitRhoTy rho_ty
    in
	 -- Our friend does the rest
    newMethodWithGivenTy orig id tys theta tau


newMethodWithGivenTy orig id tys theta tau
  = tcGetSrcLoc		`thenNF_Tc` \ loc ->
    tcGetUnique		`thenNF_Tc` \ new_uniq ->
    let
	meth_inst = Method new_uniq id tys theta tau orig loc
    in
    returnNF_Tc (unitLIE meth_inst, instToId meth_inst)

newMethodAtLoc :: InstOrigin s -> SrcLoc
	       -> Id -> [TcType s]
	       -> NF_TcM s (Inst s, TcIdOcc s)
newMethodAtLoc orig loc real_id tys	-- Local function, similar to newMethod but with 
					-- slightly different interface
  =   	-- Get the Id type and instantiate it at the specified types
    let
	 (tyvars,rho) = splitForAllTys (idType real_id)
    in
    tcInstType (zipTyVarEnv tyvars tys) rho	`thenNF_Tc` \ rho_ty ->
    tcGetUnique					`thenNF_Tc` \ new_uniq ->
    let
	(theta, tau) = splitRhoTy rho_ty
	meth_inst    = Method new_uniq (RealId real_id) tys theta tau orig loc
    in
    returnNF_Tc (meth_inst, instToId meth_inst)

newOverloadedLit :: InstOrigin s
		 -> OverloadedLit
		 -> TcType s
		 -> NF_TcM s (TcExpr s, LIE s)
newOverloadedLit orig (OverloadedIntegral i) ty
  | isIntTy ty && inIntRange i		-- Short cut for Int
  = returnNF_Tc (int_lit, emptyLIE)

  | isIntegerTy ty 			-- Short cut for Integer
  = returnNF_Tc (integer_lit, emptyLIE)

  where
    intprim_lit    = HsLitOut (HsIntPrim i) intPrimTy
    integer_lit    = HsLitOut (HsInt i) integerTy
    int_lit        = HsApp (HsVar (RealId intDataCon)) intprim_lit

newOverloadedLit orig lit ty		-- The general case
  = tcGetSrcLoc			`thenNF_Tc` \ loc ->
    tcGetUnique			`thenNF_Tc` \ new_uniq ->
    let
	lit_inst = LitInst new_uniq lit ty orig loc
    in
    returnNF_Tc (HsVar (instToId lit_inst), unitLIE lit_inst)
\end{code}


\begin{code}
instToId :: Inst s -> TcIdOcc s
instToId (Dict u clas ty orig loc)
  = TcId (mkUserLocal occ u (mkDictTy clas ty) loc)
  where
    occ = VarOcc (SLIT("d.") _APPEND_ (occNameString (getOccName clas)))

instToId (Method u id tys theta tau orig loc)
  = TcId (mkUserLocal (getOccName id) u tau loc)
    
instToId (LitInst u list ty orig loc)
  = TcId (mkSysLocal SLIT("lit") u ty loc)
\end{code}


Zonking
~~~~~~~
Zonking makes sure that the instance types are fully zonked,
but doesn't do the same for the Id in a Method.  There's no
need, and it's a lot of extra work.

\begin{code}
zonkInst :: Inst s -> NF_TcM s (Inst s)
zonkInst (Dict u clas tys orig loc)
  = zonkTcTypes	tys			`thenNF_Tc` \ new_tys ->
    returnNF_Tc (Dict u clas new_tys orig loc)

zonkInst (Method u id tys theta tau orig loc) 
  = zonkTcId id			`thenNF_Tc` \ new_id ->
      -- Essential to zonk the id in case it's a local variable
    zonkTcTypes tys		`thenNF_Tc` \ new_tys ->
    zonkTcThetaType theta	`thenNF_Tc` \ new_theta ->
    zonkTcType tau		`thenNF_Tc` \ new_tau ->
    returnNF_Tc (Method u new_id new_tys new_theta new_tau orig loc)

zonkInst (LitInst u lit ty orig loc)
  = zonkTcType ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (LitInst u lit new_ty orig loc)
\end{code}


Printing
~~~~~~~~
ToDo: improve these pretty-printing things.  The ``origin'' is really only
relevant in error messages.

\begin{code}
instance Outputable (Inst s) where
    ppr inst = pprInst inst

pprInst (LitInst u lit ty orig loc)
  = hsep [case lit of
	      OverloadedIntegral   i -> integer i
	      OverloadedFractional f -> rational f,
	   ptext SLIT("at"),
	   ppr ty,
	   show_uniq u]

pprInst (Dict u clas tys orig loc) = pprConstraint clas tys <+> show_uniq u

pprInst (Method u id tys _ _ orig loc)
  = hsep [ppr id, ptext SLIT("at"), 
	  interppSP tys,
	  show_uniq u]

show_uniq u = ifPprDebug (text "{-" <> ppr u <> text "-}")
\end{code}


%************************************************************************
%*									*
\subsection[InstEnv-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InstanceMapper = Class -> ClassInstEnv
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
  | SimpleInst (TcExpr s)		-- Just a variable, type application, or literal
  | GenInst    [Inst s] (TcExpr s)	-- The expression and its needed insts
lookupInst :: Inst s 
	   -> NF_TcM s (LookupInstResult s)

-- Dictionaries

lookupInst dict@(Dict _ clas tys orig loc)
  = case lookupSpecEnv (classInstEnv clas) tys of

      Just (tenv, dfun_id)
	-> let
		(tyvars, rho) = splitForAllTys (idType dfun_id)
		ty_args	      = map (expectJust "Inst" . lookupTyVarEnv tenv) tyvars
				-- tenv should bind all the tyvars
	   in
	   tcInstType tenv rho		`thenNF_Tc` \ dfun_rho ->
	   let
		(theta, tau) = splitRhoTy dfun_rho
		ty_app       = mkHsTyApp (HsVar (RealId dfun_id)) ty_args
	   in
	   if null theta then
		returnNF_Tc (SimpleInst ty_app)
	   else
	   newDictsAtLoc orig loc theta	`thenNF_Tc` \ (dicts, dict_ids) ->
	   let 
		rhs = mkHsDictApp ty_app dict_ids
	   in
	   returnNF_Tc (GenInst dicts rhs)
			     
      Nothing	-> returnNF_Tc NoInstance

-- Methods

lookupInst inst@(Method _ id tys theta _ orig loc)
  = newDictsAtLoc orig loc theta	`thenNF_Tc` \ (dicts, dict_ids) ->
    returnNF_Tc (GenInst dicts (mkHsDictApp (mkHsTyApp (HsVar id) tys) dict_ids))

-- Literals

lookupInst inst@(LitInst u (OverloadedIntegral i) ty orig loc)
  | isIntTy ty && in_int_range			-- Short cut for Int
  = returnNF_Tc (GenInst [] int_lit)
	-- GenInst, not SimpleInst, because int_lit is actually a constructor application

  | isIntegerTy ty				-- Short cut for Integer
  = returnNF_Tc (GenInst [] integer_lit)

  | in_int_range				-- It's overloaded but small enough to fit into an Int
  = tcLookupGlobalValueByKey fromIntClassOpKey	`thenNF_Tc` \ from_int ->
    newMethodAtLoc orig loc from_int [ty]	`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) int_lit))

  | otherwise   				-- Alas, it is overloaded and a big literal!
  = tcLookupGlobalValueByKey fromIntegerClassOpKey	`thenNF_Tc` \ from_integer ->
    newMethodAtLoc orig loc from_integer [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) integer_lit))
  where
    in_int_range   = inIntRange i
    intprim_lit    = HsLitOut (HsIntPrim i) intPrimTy
    integer_lit    = HsLitOut (HsInt i) integerTy
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
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) rational_lit))
\end{code}

There is a second, simpler interface, when you want an instance of a
class at a given nullary type constructor.  It just returns the
appropriate dictionary if it exists.  It is used only when resolving
ambiguous dictionaries.

\begin{code}
lookupSimpleInst :: ClassInstEnv
		 -> Class
		 -> [Type]			-- Look up (c,t)
	         -> NF_TcM s (Maybe ThetaType)		-- Here are the needed (c,t)s

lookupSimpleInst class_inst_env clas tys
  = case lookupSpecEnv class_inst_env tys of
      Nothing	 -> returnNF_Tc Nothing

      Just (tenv, dfun)
	-> returnNF_Tc (Just (instantiateThetaTy tenv theta))
        where
	   (_, theta, _) = splitSigmaTy (idType dfun)
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
  | Rank2Origin			-- A dict created when typechecking the argument
				-- of a rank-2 typed function

  | DoOrigin			-- The monad for a do expression

  | ClassDeclOrigin		-- Manufactured during a class decl

  | InstanceSpecOrigin	Class	-- in a SPECIALIZE instance pragma
			Type

	-- When specialising instances the instance info attached to
	-- each class is not yet ready, so we record it inside the
	-- origin information.  This is a bit of a hack, but it works
	-- fine.  (Patrick is to blame [WDP].)

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
pprOrigin :: Inst s -> SDoc
pprOrigin inst
  = hsep [text "arising from", pp_orig orig, text "at", ppr locn]
  where
    (orig, locn) = case inst of
			Dict _ _ _       orig loc -> (orig,loc)
			Method _ _ _ _ _ orig loc -> (orig,loc)
			LitInst _ _ _    orig loc -> (orig,loc)
			
    pp_orig (OccurrenceOf id)
      	= hsep [ptext SLIT("use of"), quotes (ppr id)]
    pp_orig (OccurrenceOfCon id)
	= hsep [ptext SLIT("use of"), quotes (ppr id)]
    pp_orig (LiteralOrigin lit)
	= hsep [ptext SLIT("the literal"), quotes (ppr lit)]
    pp_orig (InstanceDeclOrigin)
	=  ptext SLIT("an instance declaration")
    pp_orig (ArithSeqOrigin seq)
	= hsep [ptext SLIT("the arithmetic sequence"), quotes (ppr seq)]
    pp_orig (SignatureOrigin)
	=  ptext SLIT("a type signature")
    pp_orig (Rank2Origin)
	=  ptext SLIT("a function with an overloaded argument type")
    pp_orig (DoOrigin)
	=  ptext SLIT("a do statement")
    pp_orig (ClassDeclOrigin)
	=  ptext SLIT("a class declaration")
    pp_orig (InstanceSpecOrigin clas ty)
	= hsep [text "a SPECIALIZE instance pragma; class",
	        quotes (ppr clas), text "type:", ppr ty]
    pp_orig (ValSpecOrigin name)
	= hsep [ptext SLIT("a SPECIALIZE user-pragma for"), quotes (ppr name)]
    pp_orig (CCallOrigin clabel Nothing{-ccall result-})
	= hsep [ptext SLIT("the result of the _ccall_ to"), quotes (text clabel)]
    pp_orig (CCallOrigin clabel (Just arg_expr))
	= hsep [ptext SLIT("an argument in the _ccall_ to"), quotes (text clabel) <> comma, 
		text "namely", quotes (ppr arg_expr)]
    pp_orig (LitLitOrigin s)
	= hsep [ptext SLIT("the ``literal-literal''"), quotes (text s)]
    pp_orig (UnknownOrigin)
	= ptext SLIT("...oops -- I don't know where the overloading came from!")
\end{code}
