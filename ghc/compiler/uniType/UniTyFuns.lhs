%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[UniTyFuns]{Utility functions for @UniTypes@}

This is one of the modules whose functions know about the internal
representation of @UniTypes@ (and @TyCons@ and ... ?).

\begin{code}
#include "HsVersions.h"

module UniTyFuns (

	-- CONSTRUCTION
	applyTy, applyTyCon, applySynTyCon, applyNonSynTyCon,
	{-mkSigmaTy,-} glueTyArgs, mkSuperDictSelType, --UNUSED: mkDictFunType,
	specialiseTy,

	-- DESTRUCTION
--not exported:	expandTySyns,
	expandVisibleTySyn,
	getTyVar, getTyVarMaybe, getTyVarTemplateMaybe,
	splitType, splitForalls, getTauType, splitTyArgs,
	splitTypeWithDictsAsArgs,
--not exported/unused:	sourceTypes, targetType,
	funResultTy,
	splitDictType,
	kindFromType,
	getUniDataTyCon, getUniDataTyCon_maybe,
	getUniDataSpecTyCon, getUniDataSpecTyCon_maybe,
	unDictifyTy,
	getMentionedTyCons,
#ifdef USE_SEMANTIQUE_STRANAL
	getReferredToTyCons,
#endif {- Semantique strictness analyser -}
	getMentionedTyConsAndClassesFromUniType,
	getMentionedTyConsAndClassesFromTyCon,
    	getMentionedTyConsAndClassesFromClass,
	getUniTyDescription,

	-- FREE-VARIABLE EXTRACTION
	extractTyVarsFromTy, extractTyVarsFromTys,
	extractTyVarTemplatesFromTy,

	-- PREDICATES
	isTyVarTy, isTyVarTemplateTy,
	maybeUnpackFunTy, isFunType,
	isPrimType, isUnboxedDataType, -- UNUSED: isDataConType,
	isLeakFreeType,
	maybeBoxedPrimType,
--UNUSED:	hasHigherOrderArg,
	isDictTy, isGroundTy, isGroundOrTyVarTy,
	instanceIsExported,
-- UNUSED:	isSynTarget,
	isTauTy, isForAllTy,
	maybePurelyLocalTyCon, maybePurelyLocalClass, maybePurelyLocalType,
	returnsRealWorld, -- HACK courtesy of SLPJ
#ifdef DPH
        isProcessorTy,
	runtimeUnpodizableType,
#endif {- Data Parallel Haskell -}

	-- SUBSTITUTION
	applyTypeEnvToTy, applyTypeEnvToThetaTy,
--not exported : applyTypeEnvToTauTy,
	mapOverTyVars,
	-- moved to Subst: applySubstToTauTy, applySubstToTy, applySubstToThetaTy,
	-- genInstantiateTyUS, -- ToDo: ???

	-- PRETTY PRINTING AND FORCING
	pprUniType, pprParendUniType, pprMaybeTy,
	pprTyCon, pprIfaceClass, pprClassOp,
	getTypeString,
	typeMaybeString,
	specMaybeTysSuffix,
	showTyCon,
	showTypeCategory,

	-- MATCHING and COMPARISON
	matchTy, -- UNUSED: matchTys,
	cmpUniTypeMaybeList,

	-- to make this interface self-sufficient....
	TyVar, TyVarTemplate, TyCon, Class, UniType, UniqueSupply,
	IdEnv(..), UniqFM, UnfoldingDetails, PrimKind, TyVarEnv(..),
	TypeEnv(..), Maybe, PprStyle, PrettyRep, Bag
   ) where

IMPORT_Trace		-- ToDo:rm (debugging)

-- internal modules; allowed to see constructors for type things
import Class
import TyVar
import TyCon
import UniType

import AbsPrel		( listTyCon, integerTyCon, charPrimTyCon,
			  intPrimTyCon, wordPrimTyCon, addrPrimTyCon,
			  floatPrimTyCon, doublePrimTyCon,
			  realWorldTyCon
#ifdef DPH
			  , podTyCon
#endif {- Data Parallel Haskell -}
			)
import Bag
import CLabelInfo	( identToC )
import CmdLineOpts	( GlobalSwitch(..) )
import Id		( Id, getIdInfo,
			  getMentionedTyConsAndClassesFromId,
			  getInstantiatedDataConSig,
			  getDataConSig, mkSameSpecCon,
			  DataCon(..)
			)
import IdEnv		-- ( lookupIdEnv, IdEnv )
import IdInfo		( ppIdInfo, boringIdInfo, IdInfo, UnfoldingDetails )
import InstEnv		( ClassInstEnv(..), MatchEnv(..) )
import ListSetOps	( unionLists )
import NameTypes	( FullName )
import Maybes
import Outputable
import Pretty
import PrimKind		( PrimKind(..) )
import SpecTyFuns	( specialiseConstrTys )
import TyVarEnv
import Unique		-- used UniqueSupply monadery
import Util
\end{code}

%************************************************************************
%*									*
\subsection[UniTyFuns-construction]{Putting types together}
%*									*
%************************************************************************

\begin{code}
applyTy :: SigmaType -> SigmaType -> SigmaType

applyTy (UniSyn _ _ fun_ty) arg_ty = applyTy fun_ty arg_ty
applyTy fun_ty@(UniForall tyvar ty) arg_ty
  = instantiateTy [(tyvar,arg_ty)] ty
#ifdef DEBUG
applyTy bad_fun_ty arg_ty
  = pprPanic "applyTy: not a forall type:" (ppAbove (ppr PprDebug bad_fun_ty) (ppr PprDebug arg_ty))
#endif
\end{code}

@applyTyCon@ applies a type constructor to a list of tau-types to give
a type.  @applySynTyCon@ and @applyNonSynTyCon@ are similar, but they
``know'' what sort the type constructor is, so they are a bit lazier.
This is important in @TcMonoType.lhs@.

\begin{code}
applyTyCon, applySynTyCon, applyNonSynTyCon :: TyCon -> [TauType] -> TauType

applyTyCon tc tys
  = ASSERT (if (getTyConArity tc == length tys) then True else pprTrace "applyTyCon" (ppCat [ppr PprDebug tc, ppr PprDebug tys]) False)
    --false:ASSERT (all isTauTy tys) TauType?? 94/06
    let
	result = apply_tycon tc tys
    in
    --false:ASSERT (isTauTy result)  TauType?? 94/06
    result
 where
    apply_tycon tc@(SynonymTyCon _ _ _ _ _ _) tys = applySynTyCon tc tys
    apply_tycon tc@(DataTyCon _ _ _ _ _ _ _)  tys = applyNonSynTyCon tc tys

    apply_tycon tc@(PrimTyCon _ _ _ _) tys        = UniData tc tys

    apply_tycon tc@(TupleTyCon _) tys             = UniData tc tys
      -- The arg types here aren't necessarily tau-types, because we
      -- may have polymorphic methods in a dictionary.

      -- Original tycon used in type of SpecTyCon
    apply_tycon tc_spec@(SpecTyCon tc spec_tys) tys
      =	apply_tycon tc (fill_nothings spec_tys tys)
      where
        fill_nothings (Just ty:maybes) fills      = ty : fill_nothings maybes fills
        fill_nothings (Nothing:maybes) (ty:fills) = ty : fill_nothings maybes fills
	fill_nothings [] [] = []
   
#ifdef DPH
    apply_tycon tc@(ProcessorTyCon _) tys = UniData tc tys
#endif {- Data Parallel Haskell -}


-----------------

applySynTyCon tycon tys
  = UniSyn tycon ok_tys (instantiateTauTy (tyvars `zip` ok_tys) template)
	-- Memo the result of substituting for the tyvars in the template
  where
    SynonymTyCon _ _ _ tyvars template _ = tycon
	-- NB: Matched lazily

#ifdef DEBUG
    ok_tys = map (verifyTauTy "applyTyConLazily[syn]") tys
#else
    ok_tys = tys
#endif

-----------------

applyNonSynTyCon tycon tys	-- We don't expect function tycons;
				-- but it must be lazy, so we can't check that here!
#ifdef DEBUG
  = UniData tycon (map (verifyTauTy "applyTyConLazily[data]") tys)
#else
  = UniData tycon tys
#endif
\end{code}

@glueTyArgs [ty1,...,tyn] ty@ returns the type
@ty1 -> ... -> tyn -> ty@.  This is the exact reverse of @splitTyArgs@.

\begin{code}
-- ToDo: DEBUG: say what's true about these types
glueTyArgs :: [UniType] -> UniType -> UniType

glueTyArgs tys ty = foldr UniFun ty tys
\end{code}

\begin{code}
mkSuperDictSelType :: Class 	-- The input class
		   -> Class	-- The superclass
		   -> UniType	-- The type of the selector function

mkSuperDictSelType clas@(MkClass _ _ tyvar _ _ _ _ _ _ _) super
  = UniForall tyvar (UniFun (UniDict clas  (UniTyVarTemplate tyvar))
			    (UniDict super (UniTyVarTemplate tyvar)))
\end{code}

UNUSED: @mkDictFunType@ creates the type of a dictionary function, given:
the polymorphic type variables, the types of the dict args, the class and
tautype of the result.

\begin{code}
{- UNUSED:
mkDictFunType :: [TyVarTemplate] -> ThetaType -> Class -> TauType -> UniType

mkDictFunType tyvars theta clas tau_ty
#ifndef DEBUG
 = mkForallTy tyvars (foldr f (UniDict clas tau_ty) theta)
#else
 = mkForallTy tyvars (foldr f (UniDict clas (verifyTauTy "mkDictFunType" tau_ty)) theta)
#endif
   where
     f (clas,tau_ty) sofar = UniFun (UniDict clas tau_ty) sofar
-}
\end{code}

\begin{code}
specialiseTy :: UniType		-- The type of the Id of which the SpecId 
				-- is a specialised version
	     -> [Maybe UniType]	-- The types at which it is specialised
	     -> Int		-- Number of leading dictionary args to ignore
	     -> UniType

specialiseTy main_ty maybe_tys dicts_to_ignore
  = --false:ASSERT(isTauTy tau) TauType??
    mkSigmaTy remaining_tyvars 
	      (instantiateThetaTy inst_env remaining_theta)
	      (instantiateTauTy   inst_env tau)
  where
    (tyvars, theta, tau) = splitType main_ty	-- A prefix of, but usually all, 
						-- the theta is discarded!
    remaining_theta      = drop dicts_to_ignore theta
    tyvars_and_maybe_tys = tyvars `zip` maybe_tys
    remaining_tyvars     = [tyvar      | (tyvar, Nothing) <- tyvars_and_maybe_tys]
    inst_env             = [(tyvar,ty) | (tyvar, Just ty) <- tyvars_and_maybe_tys]
\end{code}

%************************************************************************
%*									*
\subsection[UniTyFuns-destruction]{Taking types apart}
%*									*
%************************************************************************

@expandVisibleTySyn@ removes any visible type-synonym from the top level of a
@TauType@. Note that the expansion is recursive.

@expandTySyns@ removes all type-synonyms from a @TauType@.

\begin{code}
expandVisibleTySyn, expandTySyns :: TauType -> TauType

expandVisibleTySyn (UniSyn con _ tau)
  | isVisibleSynTyCon con
  = ASSERT(isTauTy tau)
    expandVisibleTySyn tau
expandVisibleTySyn tau
  = ASSERT(isTauTy tau)
    tau

expandTySyns (UniSyn _ _ tau) = expandTySyns tau
expandTySyns (UniFun a b)     = UniFun (expandTySyns a) (expandTySyns b)
expandTySyns (UniData c tys)  = UniData c (map expandTySyns tys)
expandTySyns tau	      = -- FALSE:WDP 95/03: ASSERT(isTauTy tau)
			        tau
\end{code}

@getTyVar@ extracts a type variable from a @UniType@ if the latter is
just a type variable, failing otherwise.  @getTyVarMaybe@ is similar,
except that it returns a @Maybe@ type.

\begin{code}
getTyVar :: String -> UniType -> TyVar
getTyVar panic_msg (UniTyVar tyvar) = tyvar
getTyVar panic_msg other    	    = panic ("getTyVar: " ++ panic_msg)

getTyVarMaybe :: UniType -> Maybe TyVar
getTyVarMaybe (UniTyVar tyvar)	= Just tyvar
getTyVarMaybe (UniSyn _ _ exp)	= getTyVarMaybe exp
getTyVarMaybe other		= Nothing

getTyVarTemplateMaybe :: UniType -> Maybe TyVarTemplate
getTyVarTemplateMaybe (UniTyVarTemplate tyvar)	= Just tyvar
getTyVarTemplateMaybe (UniSyn _ _ exp)		= getTyVarTemplateMaybe exp
getTyVarTemplateMaybe other			= Nothing
\end{code}

@splitType@ splits a type into three components. The first is the
bound type variables, the second is the context and the third is the
tau type. I'll produce specific functions which access particular pieces
of the type when we see where they are needed.

\begin{code}
splitType :: UniType -> ([TyVarTemplate], ThetaType, TauType)
splitType uni_ty
  = case (split_foralls uni_ty)	of { (tyvars, rho_ty) ->
    case (split_rho_ty rho_ty)	of { (theta_ty, tau_ty) ->
    --false:ASSERT(isTauTy tau_ty) TauType
    (tyvars, theta_ty, tau_ty)
    }}
  where
    split_foralls (UniForall tyvar uni_ty)
      = case (split_foralls uni_ty) of { (tyvars,new_ty) ->
	(tyvar:tyvars, new_ty) }

    split_foralls other_ty = ([], other_ty)

    split_rho_ty (UniFun (UniDict clas ty) ty_body)
      = case (split_rho_ty ty_body)	of { (context,ty_body') ->
	((clas, ty) :context, ty_body') }

    split_rho_ty other_ty = ([], other_ty)
\end{code}

Sometimes we want the dictionaries counted as arguments.  We guarantee
to return {\em some} arguments if there are any, but not necessarily
{\em all}.  In particular, the ``result type'' might be a @UniDict@,
which might (in the case of a single-classop class) be a function.  In
that case, we strongly avoid returning a @UniDict@ ``in the corner''
(by @unDictify@ing that type, too).

This seems like a bit of a fudge, frankly, but it does the job.

\begin{code}
splitTypeWithDictsAsArgs
	:: UniType		-- input
	-> ([TyVarTemplate],
	    [UniType],		-- arg types
	    TauType)		-- result type

splitTypeWithDictsAsArgs ty
  = case (splitType ty)		of { (tvs, theta, tau_ty) ->
    case (splitTyArgs tau_ty)	of { (tau_arg_tys, res_ty) ->
    let
	result extra_arg_tys res_ty
	  = --false: ASSERT(isTauTy res_ty) TauType
	    (tvs,
	     [ mkDictTy c t | (c,t) <- theta ] ++ tau_arg_tys ++ extra_arg_tys,
	     res_ty)
    in
    if not (isDictTy res_ty) then
	result [] res_ty
    else
	let
	    undicted_res_ty 	    = unDictifyTy res_ty
	    (tau_arg_tys', res_ty') = splitTyArgs undicted_res_ty
	in
	if (null theta && null tau_arg_tys)
	|| isFunType undicted_res_ty then

	    -- (a) The input ty was just a "dictionary" for a
	    -- single-method class with no super-dicts; the
	    -- "dictionary" is just the one method itself; we'd really
	    -- rather give info about that method...

	    -- (b) The input ty gave back a "dictionary" for a
	    -- single-method class; if the method itself is a
	    -- function, then we'd jolly well better add its arguments
	    -- onto the whole "arg_tys" list.
	    
	    -- There may be excessive paranoia going on here (WDP).

	    result tau_arg_tys' res_ty'

	else -- do nothing special...
	    result [] res_ty
    }}
\end{code}

@splitForalls@ is similar, but only splits off the forall'd type
variables.
  
\begin{code}
splitForalls :: UniType -> ([TyVarTemplate], RhoType)

splitForalls (UniForall tyvar ty)
  = case (splitForalls ty) of
      (tyvars, new_ty) -> (tyvar:tyvars, new_ty)
splitForalls (UniSyn _ _ ty)	= splitForalls ty
splitForalls other_ty		= ([], other_ty)
\end{code}

And a terribly convenient way to access @splitType@:

\begin{code}
getTauType :: UniType -> TauType
getTauType uni_ty
  = case (splitType uni_ty) of { (_,_,tau_ty) ->
    --false:ASSERT(isTauTy tau_ty) TauType??? (triggered in ProfMassage)
    tau_ty }
\end{code}

@splitTyArgs@ does the same for the arguments of a function type.

\begin{code}
splitTyArgs :: TauType -> ([TauType], TauType)

splitTyArgs ty
  = --false: ASSERT(isTauTy ty) TauType???
    split ty
  where
    split (UniSyn _ _ expand) = split expand

    split (UniFun arg result)
     = case (split result) of { (args, result') ->
       (arg:args, result') }

    split ty = ([], ty)

funResultTy :: RhoType 		-- Function type
	    -> Int		-- Number of args to which applied
	    -> RhoType		-- Result type

funResultTy ty 			 0 	= ty
funResultTy (UniSyn _ _ expand)  n_args = funResultTy expand n_args
funResultTy ty@(UniDict _ _)     n_args = funResultTy (unDictifyTy ty) n_args
funResultTy (UniFun _ result_ty) n_args = funResultTy result_ty (n_args - 1)
#ifdef DEBUG
funResultTy other_ty 		 n_args = panic ("funResultTy:not a fun:"++(ppShow 80 (ppr PprDebug other_ty)))
#endif
\end{code}

The type-destructor functions above return dictionary information in
terms of @UniDict@, a relatively abstract construct.  What really
happens ``under the hood'' is that {\em tuples} (usually) are passed
around as ordinary arguments.  Sometimes we want this ``what's really
happening'' information.

The interesting case for @getUniDataTyCon_maybe@ is if the argument is
a dictionary type.  Dictionaries are represented by tuples (except for
size-one dictionaries which are represented by the method itself), so
@getUniDataTyCon_maybe@ has to figure out which tuple.  This is a bit
unsatisfactory; the information about how dictionaries are represented
is rather thinly distributed.

@unDictify@ only removes a {\em top-level} @UniDict@.  There may be
buried @UniDicts@ in what is returned.

\begin{code}
unDictifyTy :: UniType 		-- Might be a UniDict
	    -> UniType		-- Can't be a UniDict

unDictifyTy (UniSyn _ _ expansion)  = unDictifyTy expansion

unDictifyTy (UniDict clas ty)
  = ASSERT(dict_size >= 0)
    if dict_size == 1 then
	unDictifyTy (head all_arg_tys)	-- just the <whatever> itself
		-- The extra unDictify is to make sure that
		-- the result isn't still a dict, which it might be
		-- if the original guy was a dict with one superdict and
		-- no methods!
    else
	UniData (mkTupleTyCon dict_size) all_arg_tys -- a tuple of 'em
	-- NB: dict_size can be 0 if the class is
	-- _CCallable, _CReturnable (and anything else
	-- *really weird* that the user writes).
  where
    (tyvar, super_classes, ops) = getClassSig clas
    dict_size = length super_classes + length ops

    super_dict_tys 	= map mk_super_ty super_classes
    class_op_tys	= map mk_op_ty    ops

    all_arg_tys 	= super_dict_tys ++ class_op_tys

    mk_super_ty sc = mkDictTy sc ty
    mk_op_ty	op = instantiateTy [(tyvar,ty)] (getClassOpLocalType op)

unDictifyTy other_ty = other_ty
\end{code}

\begin{code}
{- UNUSED:
sourceTypes :: TauType -> [TauType]
sourceTypes ty
  = --false:ASSERT(isTauTy ty)
    (fst . splitTyArgs) ty

targetType :: TauType -> TauType
targetType ty
  = --false: ASSERT(isTauTy ty) TauType??
    (snd . splitTyArgs) ty
-}
\end{code}

Here is a function that tell you if a type has as its target a Synonym.
If so it returns the relevant constructor and its argument type.

\begin{code}
{- UNUSED:
isSynTarget :: UniType -> Maybe (TyCon,Int)

isSynTarget (UniFun _ arg)	 = case isSynTarget arg of
				     Just (tycon,x) -> Just (tycon,x + 1)
				     Nothing -> Nothing
isSynTarget (UniSyn tycon _ _)   = Just (tycon,0)
isSynTarget (UniForall _ e) 	 = isSynTarget e
isSynTarget _			 = Nothing
--isSynTarget (UniTyVarTemplate e) = panic "isSynTarget: got a UniTyVarTemplate!"
-}
\end{code}

\begin{code}
splitDictType :: UniType -> (Class, UniType)
splitDictType (UniDict clas ty) = (clas, ty)
splitDictType (UniSyn _ _ ty)   = splitDictType ty
splitDictType other             = panic "splitDictTy"
\end{code}

In @kindFromType@ it can happen that we come across a @TyVarTemplate@,
for example when figuring out the kinds of the argument of a data
constructor; inside the @DataCon@ the argument types are in template form.

\begin{code}
kindFromType :: UniType -> PrimKind
kindFromType (UniSyn tycon tys expand)  = kindFromType expand
kindFromType (UniData tycon tys)        = getTyConKind tycon (map kindFromType tys)
kindFromType other		        = PtrKind	-- the "default"

isPrimType :: UniType -> Bool

isPrimType (UniSyn tycon tys expand)  = isPrimType expand
#ifdef DPH
isPrimType (UniData tycon tys) | isPodizedPodTyCon tycon       
  = all isPrimType tys
#endif {- Data Parallel Haskell}
isPrimType (UniData tycon tys)        = isPrimTyCon tycon
isPrimType other		      = False		-- the "default"

maybeBoxedPrimType :: UniType -> Maybe (Id{-DataCon-}, UniType)

maybeBoxedPrimType ty
  = case (getUniDataTyCon_maybe ty) of	    -- Data type,
      Just (tycon, tys_applied, [data_con]) -- with exactly one constructor
        -> case (getInstantiatedDataConSig data_con tys_applied) of
	     (_, [data_con_arg_ty], _)	    -- Applied to exactly one type,
	       | isPrimType data_con_arg_ty -- which is primitive
	       -> Just (data_con, data_con_arg_ty)
	     other_cases -> Nothing
      other_cases -> Nothing
\end{code}

At present there are no unboxed non-primitive types, so
isUnboxedDataType is the same as isPrimType.

\begin{code}
isUnboxedDataType :: UniType -> Bool

isUnboxedDataType (UniSyn _ _ expand) = isUnboxedDataType expand
isUnboxedDataType (UniData tycon _)   = not (isBoxedTyCon tycon)
isUnboxedDataType other		      = False
\end{code}

If you want to run @getUniDataTyCon...@ or @UniDataArgTys@ over a
dictionary-full type, then put the type through @unDictifyTy@ first.

\begin{code}
getUniDataTyCon_maybe
	:: TauType
	-> Maybe (TyCon,	-- the type constructor
		  [TauType],	-- types to which it is applied
		  [Id])		-- its family of data-constructors

getUniDataTyCon_maybe ty
  = --false:ASSERT(isTauTy ty) TauType?
    get ty
  where
    get (UniSyn _ _ expand) = get expand
    get ty@(UniDict _ _)    = get (unDictifyTy ty)

    get (UniData tycon arg_tys)
      = Just (tycon, arg_tys, getTyConDataCons tycon)
	-- does not returned specialised data constructors

    get other_ty = Nothing
\end{code}

@getUniDataTyCon@ is just a version which fails noisily.
\begin{code}
getUniDataTyCon ty
  = case getUniDataTyCon_maybe ty of
      Just stuff -> stuff
#ifdef DEBUG
      Nothing    -> pprPanic "getUniDataTyCon:" (ppr PprShowAll ty)
#endif
\end{code}

@getUniDataSpecTyCon_maybe@ returns an appropriate specialised tycon,
any remaining (boxed) type arguments, and specialsied constructors.
\begin{code}
getUniDataSpecTyCon_maybe
	:: TauType
	-> Maybe (TyCon,	-- the type constructor
		  [TauType],	-- types to which it is applied
		  [Id])		-- its family of data-constructors

getUniDataSpecTyCon_maybe ty
  = case getUniDataTyCon_maybe ty of
      Nothing -> Nothing
      Just unspec@(tycon, tycon_arg_tys, datacons) ->
	let spec_tys  = specialiseConstrTys tycon_arg_tys
	    spec_reqd = maybeToBool (firstJust spec_tys)

	    data_cons = getTyConDataCons tycon
	    spec_datacons = map (mkSameSpecCon spec_tys) data_cons
	    spec_tycon = mkSpecTyCon tycon spec_tys

	    tys_left = [ty | (spec, ty) <- spec_tys `zip` tycon_arg_tys,
		             not (maybeToBool spec) ]
        in
  	    if spec_reqd
	    then Just (spec_tycon, tys_left, spec_datacons)
	    else Just unspec
\end{code}

@getUniDataSpecTyCon@ is just a version which fails noisily.
\begin{code}
getUniDataSpecTyCon ty
  = case getUniDataSpecTyCon_maybe ty of
      Just stuff -> stuff
      Nothing    -> panic ("getUniDataSpecTyCon:"++ (ppShow 80 (ppr PprShowAll ty)))
\end{code}

@getMentionedTyCons@ maps a type constructor to a list of type
constructors.  If the type constructor is built-in or a @data@ type
constructor, the list is empty.  In the case of synonyms, list
contains all the type {\em synonym} constructors {\em directly}
mentioned in the definition of the synonym.
\begin{code}
getMentionedTyCons :: TyCon -> [TyCon]

getMentionedTyCons (SynonymTyCon _ _ _ _ expansion _) = get_ty_cons expansion
  where
    get_ty_cons (UniTyVar _)        = []
    get_ty_cons (UniTyVarTemplate _)= []
    get_ty_cons (UniData _ tys)     = concat (map get_ty_cons tys)
    get_ty_cons (UniFun ty1 ty2)    = get_ty_cons ty1 ++ get_ty_cons ty2
    get_ty_cons (UniSyn tycon _ _)  = [tycon]
    get_ty_cons _ = panic "get_ty_cons: unexpected UniType"

getMentionedTyCons other_tycon = []
\end{code}

Here's a similar thing used in the Semantique strictness analyser:
\begin{code}
#ifdef USE_SEMANTIQUE_STRANAL
getReferredToTyCons :: TauType -> [TyCon]
getReferredToTyCons (UniTyVar v)   = []
getReferredToTyCons (UniTyVarTemplate v)   = []
getReferredToTyCons (UniData t ts) = t : concat (map getReferredToTyCons ts)
getReferredToTyCons (UniFun s t)   = getReferredToTyCons s ++ getReferredToTyCons t
getReferredToTyCons (UniSyn _ _ t) = getReferredToTyCons (getTauType t)
getReferredToTyCons other	   = panic "getReferredToTyCons: not TauType"
#endif {- Semantique strictness analyser -}
\end{code}

This @getMentioned*@ code is for doing interfaces.  Tricky point: we
{\em always} expand synonyms in interfaces, so note the handling of
@UniSyns@.
\begin{code}
getMentionedTyConsAndClassesFromUniType :: UniType -> (Bag TyCon, Bag Class)

getMentionedTyConsAndClassesFromUniType (UniTyVar _)	     = (emptyBag, emptyBag)
getMentionedTyConsAndClassesFromUniType (UniTyVarTemplate _) = (emptyBag, emptyBag)

getMentionedTyConsAndClassesFromUniType (UniData tycon arg_tys)
  = foldr do_arg_ty (unitBag tycon, emptyBag) arg_tys
  where
    do_arg_ty ty (ts_sofar, cs_sofar)
      = case (getMentionedTyConsAndClassesFromUniType ty) of { (ts, cs) ->
    	(ts `unionBags` ts_sofar, cs `unionBags` cs_sofar) }

getMentionedTyConsAndClassesFromUniType (UniFun ty1 ty2)   
  = case (getMentionedTyConsAndClassesFromUniType ty1) of { (ts1, cs1) ->
    case (getMentionedTyConsAndClassesFromUniType ty2) of { (ts2, cs2) ->
    (ts1 `unionBags` ts2, cs1 `unionBags` cs2) }}
   
getMentionedTyConsAndClassesFromUniType (UniSyn tycon _ expansion) 
 = getMentionedTyConsAndClassesFromUniType expansion
   -- if synonyms were not expanded: (unitBag tycon, emptyBag)

getMentionedTyConsAndClassesFromUniType (UniDict clas ty)
  = case (getMentionedTyConsAndClassesFromUniType ty) of { (ts, cs) ->
    (ts, cs `snocBag` clas) }

getMentionedTyConsAndClassesFromUniType (UniForall _ ty)
  = getMentionedTyConsAndClassesFromUniType ty
\end{code}

This code could go in @TyCon@, but it's better to keep all the
``getMentioning'' together.
\begin{code}
getMentionedTyConsAndClassesFromTyCon :: TyCon -> (Bag TyCon, Bag Class)

getMentionedTyConsAndClassesFromTyCon tycon@(SynonymTyCon _ _ _ _ ty _)
  = case (getMentionedTyConsAndClassesFromUniType ty) of { (ts, cs) ->
    (ts `snocBag` tycon, cs) }

getMentionedTyConsAndClassesFromTyCon tycon@(DataTyCon _ _ _ _ constructors _ _)
  = foldr do_con (unitBag tycon, emptyBag) constructors
    -- We don't worry whether this TyCon is exported abstractly
    -- or not, because even if so, the pragmas probably need
    -- to know this info.
  where
    do_con con (ts_sofar, cs_sofar)
      = case (getMentionedTyConsAndClassesFromId con) of { (ts, cs) ->
    	(ts `unionBags` ts_sofar, cs `unionBags` cs_sofar) }

getMentionedTyConsAndClassesFromTyCon other
 = panic "tried to get mentioned tycons and classes from funny tycon"
\end{code}

\begin{code}
getMentionedTyConsAndClassesFromClass :: Class -> (Bag TyCon, Bag Class)

getMentionedTyConsAndClassesFromClass clas@(MkClass _ _ _ super_classes _ ops _ _ _ _)
  = foldr do_op
	  (emptyBag, unitBag clas `unionBags` listToBag super_classes)
	  ops
  where
    do_op (MkClassOp _ _ ty) (ts_sofar, cs_sofar)
      = case (getMentionedTyConsAndClassesFromUniType ty) of { (ts, cs) ->
    	(ts `unionBags` ts_sofar, cs `unionBags` cs_sofar) }
\end{code}

Grab a name for the type. This is used to determine the type
description for profiling.
\begin{code}
getUniTyDescription :: UniType -> String
getUniTyDescription ty
  = case (getTauType ty) of
      UniFun arg res    -> '-' : '>' : fun_result res
      UniData tycon _   -> _UNPK_ (getOccurrenceName tycon)
      UniSyn tycon _ _  -> _UNPK_ (getOccurrenceName tycon)
      UniDict cls uni   -> "dict"			   -- Or from unitype ?
      UniTyVar _	-> "*"			   	   -- Distinguish ?
      UniTyVarTemplate _-> "*"
      _			-> panic "getUniTyName: other"

  where
    fun_result (UniFun _ res) = '>' : fun_result res
    fun_result other	      = getUniTyDescription other

\end{code}
  
%************************************************************************
%*									*
\subsection[UniTyFuns-fvs]{Extracting free type variables}
%*									*
%************************************************************************

@extractTyVarsFromTy@ gets the free type variables from a @UniType@.
The list returned has no duplicates.

\begin{code}
extractTyVarsFromTys :: [UniType] -> [TyVar]
extractTyVarsFromTys = foldr (unionLists . extractTyVarsFromTy) []

extractTyVarsFromTy :: UniType -> [TyVar]
extractTyVarsFromTy ty
  = get ty []
  where
    -- weird arg order so we can foldr easily
    get (UniTyVar tyvar) free
	| tyvar `is_elem` free     = free
	| otherwise                = tyvar:free
    get (UniTyVarTemplate _)  free = free
    get (UniFun ty1 ty2)      free = get ty1 (get ty2 free)
    get (UniData tycon tys)   free = foldr get free tys
    get (UniSyn tycon tys ty) free = foldr get free tys
    get (UniDict clas ty)     free = get ty free
    get (UniForall tyvar ty)  free = get ty free

    is_elem = isIn "extractTyVarsFromTy"
\end{code}

\begin{code}
extractTyVarTemplatesFromTy :: UniType -> [TyVarTemplate]
extractTyVarTemplatesFromTy ty
  = get ty []
  where
    get (UniTyVarTemplate tyvar) free
	| tyvar `is_elem` free     = free
	| otherwise                = tyvar:free
    get (UniTyVar tyvar)      free = free
    get (UniFun ty1 ty2)      free = get ty1 (get ty2 free)
    get (UniData tycon tys)   free = foldr get free tys
    get (UniSyn tycon tys ty) free = foldr get free tys
    get (UniDict clas ty)     free = get ty free
    get (UniForall tyvar ty)  free = get ty free

    is_elem = isIn "extractTyVarTemplatesFromTy"
\end{code}

%************************************************************************
%*									*
\subsection[UniTyFuns-predicates]{Predicates (and such) on @UniTypes@}
%*									*
%************************************************************************

We include functions that return @Maybe@ thingies as ``predicates.''

\begin{code}
isTyVarTy :: UniType -> Bool
isTyVarTy (UniTyVar _)	      = True
isTyVarTy (UniSyn _ _ expand) = isTyVarTy expand
isTyVarTy other		      = False

-- isTyVarTemplateTy only used in Renamer for error checking
isTyVarTemplateTy :: UniType -> Bool
isTyVarTemplateTy (UniTyVarTemplate tv) = True
isTyVarTemplateTy (UniSyn _ _ expand)   = isTyVarTemplateTy expand
isTyVarTemplateTy other 		= False

maybeUnpackFunTy :: TauType -> Maybe (TauType, TauType)

maybeUnpackFunTy ty
  = --false: ASSERT(isTauTy ty) TauType??
    maybe ty
  where
    maybe (UniSyn _ _ expand) = maybe expand
    maybe (UniFun arg result) = Just (arg, result)
    maybe ty@(UniDict _ _)    = maybe (unDictifyTy ty)
    maybe other		      = Nothing

isFunType :: TauType -> Bool
isFunType ty
  = --false: ASSERT(isTauTy ty) TauType???
    maybeToBool (maybeUnpackFunTy ty)
\end{code}

\begin{code}
{- UNUSED:
isDataConType :: TauType -> Bool

isDataConType ty
  = ASSERT(isTauTy ty)
    is_con_ty ty
  where
    is_con_ty (UniData _ _)       = True
    is_con_ty (UniSyn _ _ expand) = is_con_ty expand
    is_con_ty _			  = False
-}
\end{code}

SIMON'S NOTES:

leakFree (UniData (DataTyCon ...) tys) 
  = nonrecursive type &&
    all leakFree (apply constructors to tys)

leakFree (PrimTyCon...) = True

leakFree (TyVar _) = False
leakFree (UniFun _ _) = False

non-recursive: enumeration types, tuples, primitive types...

END NOTES

The list of @TyCons@ is ones we have already seen (and mustn't see
again).

\begin{code}
isLeakFreeType :: [TyCon] -> UniType -> Bool

isLeakFreeType seen (UniSyn _ _ expand) = isLeakFreeType seen expand

isLeakFreeType _ (UniTyVar _) 	      = False	-- Utterly unknown
isLeakFreeType _ (UniTyVarTemplate _) = False

isLeakFreeType _ (UniFun _ _) = False	-- Could have leaky free variables

isLeakFreeType _ ty@(UniDict _ _) = True -- I'm prepared to bet that
					-- we'll never get a space leak
					-- from a dictionary.  But I could 
					-- be wrong... SLPJ

isLeakFreeType seen (UniForall _ ty) = isLeakFreeType seen ty

-- For a data type we must look at all the argument types of all
-- the constructors.  It isn't enough to look merely at the
-- types to which the type constructor is applied. For example
--
--	data Foo a = MkFoo [a]
--
-- Is (Foo Int) leak free?  No!

isLeakFreeType seen (UniData tycon tycon_arg_tys)
  | tycon `is_elem` seen = False	-- Recursive type!  Bale out!

  | isDataTyCon tycon = all data_con_args_leak_free (getTyConDataCons tycon)

  | otherwise	      = isPrimTyCon tycon && -- was an assert; now just paranoia
			-- We should have a leak-free-ness predicate on PrimTyCons,
			-- but that's too big a change for today, so we hack it.
			-- Return true iff it's one of the tycons we know are leak-free
			-- 94/10: I hope I don't live to regret taking out
			-- the first check...
			{-(tycon `elem` [
			    charPrimTyCon, intPrimTyCon, wordPrimTyCon,
			    addrPrimTyCon, floatPrimTyCon, doublePrimTyCon,
			    byteArrayPrimTyCon, arrayPrimTyCon,
			    mallocPtrPrimTyCon, stablePtrPrimTyCon
			    -- List almost surely incomplete!
			   ])
			&&-} (all (isLeakFreeType (tycon:seen)) tycon_arg_tys)
  where
    data_con_args_leak_free data_con
      = case (getInstantiatedDataConSig data_con tycon_arg_tys) of { (_,arg_tys,_) ->
	all (isLeakFreeType (tycon:seen)) arg_tys }

    is_elem = isIn "isLeakFreeType"
\end{code}

\begin{code}
{- UNUSED:
hasHigherOrderArg :: UniType -> Bool
hasHigherOrderArg ty
  = case (splitType   ty)   	of { (_, _, tau_ty) ->
    case (splitTyArgs tau_ty)	of { (arg_tys, _) ->

    foldr ((||) . isFunType . expandTySyns) False arg_tys
    }}
-}
\end{code}

\begin{code}
isDictTy :: UniType -> Bool

isDictTy (UniDict _ _)	     = True
isDictTy (UniSyn _ _ expand) = isDictTy expand
isDictTy _		     = False

isTauTy :: UniType -> Bool

isTauTy (UniTyVar v)     = True
isTauTy (UniFun  a b)    = isTauTy a && isTauTy b
isTauTy (UniData _ tys)  = all isTauTy tys
isTauTy (UniSyn _ _ ty)  = isTauTy ty
isTauTy (UniDict _ ty)   = False
isTauTy (UniTyVarTemplate _) = False
isTauTy (UniForall _ _) = False

isForAllTy :: UniType -> Bool
isForAllTy (UniForall _ _) = True
isForAllTy (UniSyn _ _ ty) = isForAllTy ty
isForAllTy _		   = False
\end{code}

NOTE: I haven't thought about this much (ToDo: check).
\begin{code}
isGroundOrTyVarTy, isGroundTy :: UniType -> Bool

isGroundOrTyVarTy ty = isGroundTy ty || isTyVarTy ty

isGroundTy (UniTyVar tyvar)	 = False
isGroundTy (UniTyVarTemplate _)	 = False
isGroundTy (UniFun ty1 ty2)	 = isGroundTy ty1 && isGroundTy ty2
isGroundTy (UniData tycon tys)	 = all isGroundTy tys
isGroundTy (UniSyn _ _ exp)      = isGroundTy exp
isGroundTy (UniDict clas ty)	 = isGroundTy ty
isGroundTy (UniForall tyvar ty)	 = False		-- Safe for the moment
\end{code}

Broadly speaking, instances are exported (a)~if {\em either} the class
or {\em OUTERMOST} tycon [arbitrary...] is exported; or (b)~{\em both}
class and tycon are from PreludeCore [non-std, but convenient] {\em
and} the instance was defined in this module.  BUT: if either the
class or tycon was defined in this module, but not exported, then
there is no point exporting the instance.

\begin{code}
instanceIsExported
	:: Class -> TauType	-- class/"tycon" defining instance
	-> Bool			-- True <=> instance decl in this module
	-> Bool

instanceIsExported clas ty from_here
  = --false:ASSERT(isTauTy ty) TauType?? failed compiling IArray
    if is_core_class then
	if is_fun_tycon || is_core_tycon then
	   {-if-} from_here
	else
	   is_exported_tycon
	   || (is_imported_tycon && from_here) -- V NAUGHTY BY HASKELL RULES

    else if is_fun_tycon || is_core_tycon then
	-- non-Core class; depends on its export flag
	is_exported_class
	|| (is_imported_class && from_here) -- V NAUGHTY BY HASKELL RULES

    else -- non-Core class & non-Core tycon:
	 -- exported if one of them is, but not if either of them
	 -- is locally-defined *and* not exported
	if  (isLocallyDefined clas  && not is_exported_class)
	 || (isLocallyDefined tycon && not is_exported_tycon) then
	    False
    	else
	    is_exported_class || is_exported_tycon
  where
    tycon = case getUniDataTyCon_maybe ty of
	      Just (xx,_,_) -> xx
	      Nothing 	    -> panic "instanceIsExported:no tycon"

    is_core_class = fromPreludeCore clas
    is_core_tycon = fromPreludeCore tycon

    is_fun_tycon = isFunType ty

    is_exported_class = case (getExportFlag clas) of
			  NotExported -> False
			  _ 	      -> True

    is_exported_tycon = case (getExportFlag tycon) of
			  NotExported -> False
			  _ 	      -> True

    is_imported_class = not (isLocallyDefined clas)
    is_imported_tycon = not (isLocallyDefined tycon)
\end{code}

\begin{code}
maybePurelyLocalTyCon :: TyCon   -> Maybe [Pretty]
maybePurelyLocalClass :: Class   -> Maybe [Pretty]
maybePurelyLocalType  :: UniType -> Maybe [Pretty]

purely_local tc -- overloaded
  = if (isLocallyDefined tc && not (isExported tc))
    then Just (ppr PprForUser tc)
    else Nothing

--overloaded: merge_maybes :: (a -> Maybe b) -> [a] -> Maybe [b]

merge_maybes f xs
  = case (catMaybes (map f xs)) of
      [] -> Nothing   -- no hit anywhere along the list
      xs -> Just xs

maybePurelyLocalTyCon tycon
  = let
	mentioned_tycons = fst (getMentionedTyConsAndClassesFromTyCon tycon)
	-- will include tycon itself
    in
    merge_maybes purely_local (bagToList mentioned_tycons)

maybePurelyLocalClass clas
  = let
	(mentioned_classes, mentioned_tycons)
	  = getMentionedTyConsAndClassesFromClass clas
	  -- will include clas itself

	tc_stuff = merge_maybes purely_local (bagToList mentioned_tycons)
	cl_stuff = merge_maybes purely_local (bagToList mentioned_classes)
    in
    case (tc_stuff, cl_stuff) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just xs) -> Just xs
      (Just xs, Nothing) -> Just xs
      (Just xs, Just ys) -> Just (xs ++ ys)

maybePurelyLocalType ty
  = let
	(mentioned_classes, mentioned_tycons)
	  = getMentionedTyConsAndClassesFromUniType ty
	  -- will include ty itself

	tc_stuff = merge_maybes purely_local (bagToList mentioned_tycons)
	cl_stuff = merge_maybes purely_local (bagToList mentioned_classes)
    in
    case (tc_stuff, cl_stuff) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just xs) -> Just xs
      (Just xs, Nothing) -> Just xs
      (Just xs, Just ys) -> Just (xs ++ ys)
\end{code}

A gigantic HACK due to Simon (95/05)
\begin{code}
returnsRealWorld :: UniType -> Bool

returnsRealWorld (UniTyVar _)	      = False
returnsRealWorld (UniTyVarTemplate _) = False
returnsRealWorld (UniSyn _ _ exp)     = returnsRealWorld exp
returnsRealWorld (UniDict _ ty)	      = returnsRealWorld ty
returnsRealWorld (UniForall _ ty)     = returnsRealWorld ty
returnsRealWorld (UniFun ty1 ty2)     = returnsRealWorld ty2

returnsRealWorld (UniData tycon [])   =	tycon == realWorldTyCon
returnsRealWorld (UniData tycon tys)  = any returnsRealWorld tys
\end{code}

\begin{code}
#ifdef DPH
isProcessorTy :: UniType -> Bool
isProcessorTy (UniData tycon _) = isProcessorTyCon tycon
isProcessorTy _		        = False
#endif {- Data Parallel Haskell -}
\end{code}

Podization of a function @f@ is the compile time specialisation of @f@
to a form that is equivalent to (map.f) . We can podize {\em some}
functions at runtime because of the laws concerning map and functional
composition:
\begin{verbatim}
	map (f . g) == (map f) . (map g) etc...
\end{verbatim}
i.e If we compose two functions, to create a {\em new} function, then
we can compose the podized versions in just the same way. There is a
problem however (as always :-(; We cannot convert between an vanilla
function, and the podized form (and visa versa) at run-time. The
predicate below describes the set of all objects that cannot be
podized at runtime (i.e anything that has a function in it).
\begin{code}
#ifdef DPH
runtimeUnpodizableType:: UniType -> Bool
runtimeUnpodizableType (UniDict _ _)    = True
runtimeUnpodizableType (UniFun _ _)     = True
runtimeUnpodizableType (UniData _ tys)  = any runtimeUnpodizableType tys
runtimeUnpodizableType (UniSyn _ _ ty)  = runtimeUnpodizableType ty
runtimeUnpodizableType other            = False
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[UniTyFuns-subst]{Substitute in a type}
%*									*
%************************************************************************

The idea here is to substitute for the TyVars in a type.  Note, not
the TyVarTemplates---that's the job of instantiateTy.
  
There is a single general function, and two interfaces.

\subsubsection{Interface 1: substitutions}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NOTE: This has been moved to @Subst@ (mostly for speed reasons).

\subsubsection{Interface 2: Envs}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
applyTypeEnvToTy :: TypeEnv -> SigmaType -> SigmaType
applyTypeEnvToTy tenv ty 
  = mapOverTyVars v_fn ty
  where
    v_fn v = case (lookupTyVarEnv tenv v) of
		Just ty -> ty
		Nothing -> UniTyVar v

applyTypeEnvToTauTy :: TypeEnv -> TauType -> TauType
applyTypeEnvToTauTy e ty
  = ASSERT(isTauTy ty)
    applyTypeEnvToTy e ty

applyTypeEnvToThetaTy tenv theta
  = [(clas,
      ASSERT(isTauTy ty)
      applyTypeEnvToTauTy tenv ty) | (clas, ty) <- theta]
\end{code}

\subsubsection{@mapOverTyVars@: does the real work}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@mapOverTyVars@ is a local function which actually does the work.  It does
no cloning or other checks for shadowing, so be careful when calling
this on types with Foralls in them.

\begin{code}
mapOverTyVars :: (TyVar -> UniType) -> UniType -> UniType
mapOverTyVars v_fn (UniTyVar v) 	= v_fn v
mapOverTyVars v_fn (UniFun t1 t2)       = UniFun (mapOverTyVars v_fn t1) (mapOverTyVars v_fn t2)
mapOverTyVars v_fn (UniData con args)   = UniData con (map (mapOverTyVars v_fn) args)
mapOverTyVars v_fn (UniSyn con args ty) = UniSyn con (map (mapOverTyVars v_fn) args) (mapOverTyVars v_fn ty)
mapOverTyVars v_fn (UniDict clas ty)    = UniDict clas (mapOverTyVars v_fn ty)
mapOverTyVars v_fn (UniForall v ty)     = UniForall v (mapOverTyVars v_fn ty)
mapOverTyVars v_fn (UniTyVarTemplate v) = UniTyVarTemplate v
\end{code}

%************************************************************************
%*									*
\subsection[UniTyFuns-ppr]{Pretty-printing @UniTypes@}
%*									*
%************************************************************************

@pprUniType@ is the std @UniType@ printer; the overloaded @ppr@
function is defined to use this.  @pprParendUniType@ is the same,
except it puts parens around the type, except for the atomic cases.
@pprParendUniType@ works just by setting the initial context
precedence very high.  ToDo: what if not a @TauType@?
\begin{code}
pprUniType, pprParendUniType :: PprStyle -> UniType -> Pretty

pprUniType  	 sty ty = ppr_ty_init sty tOP_PREC   ty
pprParendUniType sty ty = ppr_ty_init sty tYCON_PREC ty

pprMaybeTy :: PprStyle -> Maybe UniType -> Pretty
pprMaybeTy PprDebug Nothing   = ppStr "*"
pprMaybeTy PprDebug (Just ty) = pprParendUniType PprDebug ty

getTypeString :: UniType -> [FAST_STRING]
    -- shallowly magical; converts a type into something
    -- vaguely close to what can be used in C identifier.
    -- Don't forget to include the module name!!!

getTypeString ty
  = let
	ppr_t  = ppr_ty PprForUser (\t -> ppStr "*") tOP_PREC (expandTySyns ty)

	string = _PK_ (tidy (ppShow 1000 ppr_t))
    in
    if is_prelude_ty
    then [string]
    else [mod, string]
  where
    (is_prelude_ty, mod)
      = case getUniDataTyCon_maybe ty of
	  Nothing -> true_bottom
	  Just (tycon,_,_) ->
	    if fromPreludeCore tycon
	    then true_bottom
	    else (False, fst (getOrigName tycon))
    
    true_bottom = (True, panic "getTypeString")

    --------------------------------------------------
    -- tidy: very ad-hoc
    tidy [] = [] -- done

    tidy (' ' : more)
      = case more of
	  ' ' : _	 -> tidy more
	  '-' : '>' : xs -> '-' : '>' : tidy (no_leading_sps xs)
	  other	    	 -> ' ' : tidy more

    tidy (',' : more) = ',' : tidy (no_leading_sps more)

    tidy (x : xs) = x : tidy xs  -- catch all

    no_leading_sps [] = []
    no_leading_sps (' ':xs) = no_leading_sps xs
    no_leading_sps other = other

typeMaybeString :: Maybe UniType -> [FAST_STRING]
typeMaybeString Nothing  = [SLIT("!")]
typeMaybeString (Just t) = getTypeString t

specMaybeTysSuffix :: [Maybe UniType] -> FAST_STRING
specMaybeTysSuffix ty_maybes
  = let
	ty_strs  = concat (map typeMaybeString ty_maybes)
	dotted_tys = [ _CONS_ '.' str | str <- ty_strs ] 
    in
    _CONCAT_ dotted_tys
\end{code}

Nota Bene: we must assign print-names to the forall'd type variables
alphabetically, with the first forall'd variable having the alphabetically
first name.  Reason: so anyone reading the type signature printed without
explicit forall's will be able to reconstruct them in the right order.

\begin{code}
ppr_ty_init :: PprStyle -> Int -> UniType -> Pretty

ppr_ty_init sty init_prec ty
  = let (tyvars, _, _)	= splitType ty
	lookup_fn	= mk_lookup_tyvar_fn sty tyvars
    in
	ppr_ty sty lookup_fn init_prec ty

mk_lookup_tyvar_fn :: PprStyle -> [TyVarTemplate] -> (TyVarTemplate -> Pretty)

mk_lookup_tyvar_fn sty tyvars
  = tv_lookup_fn
  where
    tv_lookup_fn :: TyVarTemplate -> Pretty
    tv_lookup_fn tyvar
      = let
	    pp_tyvar_styish = ppr sty tyvar

	    assocs = [ pp | (tv, pp) <- tvs_n_pprs, tv == tyvar ]

	    pp_tyvar_canonical
	      = case assocs of
		  []  -> pprPanic "pprUniType: bad tyvar lookup:" (ppr sty tyvar)
			    -- sometimes, in printing monomorphic types,
			    -- (usually in debugging), we won't have the tyvar
			    -- in our list; so we just ppr it anyway...
		  x:_ -> x
	in
    	case sty of
	  PprInterface _ -> pp_tyvar_canonical
	  PprForC _ 	 -> ppChar '*'
	  PprUnfolding _ -> case assocs of
			      x:_ -> ppBeside x (ppPStr SLIT("$z1"))
			      _   -> ppPStr SLIT("z$z1")
	  PprForUser	 -> case assocs of
			      x:_ -> x
			      _   -> pp_tyvar_styish
	  debuggish	 -> pp_tyvar_styish

    tvs_n_pprs = tyvars `zip` tyvar_pretties

    tyvar_pretties = letter_pprs {- a..y -} ++ number_pprs {- z0 ... zN -}

    letter_pprs = map (\ c -> ppChar c )    ['a' .. 'y']
    number_pprs = map (\ n -> ppBeside (ppChar 'z') (ppInt n))
		      ([0 .. ] :: [Int])
\end{code}

\begin{code}
ppr_ty :: PprStyle -> (TyVarTemplate -> Pretty) -> Int -> UniType -> Pretty

ppr_ty sty lookup_fn ctxt_prec (UniTyVarTemplate tyvar) = lookup_fn tyvar

ppr_ty sty lookup_fn ctxt_prec (UniTyVar tyvar) = ppr sty tyvar

ppr_ty sty lookup_fn ctxt_prec ty
  = case sty of
      PprForUser     -> context_onward
      PprInterface _ -> context_onward
      _		     ->
	(if null tyvars then id else ppBeside (ppr_forall sty tyvars))
	context_onward
  where
    (tyvars, context, tau_ty) = splitType ty

    context_onward =
      if (null pretty_context_pieces) then
	ppr_tau_ty sty lookup_fn ctxt_prec tau_ty
      else
	ppCat (pretty_context_pieces
	      ++ [connector sty, ppr_tau_ty sty lookup_fn ctxt_prec tau_ty]) -- ToDo: dubious

    pretty_context_pieces = ppr_context sty context

    ppr_forall :: PprStyle -> [TyVarTemplate] -> Pretty

    ppr_forall _   []	 = ppNil
    ppr_forall sty tyvars
      =	ppBesides [ppPStr SLIT("_forall_ "), ppIntersperse pp'SP{-'-} pp_tyvars,
		   ppPStr SLIT(" =>")]
      where
    	pp_tyvars = map lookup_fn tyvars

    ppr_context :: PprStyle -> [(Class, UniType)] -> [Pretty]

    ppr_context _   []		    = []
    ppr_context sty context@(c:cs)
      = case sty of
	  PprForUser	 -> userish
	  PprInterface _ -> userish
	  _		 -> hackerish
      where
    	userish 
	  = [if (context `lengthExceeds` (1::Int)) then
		ppBesides [ ppLparen,
		    ppIntersperse pp'SP{-'-} (map (ppr_kappa_tau PprForUser) context),
		    ppRparen]
	     else
		ppr_kappa_tau PprForUser (head context)
	    ]
	hackerish
    	  = (ppr_kappa_tau sty c) : (map ( pin_on_arrow . (ppr_kappa_tau sty) ) cs)

    connector PprForUser       = ppPStr SLIT("=>")
    connector (PprInterface _) = ppPStr SLIT("=>")
    connector other_sty        = ppPStr SLIT("->")

    ppr_kappa_tau :: PprStyle -> (Class, UniType) -> Pretty

    ppr_kappa_tau sty (clas, ty)
      = let
	    pp_ty    = ppr_tau_ty sty lookup_fn ctxt_prec ty
	    user_ish = ppCat [ppr PprForUser clas, pp_ty]
	    hack_ish = ppBesides [ppStr "{{", ppr sty clas, ppSP, pp_ty, ppStr "}}"]
	in
	case sty of
	  PprForUser     -> user_ish
	  PprInterface _ -> user_ish
	  _	         -> hack_ish

    pin_on_arrow p = ppBeside (ppPStr SLIT("-> ")) p
\end{code}

@ppr_tau_ty@ takes an @Int@ that is the precedence of the context.
The precedence levels are:
\begin{description}
\item[0:] What we start with.
\item[1:] Function application (@UniFuns@).
\item[2:] Type constructors.
\end{description}

A non-exported help function that really does the printing:
\begin{code}
tOP_PREC    = (0 :: Int)
fUN_PREC    = (1 :: Int)
tYCON_PREC  = (2 :: Int)

ppr_tau_ty :: PprStyle -> (TyVarTemplate -> Pretty) -> Int -> UniType -> Pretty

-- a quite special case, for printing instance decls in interfaces:
ppr_tau_ty sty@(PprInterface _) lookup_fn ctxt_prec (UniDict clas ty)
  = ppCat [ppr PprForUser clas, ppr_ty sty lookup_fn tYCON_PREC ty]

ppr_tau_ty sty lookup_fn ctxt_prec (UniSyn _ _ expansion)
  -- Expand type synonyms unless PprForUser
  -- NB: it is important that synonyms are expanded with PprInterface
  | case sty of { PprForUser -> False; _ -> True }
  = ppr_tau_ty sty lookup_fn ctxt_prec expansion 

ppr_tau_ty sty lookup_fn ctxt_prec (UniTyVarTemplate tyvar) = lookup_fn tyvar

ppr_tau_ty sty lookup_fn ctxt_prec (UniTyVar tyvar) = ppr sty tyvar

ppr_tau_ty sty lookup_fn ctxt_prec (UniFun ty1 ty2)
    -- we fiddle the precedences passed to left/right branches,
    -- so that right associativity comes out nicely...

    = let p1 = ppr_tau_ty sty lookup_fn fUN_PREC ty1
	  p2 = ppr_tau_ty sty lookup_fn tOP_PREC ty2
      in
      if ctxt_prec < fUN_PREC then -- no parens needed
	 ppCat [p1, ppBeside (ppPStr SLIT("-> ")) p2]
      else
	 ppCat [ppBeside ppLparen p1, ppBesides [ppPStr SLIT("-> "), p2, ppRparen]]

-- Special printing for list and tuple types.
-- we can re-set the precedence to tOP_PREC

ppr_tau_ty sty lookup_fn ctxt_prec (UniData tycon tys)
  = if tycon == listTyCon then
	ppBesides [ppLbrack, ppr_tau_ty sty lookup_fn tOP_PREC (head tys), ppRbrack]

    else if (tycon == (TupleTyCon (length tys))) then
    	ppBesides [ppLparen, ppIntersperse pp'SP{-'-} (map (ppr_tau_ty sty lookup_fn tOP_PREC) tys), ppRparen]
#ifdef DPH
    else if (tycon == podTyCon) then
       pprPodshort sty lookup_fn tOP_PREC (head tys)

    else if (tycon == (ProcessorTyCon ((length tys)-1))) then
       ppBesides [ppStr "(|",
                  ppIntersperse pp'SP{-'-}
                     (map (ppr_tau_ty sty lookup_fn tOP_PREC) (init tys)),
                  ppSemi ,
                  ppr_tau_ty sty lookup_fn tOP_PREC (last tys),
                  ppStr "|)"]
#endif {- Data Parallel Haskell -}
    else
	ppr_tycon_and_tys sty lookup_fn ctxt_prec tycon tys

ppr_tau_ty sty lookup_fn ctxt_prec (UniSyn tycon tys expansion)
 = ppBeside
     (ppr_tycon_and_tys sty lookup_fn ctxt_prec tycon tys)
     (ifPprShowAll sty (ppCat [ppStr " {- expansion:", ppr_ty sty lookup_fn ctxt_prec expansion, ppStr "-}"]))

-- For SPECIALIZE instance error messages ...
ppr_tau_ty sty@PprForUser lookup_fn ctxt_prec (UniDict clas ty)
 = if ctxt_prec < tYCON_PREC then
	ppCat [ppr sty clas, ppr_ty sty lookup_fn tYCON_PREC ty]
   else
	ppBesides [ppStr "(", ppr sty clas, ppSP, ppr_ty sty lookup_fn tYCON_PREC ty, ppStr ")"]

ppr_tau_ty sty lookup_fn ctxt_prec (UniDict clas ty)
 = ppBesides [ppStr "{{", ppr sty clas, ppSP, ppr_ty sty lookup_fn tYCON_PREC ty, ppStr "}}"]

ppr_tau_ty sty lookup_fn ctxt_prec other_ty -- must a be UniForall (ToDo: something?)
 = ppBesides [ppLparen, ppr_ty sty lookup_fn ctxt_prec other_ty, ppRparen]

-- code shared for UniDatas and UniSyns
ppr_tycon_and_tys :: PprStyle -> (TyVarTemplate -> Pretty) -> Int -> TyCon -> [UniType] -> Pretty

ppr_tycon_and_tys sty lookup_fn ctxt_prec tycon tys
  = let pp_tycon = ppr (case sty of PprInterface _ -> PprForUser; _ -> sty) tycon
    in
    if null tys then
	pp_tycon
    else if ctxt_prec < tYCON_PREC then -- no parens needed
	ppCat [pp_tycon, ppIntersperse ppSP (map (ppr_tau_ty sty lookup_fn tYCON_PREC) tys) ]
    else
	ppBesides [ ppLparen, pp_tycon, ppSP,
	       ppIntersperse ppSP (map (ppr_tau_ty sty lookup_fn tYCON_PREC) tys), ppRparen ]
\end{code}

\begin{code}
#ifdef DPH
pprPodshort :: PprStyle -> (TyVarTemplate-> Pretty) -> Int -> UniType -> Pretty
pprPodshort sty lookup_fn ctxt_prec (UniData tycon tys)
  | (tycon == (ProcessorTyCon ((length tys)-1))) 
    =  ppBesides [ppStr "<<", 
		  ppIntersperse pp'SP{-'-} 
		     (map (ppr_tau_ty sty lookup_fn tOP_PREC) (init tys)), 
	          ppSemi ,
		  ppr_tau_ty sty lookup_fn tOP_PREC (last tys), 
		  ppStr ">>"]
pprPodshort sty lookup_fn ctxt_prec ty
    =  ppBesides [ppStr "<<",
		  ppr_tau_ty sty lookup_fn tOP_PREC ty,
		  ppStr ">>"]
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
showTyCon :: PprStyle -> TyCon -> String
showTyCon sty tycon
  = ppShow 80 (pprTyCon sty tycon [])

pprTyCon :: PprStyle -> TyCon -> [[Maybe UniType]] -> Pretty
-- with "PprInterface", we print out for interfaces

pprTyCon sty@(PprInterface sw_chkr) (SynonymTyCon k n a vs exp unabstract) specs
  = ASSERT (null specs)
    let
	lookup_fn   = mk_lookup_tyvar_fn sty vs
	pp_tyvars   = map lookup_fn vs
	pp_abstract = if unabstract || (sw_chkr OmitInterfacePragmas)
		      then ppNil
		      else ppStr "{-# GHC_PRAGMA _ABSTRACT_ #-}"
    in
    ppCat [ppPStr SLIT("type"), ppr sty n, ppIntersperse ppSP pp_tyvars,
	   ppEquals, ppr_ty sty lookup_fn tOP_PREC exp, pp_abstract]

pprTyCon sty@(PprInterface sw_chkr) this_tycon@(DataTyCon k n a vs cons derivings unabstract) specs
  = ppHang (ppCat [ppPStr SLIT("data"),
		   -- pprContext sty context,
		   ppr sty n,
		   ppIntersperse ppSP (map lookup_fn vs)])
	   4
	   (ppCat [pp_unabstract_condecls,
		   pp_pragma])
	   -- NB: we do not print deriving info in interfaces
  where
    lookup_fn = mk_lookup_tyvar_fn sty vs

    yes_we_print_condecls
      = unabstract
    	&& not (null cons)	-- we know what they are
	&& (case (getExportFlag n) of
	      ExportAbs -> False
	      other 	-> True)

    yes_we_print_pragma_condecls
      = not yes_we_print_condecls
	&& not (sw_chkr OmitInterfacePragmas)
        && not (null cons)
        && not (maybeToBool (maybePurelyLocalTyCon this_tycon))
	{- && not (any (dataConMentionsNonPreludeTyCon this_tycon) cons) -}

    yes_we_print_pragma_specs
      = not (null specs)

    pp_unabstract_condecls
      = if yes_we_print_condecls
	then ppCat [ppSP, ppEquals, pp_condecls]
	else ppNil
	    
    pp_pragma_condecls
      = if yes_we_print_pragma_condecls
	then pp_condecls
	else ppNil

    pp_pragma_specs
      = if yes_we_print_pragma_specs
	then pp_specs
	else ppNil

    pp_pragma
      = if (yes_we_print_pragma_condecls || yes_we_print_pragma_specs)
	then ppCat [ppStr "\t{-# GHC_PRAGMA", pp_pragma_condecls, pp_pragma_specs, ppStr "#-}"]
	else ppNil

    pp_condecls
      = let
	    (c:cs) = cons
	in
	ppCat ((ppr_con c) : (map ppr_next_con cs))
      where
	ppr_con con
	  = let
		(_, _, con_arg_tys, _) = getDataConSig con
	    in
	    ppCat [pprNonOp PprForUser con, -- the data con's name...
		   ppIntersperse ppSP (map (ppr_ty sty lookup_fn tYCON_PREC) con_arg_tys)]

    	ppr_next_con con = ppCat [ppChar '|', ppr_con con]

    pp_specs
      = ppBesides [ppStr "_SPECIALISE_ ", pp_the_list [
	  ppCat [ppLbrack, ppInterleave ppComma (map pp_maybe ty_maybes), ppRbrack]
	  | ty_maybes <- specs ]]
         
    pp_the_list [p]    = p
    pp_the_list (p:ps) = ppCat [ppBeside p ppComma, pp_the_list ps]

    pp_maybe Nothing   = pp_NONE
    pp_maybe (Just ty) = pprParendUniType sty ty

    pp_NONE = ppStr "_N_"

pprTyCon (PprInterface _) (TupleTyCon a) specs
  = ASSERT (null specs)
    ppCat [ ppStr "{- Tuple", ppInt a, ppStr "-}" ]

pprTyCon (PprInterface _) (PrimTyCon k n a kind_fn) specs
  = ASSERT (null specs)
    ppCat [ ppStr "{- data", ppr PprForUser n, ppStr " *built-in* -}" ]

#ifdef DPH
pprTyCon (PprInterface _) (ProcessorTyCon a) specs
      = ppCat [ ppStr "{- Processor", ppInt a, ppStr "-}" ]
#endif {- Data Parallel Haskell -}

-- regular printing (ToDo: probably update)

pprTyCon sty (SynonymTyCon k n a vs exp unabstract) [{-no specs-}]
  = ppBeside (ppr sty n)
	     (ifPprShowAll sty
		(ppCat [ ppStr " {-", ppInt a, interpp'SP sty vs,
			 pprParendUniType sty exp,
			 if unabstract then ppNil else ppStr "_ABSTRACT_", ppStr "-}"]))

pprTyCon sty tycon@(DataTyCon k n a vs cons derivings unabstract) [{-no specs-}]
  = case sty of
      PprDebug   -> pp_tycon_and_uniq
      PprShowAll -> pp_tycon_and_uniq
      _	    	 -> pp_tycon
  where
    pp_tycon_and_uniq = ppBesides [pp_tycon, ppStr "{-", pprUnique k, ppStr "-}"]
    pp_tycon
      = let
	    pp_name = ppr sty n
    	in
	if codeStyle sty || tycon /= listTyCon
	then pp_name
	else ppBesides [ppLbrack, interpp'SP sty vs, ppRbrack]

{-ppBeside-} -- pp_tycon
{- SOMETIMES:
	     (ifPprShowAll sty
		(ppCat [ ppStr " {-", ppInt a, interppSP sty vs,
			    interpp'SP PprForUser cons,
			    ppStr "deriving (", interpp'SP PprForUser derivings,
			    ppStr ")-}" ]))
-}

pprTyCon sty (TupleTyCon a) [{-no specs-}]
  = ppBeside (ppPStr SLIT("Tuple")) (ppInt a)

pprTyCon sty (PrimTyCon k n a kind_fn) [{-no specs-}]
  = ppr sty n

pprTyCon sty (SpecTyCon tc ty_maybes) []
  = ppBeside (pprTyCon sty tc []) 
	     (if (codeStyle sty)
	      then identToC tys_stuff
	      else ppPStr   tys_stuff)
  where
    tys_stuff = specMaybeTysSuffix ty_maybes

#ifdef DPH
pprTyCon sty (ProcessorTyCon a) [] = ppBeside (ppStr "Processor") (ppInt a)

pprTyCon sty (PodizedPodTyCon dim tc) []
  = ppBesides [ ppr sty tc, ppStr "Podized", ppr sty dim]
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
pprIfaceClass :: (GlobalSwitch -> Bool) -> (Id -> Id) -> IdEnv UnfoldingDetails -> Class -> Pretty

pprIfaceClass sw_chker better_id_fn inline_env
	(MkClass k n tyvar super_classes sdsels ops sels defms insts links)
  = let
	sdsel_infos = map (getIdInfo . better_id_fn) sdsels
    in
    ppAboves [ ppCat [ppPStr SLIT("class"), ppr_theta tyvar super_classes,
		      ppr sty n, lookup_fn tyvar,
		      if null sdsel_infos
		      || omit_iface_pragmas
		      || (any boringIdInfo sdsel_infos)
			-- ToDo: really should be "all bor..."
			-- but then parsing is more tedious,
			-- and this is really as good in practice.
		      then ppNil
		      else pp_sdsel_pragmas (sdsels `zip` sdsel_infos),
		      if (null ops)
		      then ppNil
		      else ppPStr SLIT("where")],
	       ppNest 8  (ppAboves 
		 [ ppr_op op (better_id_fn sel) (better_id_fn defm)
		 | (op,sel,defm) <- zip3 ops sels defms]) ]
  where
    sty = PprInterface sw_chker
    omit_iface_pragmas = sw_chker OmitInterfacePragmas

    lookup_fn = mk_lookup_tyvar_fn sty [tyvar]

    ppr_theta :: TyVarTemplate -> [Class] -> Pretty
    ppr_theta tv [] = ppNil
    ppr_theta tv super_classes
      = ppBesides [ppLparen,
		   ppIntersperse pp'SP{-'-} (map ppr_assert super_classes), 
		   ppStr ") =>"]
      where
	ppr_assert (MkClass _ n _ _ _ _ _ _ _ _) = ppCat [ppr sty n, lookup_fn tv]

    pp_sdsel_pragmas sdsels_and_infos
      = ppCat [ppStr "{-# GHC_PRAGMA {-superdicts-}",
	       ppIntersperse pp'SP{-'-}
	         [ppIdInfo sty sdsel False{-NO specs-} better_id_fn inline_env info
		 | (sdsel, info) <- sdsels_and_infos ],
	       ppStr "#-}"]

    ppr_op op opsel_id defm_id
      = let
	    stuff = ppBeside (ppChar '\t') (ppr_class_op sty [tyvar] op)
	in
	if omit_iface_pragmas
	then stuff
	else ppAbove stuff
		(ppCat [ppStr "\t {-# GHC_PRAGMA", ppAbove pp_opsel pp_defm, ppStr "#-}"])
      where
	pp_opsel = ppCat [ppPStr SLIT("{-meth-}"), ppIdInfo sty opsel_id False{-no specs-} better_id_fn inline_env (getIdInfo opsel_id)]
	pp_defm  = ppCat [ppPStr SLIT("\t\t{-defm-}"), ppIdInfo sty defm_id False{-no specs-} better_id_fn inline_env (getIdInfo defm_id)]
\end{code}

\begin{code}
pprClassOp :: PprStyle -> ClassOp -> Pretty

pprClassOp sty op = ppr_class_op sty [] op

ppr_class_op sty tyvars (MkClassOp op_name i ty)
  = case sty of
      PprForC _	      -> pp_C
      PprForAsm _ _ _ -> pp_C
      PprInterface _  -> ppCat [pp_user, ppPStr SLIT("::"), ppr_ty sty      lookup_fn tOP_PREC ty]
      PprShowAll      -> ppCat [pp_user, ppPStr SLIT("::"), ppr_ty PprDebug lookup_fn tOP_PREC ty]
      _		      -> pp_user
  where
    (local_tyvars,_,_)	= splitType ty
    lookup_fn		= mk_lookup_tyvar_fn sty (tyvars ++ local_tyvars)

    pp_C		= ppPStr op_name
    pp_user		= if isAvarop op_name
			  then ppBesides [ppLparen, pp_C, ppRparen]
			  else pp_C
\end{code}

%************************************************************************
%*									*
\subsection[UniTyFuns-matching]{@matchTy@}
%*									*
%************************************************************************

Matching is a {\em unidirectional} process, matching a type against a
template (which is just a type with type variables in it).  The matcher
assumes that there are no repeated type variables in the template, so that
it simply returns a mapping of type variables to types.

\begin{code}
matchTy :: UniType				-- Template
	-> UniType				-- Proposed instance of template
	-> Maybe [(TyVarTemplate,UniType)]	-- Matching substitution

matchTy (UniTyVarTemplate v) ty = Just [(v,ty)]
matchTy (UniTyVar _) ty = panic "matchTy: unexpected TyVar (need TyVarTemplates)"

matchTy (UniFun fun1 arg1) (UniFun fun2 arg2) = matchTys [fun1, arg1] [fun2, arg2]

matchTy ty1@(UniData con1 args1) ty2@(UniData con2 args2) | con1 == con2
  = matchTys args1 args2 -- Same constructors, just match the arguments

-- with type synonyms, we have to be careful
-- for the exact same reasons as in the unifier.
-- Please see the considerable commentary there
-- before changing anything here! (WDP 95/05)

-- If just one or the other is a "visible" synonym (they all are at
-- the moment...), just expand it.

matchTy (UniSyn con1 args1 ty1) ty2
  | isVisibleSynTyCon con1
  = matchTy ty1 ty2
matchTy ty1 (UniSyn con2 args2 ty2)
  | isVisibleSynTyCon con2
  = matchTy ty1 ty2

matchTy (UniSyn con1 args1 ty1) (UniSyn con2 args2 ty2)
  -- if we get here, both synonyms must be "abstract"
  -- (NB: not done yet)
  = if (con1 == con2) then
	-- Good news!  Same synonym constructors, so we can shortcut
	-- by unifying their arguments and ignoring their expansions.
	matchTys args1 args2
    else
	-- Never mind.  Just expand them and try again
	matchTy ty1 ty2

-- Catch-all fails
matchTy templ ty = Nothing
\end{code}

@matchTys@ matches corresponding elements of a list of templates and
types.

\begin{code}
matchTys :: [UniType] -> [UniType] -> Maybe [(TyVarTemplate, UniType)]

matchTys [] [] = Just []
matchTys (templ:templs) (ty:tys)
  = case (matchTy templ ty) of
      Nothing    -> Nothing
      Just subst -> case (matchTys templs tys) of
		      Nothing     -> Nothing
		      Just subst2 -> Just (subst ++ subst2)
#ifdef DEBUG
matchTys [] tys
  = pprPanic "matchTys: out of templates!; tys:" (ppr PprDebug tys)
matchTys tmpls []
  = pprPanic "matchTys: out of types!; templates:" (ppr PprDebug tmpls)
#endif
\end{code}

%************************************************************************
%*									*
\subsection[UniTyFuns-misc]{Misc @UniType@ functions}
%*									*
%************************************************************************

\begin{code}
cmpUniTypeMaybeList :: [Maybe UniType] -> [Maybe UniType] -> TAG_
cmpUniTypeMaybeList []     []     = EQ_
cmpUniTypeMaybeList (x:xs) []     = GT_
cmpUniTypeMaybeList []     (y:ys) = LT_
cmpUniTypeMaybeList (x:xs) (y:ys)
  = case cmp_maybe_ty x y of { EQ_ -> cmpUniTypeMaybeList xs ys; other -> other }

cmp_maybe_ty Nothing  Nothing  = EQ_
cmp_maybe_ty (Just x) Nothing  = GT_
cmp_maybe_ty Nothing  (Just y) = LT_
cmp_maybe_ty (Just x) (Just y) = cmpUniType True{-properly-} x y
\end{code}

Identity function if the type is a @TauType@; panics otherwise.
\begin{code}
#ifdef DEBUG
verifyTauTy :: String -> TauType -> TauType

verifyTauTy caller ty@(UniDict _ _)   = pprPanic (caller++":verifyTauTy:dict") (ppr PprShowAll ty)
verifyTauTy caller ty@(UniForall _ _) = pprPanic (caller++":verifyTauTy:forall") (ppr PprShowAll ty)
verifyTauTy caller (UniSyn tycon tys expansion) = UniSyn tycon tys (verifyTauTy caller expansion)
verifyTauTy caller tau_ty	    = tau_ty

#endif {- DEBUG -}
\end{code}

\begin{code}
showTypeCategory :: UniType -> Char
    {-
	{C,I,F,D}   char, int, float, double
	T	    tuple
	S	    other single-constructor type
	{c,i,f,d}   unboxed ditto
	t	    *unpacked* tuple
	s	    *unpacked" single-cons...

	v	    void#
	a	    primitive array

	E	    enumeration type
	+	    dictionary, unless it's a ...
	L	    List
	>	    function
	M	    other (multi-constructor) data-con type
	.	    other type
	-	    reserved for others to mark as "uninteresting"
    -}
showTypeCategory ty
  = if isDictTy ty
    then '+'
    else
      case getUniDataTyCon_maybe ty of
	Nothing -> if isFunType ty
		   then '>'
		   else '.'

	Just (tycon,_,_) ->
	  if	  maybeToBool (maybeCharLikeTyCon tycon)   then 'C'
	  else if maybeToBool (maybeIntLikeTyCon tycon)	   then 'I'
	  else if maybeToBool (maybeFloatLikeTyCon tycon)  then 'F'
	  else if maybeToBool (maybeDoubleLikeTyCon tycon) then 'D'
	  else if tycon == integerTyCon			   then 'J'
	  else if tycon == charPrimTyCon		   then 'c'
	  else if (tycon == intPrimTyCon || tycon == wordPrimTyCon
		|| tycon == addrPrimTyCon)		   then 'i'
	  else if tycon == floatPrimTyCon		   then 'f'
	  else if tycon == doublePrimTyCon		   then 'd'
	  else if isPrimTyCon tycon {- array, we hope -}   then 'A'
	  else if isEnumerationTyCon tycon		   then 'E'
	  else if isTupleTyCon tycon			   then 'T'
	  else if maybeToBool (maybeSingleConstructorTyCon tycon) then 'S'
	  else if tycon == listTyCon			   then 'L'
	  else 'M' -- oh, well...
\end{code}
