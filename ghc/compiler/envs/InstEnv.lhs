%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[InstEnv]{Instance environments}

\begin{code}
#include "HsVersions.h"

module InstEnv (
	-- these types could use some abstractification (??? ToDo)
	ClassInstEnv(..), -- OLD: IdInstEnv(..),
	InstTemplate, InstTy,
	MethodInstInfo(..),	-- needs to be exported? (ToDo)
	InstanceMapper(..),	-- widely-used synonym

--	instMethod, instTemplate, -- no need to export
	addClassInst, {- NOT USED addConstMethInst, -}
	lookupInst,
	lookupClassInstAtSimpleType,
	lookupNoBindInst,
	mkInstSpecEnv,

	MatchEnv(..),	-- mk more abstract (??? ToDo)
	nullMEnv,
--	mkMEnv, lookupMEnv, matchMEnv, insertMEnv, -- no need to export

	-- and to make the interface self-sufficient...
	Class, ClassOp, CoreExpr, Expr, TypecheckedPat, Id,
	Inst, InstOrigin, Maybe, MaybeErr, TyVarTemplate, TyCon,
	UniType, SplitUniqSupply, SpecInfo, SpecEnv
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)

import AbsPrel		( intTyCon, --wordTyCon, addrTyCon,
			  floatTyCon, doubleTyCon, charDataCon, intDataCon,
			  wordDataCon, addrDataCon, floatDataCon,
			  doubleDataCon,
			  intPrimTyCon, doublePrimTyCon
			)
import AbsSyn		-- TypecheckedExpr, etc.
import AbsUniType
import Id
import IdInfo
import Inst
import Maybes		-- most of it
import Outputable	( isExported )
import PlainCore	-- PlainCoreExpr, etc.
import Pretty
import PrimKind		-- rather grubby import (ToDo?)
import SplitUniq
import Util
\end{code}

%************************************************************************
%*									*
\subsection[InstEnv-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InstanceMapper
  = Class -> (ClassInstEnv, ClassOp -> SpecEnv)

type ClassInstEnv
  = MatchEnv UniType InstTemplate	-- Instances of dicts

data InstTemplate
  = MkInstTemplate
	Id		-- A fully polymorphic Id; it is the function
			-- which produces the Id instance or dict from
			-- the pieces specified by the rest of the
			-- template.  Its SrcLoc tells where the
			-- instance was defined.
	[UniType]	-- Apply it to these types, suitably instantiated
	[InstTy]	-- and instances of these things

type MethodInstInfo = (Id, [UniType], InstTemplate) -- Specifies a method instance
\end{code}

There is an important consistency constraint between the @MatchEnv@s
in and the @InstTemplate@s inside them: the @UniType@(s) which is/are
the key for the @MatchEnv@ must contain only @TyVarTemplates@, and
these must be a superset of the @TyVarTemplates@ mentioned in the
corresponding @InstTemplate@.

Reason: the lookup process matches the key against the desired value,
returning a substitution which is used to instantiate the template.

\begin{code}
data InstTy
  = DictTy	Class UniType
  | MethodTy	Id    [UniType]
\end{code}

	MkInstTemplate f tvs insts

says that, given a particular mapping of type variables tvs to some
types tys, the value which is the required instance is

	f tys (insts [tys/tvs])


@instMethod@ is used if there is no instance for a method; then it is
expressed in terms of the corresponding dictionary (or possibly, in a
wired-in case only, dictionaries).

\begin{code}
instMethod :: SplitUniqSupply
	   -> InstOrigin
	   -> Id -> [UniType]
	   -> (TypecheckedExpr, [Inst])

instMethod uniqs orig id tys
  = (mkDictApp (mkTyApp (Var id) tys) dicts,
     insts)
  where
   (tyvars, theta, tau_ty) = splitType (getIdUniType id)
   tenv			   = tyvars `zipEqual` tys
   insts		   = mk_dict_insts uniqs theta
   dicts		   = map mkInstId insts

   mk_dict_insts us [] = []
   mk_dict_insts us ((clas, ty) : rest)
      = case splitUniqSupply us of { (s1, s2) ->
        (Dict (getSUnique s1) clas (instantiateTauTy tenv ty) orig)
	: mk_dict_insts s2 rest
	}
\end{code}

@instTemplate@ is used if there is an instance for a method or dictionary.

\begin{code}
instTemplate :: SplitUniqSupply
	     -> InstOrigin
	     -> [(TyVarTemplate, UniType)]
	     -> InstTemplate
	     -> (TypecheckedExpr, [Inst])

instTemplate uniqs orig tenv (MkInstTemplate id ty_tmpls inst_tys)
  = (mkDictApp (mkTyApp (Var id) ty_args) ids,	-- ToDo: not strictly a dict app
						-- for Method inst_tys
     insts)
  where
    ty_args	    = map (instantiateTy tenv) ty_tmpls
    insts	    = mk_insts uniqs inst_tys
    ids		    = map mkInstId insts

    mk_insts us [] = []
    mk_insts us (inst_ty : rest)
      = case splitUniqSupply us of { (s1, s2) ->
	let
	    uniq = getSUnique s1
	in
        (case inst_ty of
	   DictTy clas ty  -> Dict uniq clas (instantiateTy tenv ty) orig
	   MethodTy id tys -> Method uniq id (map (instantiateTy tenv) tys) orig
	) : mk_insts s2 rest
	}
\end{code}


%************************************************************************
%*									*
\subsection[InstEnv-adding]{Adding new class instances}
%*									*
%************************************************************************

@addClassInstance@ adds the appropriate stuff to the @ClassInstEnv@ based on
information from a single instance declaration.	 It complains about
any overlap with an existing instance.

Notice that we manufacture the @DictFunId@ and @ConstMethodId@s from
scratch here, rather than passing them in.  This means a small amount
of duplication (no big deal) and that we can't attach a single
canonical unfolding; but they don't have a slot for unfoldings
anyway...  This could be improved.  (We do, however, snaffle in the
pragma info from the interface...)

{\em Random notes}

\begin{verbatim}
class Foo a where
  fop :: Ord b => a -> b -> b -> a

instance Foo Int where
  fop x y z = if y<z then x else fop x z y

instance Foo a => Foo [a] where
  fop []     y z = []
  fop (x:xs) y z = [fop x y z]
\end{verbatim}


For the Int instance we add to the ??? envt
\begin{verbatim}
  (ClassOpId Foo fop) |--> [Int,b] |--> InstTemplate (ConstMethodId Foo fop Int) [b] [Ord b]
\end{verbatim}

If there are no type variables, @addClassInstance@ adds constant
instances for those class ops not mentioned in the class-op details
(possibly using the pragma info that was passed in).  This MUST
be the same decision as that by @tcInstDecls2@ about whether to
generate constant methods.  NB: A slightly more permissive version
would base the decision on the context being empty, but there is
slightly more admin associated and the benefits are very slight; the
context is seldom empty unless there are no tyvars involved.

Note: the way of specifying class-op instance details is INADEQUATE
for polymorphic class ops.  That just means you can't specify clever
instances for them via this function.

\begin{code}
addClassInst
    :: Class			-- class in question (for err msg only)    	
    -> ClassInstEnv		-- Incoming envt
    -> UniType			-- The instance type
    -> Id			-- Dict fun id to apply
    -> [TyVarTemplate]		-- Types to which (after instantiation) to apply the dfun
    -> ThetaType		-- Dicts to which to apply the dfun
    -> SrcLoc			-- associated SrcLoc (for err msg only)
    -> MaybeErr
	  ClassInstEnv		-- Success
	  (Class, (UniType, SrcLoc),  -- Failure: the overlapping pair
		  (UniType, SrcLoc))

addClassInst clas inst_env inst_ty dfun_id inst_tyvars dfun_theta locn
  = case (insertMEnv matchTy inst_env inst_ty dict_template) of
      Succeeded inst_env' -> Succeeded inst_env'
      Failed (ty', MkInstTemplate id' _ _)
	-> Failed (clas, (inst_ty, locn), (ty', getSrcLoc id'))
  where
    dict_template = MkInstTemplate dfun_id 
				   (map mkTyVarTemplateTy inst_tyvars) 
				   (unzipWith DictTy dfun_theta)
\end{code}

============ NOT USED =============
@addConstMethInst@ panics on overlap, because @addClassInst@ has already found
any overlap.

\begin{pseudocode}
addConstMethInst :: IdInstEnv
		 -> UniType		-- The instance type
		 -> Id			-- The constant method
		 -> [TyVarTemplate]	-- Apply method to these (as above)
		 -> IdInstEnv

addConstMethInst inst_env inst_ty meth_id inst_tyvars
  = case (insertMEnv matchTys inst_env [inst_ty] template) of
      Succeeded inst_env' -> inst_env'
      Failed (tys', MkInstTemplate id' _ _) ->
	pprPanic "addConstMethInst:"
		(ppSep [ppr PprDebug meth_id,
		        ppr PprDebug inst_ty,
			ppr PprDebug id'])
  where
     template = MkInstTemplate meth_id (map mkTyVarTemplateTy inst_tyvars) []
	-- Constant method just needs to be applied to tyvars
	-- (which are usually empty)
\end{pseudocode}

@mkIdInstEnv@ is useful in the simple case where we've a list of
@(types, id)@ pairs; the \tr{id} is the \tr{types} specialisation of
some other Id (in which the resulting IdInstEnv will doubtless be
embedded.  There's no messing about with type variables or
dictionaries here.

\begin{code}
{- OLD:
mkIdInstEnv :: [([TauType],Id)] -> IdInstEnv

mkIdInstEnv [] = nullMEnv
mkIdInstEnv ((tys,id) : rest) 
  = let
	inst_env = mkIdInstEnv rest
    in
    case (insertMEnv matchTys inst_env tys template) of
      Succeeded inst_env' -> inst_env'
      Failed _ -> panic "Failed in mkIdInstEnv"
  where
    template = MkInstTemplate id [] []
-}
\end{code}

%************************************************************************
%*									*
\subsection[InstEnv-lookup]{Performing lookup}
%*									*
%************************************************************************

\begin{code}
lookupInst :: SplitUniqSupply
	   -> Inst
	   -> Maybe (TypecheckedExpr,
		     [Inst])

lookupInst uniqs (Dict _ clas ty orig)
  = if isTyVarTy ty then
	Nothing	-- No instances of a class at a type variable
    else
      case (lookupMEnv matchTy inst_env ty) of
	Nothing		    -> Nothing
	Just (_,tenv,templ) -> Just (instTemplate uniqs orig tenv templ)
  where
    inst_env
      = case orig of

	  -- During deriving and instance specialisation operations
	  -- we can't get the instances of the class from inside the
	  -- class, because the latter ain't ready yet.  Instead we
	  -- find a mapping from classes to envts inside the dict origin.
	  -- (A Simon hack [WDP])

	  DerivingOrigin inst_mapper _ _ _ _ -> fst (inst_mapper clas)

	  InstanceSpecOrigin inst_mapper _ _ _ -> fst (inst_mapper clas)

	  -- Usually we just get the instances of the class from
	  -- inside the class itself.

	  other -> getClassInstEnv clas

lookupInst uniqs (Method _ id tys orig)
  = if (all isTyVarTy tys) then
	general_case	-- Instance types are all type variables, so there can't be
			-- a special instance for this method

    else	-- Get the inst env from the Id, and look up in it
      case (lookupSpecEnv (getIdSpecialisation id) tys) of
	Nothing		    -> general_case
	Just (spec_id, types_left, num_dicts_to_toss)
	  -> Just (instMethod uniqs orig spec_id types_left)
  where
    general_case = Just (instMethod uniqs orig id tys)
\end{code}

Now "overloaded" literals: the plain truth is that the compiler
is intimately familiar w/ the types Int, Integer, Float, and Double;
for everything else, we actually conjure up an appropriately-applied
fromInteger/fromRational, as the Haskell report suggests.

\begin{code}
lookupInst uniqs (LitInst u (OverloadedIntegral i from_int from_integer) ty orig)
  = Just (
    case (getUniDataTyCon_maybe ty) of	-- this way is *unflummoxed* by synonyms
      Just (tycon, [], _)
        | tycon == intPrimTyCon		-> (intprim_lit,    [])
	| tycon == doublePrimTyCon	-> (doubleprim_lit, [])
        | tycon == intTyCon		-> (int_lit,        [])
	| tycon == doubleTyCon		-> (double_lit,     [])
	| tycon == floatTyCon		-> (float_lit,      [])
--	| tycon == wordTyCon		-> (word_lit,       [])
--	| tycon == addrTyCon		-> (addr_lit,       [])

      _{-otherwise-} ->

	if (i >= toInteger minInt && i <= toInteger maxInt) then
	    -- It's overloaded but small enough to fit into an Int

	    let u2		= getSUnique uniqs
		method	= Method u2 from_int [ty] orig
	    in
	    (App (Var (mkInstId method)) int_lit, [method])

	else
	    -- Alas, it is overloaded and a big literal!

	    let u2	   = getSUnique uniqs
		method = Method u2 from_integer [ty] orig
	    in
	    (App (Var (mkInstId method)) (Lit (IntLit i)), [method])
    )
  where
#if __GLASGOW_HASKELL__ <= 22
    iD = ((fromInteger i) :: Double)
#else
    iD = ((fromInteger i) :: Rational)
#endif
    intprim_lit    = Lit (IntPrimLit i)
    doubleprim_lit = Lit (DoublePrimLit iD)
    int_lit        = App (Var intDataCon)    intprim_lit
    double_lit     = App (Var doubleDataCon) doubleprim_lit
    float_lit      = App (Var floatDataCon)  (Lit (FloatPrimLit iD))
--  word_lit       = App (Var wordDataCon)   intprim_lit
--  addr_lit       = App (Var addrDataCon)   intprim_lit

lookupInst uniqs (LitInst u (OverloadedFractional f from_rational) ty orig)
  = Just (
    case (getUniDataTyCon_maybe ty) of	-- this way is *unflummoxed* by synonyms
      Just (tycon, [], _)
	| tycon == doublePrimTyCon -> (doubleprim_lit, [])
	| tycon == doubleTyCon	   -> (double_lit, [])
	| tycon == floatTyCon	   -> (float_lit,  [])

      _ {-otherwise-} ->    -- gotta fromRational it...
	--pprTrace "lookupInst:fractional lit ty?:" (ppr PprDebug ty) (
	let
	    u2	   = getSUnique uniqs
	    method = Method u2 from_rational [ty] orig
	in
	(App (Var (mkInstId method)) (Lit (FracLit f)), [method])
	--)
    )
  where
#if __GLASGOW_HASKELL__ <= 22
    fD = ((fromRational f) :: Double)
#else
    fD = f
#endif
    doubleprim_lit = Lit (DoublePrimLit fD)
    double_lit     = App (Var doubleDataCon) doubleprim_lit
    float_lit      = App (Var floatDataCon)  (Lit (FloatPrimLit  fD))
\end{code}

There is a second, simpler interface, when you want an instance of a
class at a given nullary type constructor.  It just returns the
appropriate dictionary if it exists.  It is used only when resolving
ambiguous dictionaries.

\begin{code}
lookupClassInstAtSimpleType :: Class -> UniType -> Maybe Id

lookupClassInstAtSimpleType clas ty
  = case (lookupMEnv matchTy (getClassInstEnv clas) ty) of
      Nothing				   -> Nothing
      Just (_,_,MkInstTemplate dict [] []) -> Just dict
\end{code}

Notice in the above that the type constructors in the default list
should all have arity zero, so there should be no type variables
or thetas in the instance declaration.

There's yet a third interface for Insts which need no binding.
They are used to record constraints on type variables, notably
for CCall arguments and results.

\begin{code}
lookupNoBindInst :: SplitUniqSupply
	         -> Inst
	         -> Maybe [Inst]

lookupNoBindInst uniqs (Dict _ clas ty orig)
  = if isTyVarTy ty then
	Nothing	-- No instances of a class at a type variable
    else
      case (lookupMEnv matchTy inst_env ty) of
	Nothing		    -> Nothing
	Just (_,tenv,templ) ->
	  case (instTemplate uniqs orig tenv templ) of
	    (bottom_rhs, insts)
	      -> Just insts
		-- The idea here is that the expression built by
		-- instTemplate isn't relevant; indeed, it might well
		-- be a place-holder bottom value.
  where
    inst_env = getClassInstEnv clas
\end{code}

\begin{code}
mkInstSpecEnv :: Class			-- class
	      -> UniType		-- instance type
	      -> [TyVarTemplate]	-- instance tyvars
	      -> ThetaType		-- superclasses dicts
	      -> SpecEnv		-- specenv for dfun of instance

mkInstSpecEnv clas inst_ty inst_tvs inst_theta
  = mkSpecEnv (catMaybes (map maybe_spec_info matches))
  where
    matches = matchMEnv matchTy (getClassInstEnv clas) inst_ty

    maybe_spec_info (_, match_info, MkInstTemplate dfun _ [])
      = Just (SpecInfo (map (assocMaybe match_info) inst_tvs) (length inst_theta) dfun)
    maybe_spec_info (_, match_info, _)
      = Nothing

\end{code}

%************************************************************************
%*									*
\subsection[MatchEnv]{Matching environments}
%*									*
%************************************************************************

``Matching'' environments allow you to bind a template to a value;
when you look up in it, you supply a value which is matched against
the template.

\begin{code}
type MatchEnv key value = [(key, value)]
\end{code}

For now we just use association lists.	The list is maintained sorted
in order of {\em decreasing specificness} of @key@, so that the first
match will be the most specific.

\begin{code}
nullMEnv :: MatchEnv a b
nullMEnv = []

mkMEnv :: [(key, value)] -> MatchEnv key value
mkMEnv stuff = stuff
\end{code}

@lookupMEnv@ looks up in a @MatchEnv@.
It simply takes the first match, should be the most specific.

\begin{code}
lookupMEnv :: (key {- template -} ->	-- Matching function
	       key {- instance -} ->
	       Maybe match_info)
	   -> MatchEnv key value	-- The envt
	   -> key			-- Key
	   -> Maybe (key,		-- Template
		     match_info,	-- Match info returned by matching fn
		     value)		-- Value

lookupMEnv key_match alist key
  = find alist
  where
    find [] = Nothing
    find ((tpl, val) : rest)
      = case key_match tpl key of
	  Nothing	  -> find rest
	  Just match_info -> Just (tpl, match_info, val)
\end{code}

@matchEnv@ returns all more specidfic matches in a @MatchEnv@,
most specific first.

\begin{code}
matchMEnv :: (key {- template -} ->	-- Matching function
	      key {- instance -} ->
	      Maybe match_info)
	  -> MatchEnv key value		-- The envt
	  -> key			-- Key
	  -> [(key,
	       match_info,		-- Match info returned by matching fn
	       value)]			-- Value

matchMEnv key_match alist key
  = match alist
  where
    match [] = []
    match ((tpl, val) : rest)
      = case key_match tpl key of
	  Nothing -> case key_match key tpl of
		       Nothing         -> match rest
		       Just match_info -> (tpl, match_info, val) : match rest 
	  Just _  -> []
\end{code}

@insertMEnv@ extends a match environment, checking for overlaps.

\begin{code}
insertMEnv :: (key {- template -} ->		-- Matching function
	       key {- instance -} ->
	       Maybe match_info)
	   -> MatchEnv key value		-- Envt
	   -> key -> value			-- New item
	   -> MaybeErr (MatchEnv key value)	-- Success...
		       (key, value)		-- Failure: Offending overlap

insertMEnv match_fn alist key value
  = insert alist
  where
    -- insert has to put the new item in BEFORE any keys which are
    -- LESS SPECIFIC than the new key, and AFTER any keys which are
    -- MORE SPECIFIC The list is maintained in specific-ness order, so
    -- we just stick it in either last, or just before the first key
    -- of which the new key is an instance.  We check for overlap at
    -- that point.

    insert [] = returnMaB [(key, value)]
    insert ((t,v) : rest)
      = case (match_fn t key) of
	  Nothing ->
	    -- New key is not an instance of this existing one, so
	    -- continue down the list.
	    insert rest			`thenMaB` (\ rest' ->
	    returnMaB ((t,v):rest') )

	  Just match_info ->
	    -- New key *is* an instance of the old one, so check the
	    -- other way round in case of identity.

	    case (match_fn key t) of
	      Just _  -> failMaB (t,v)
			 -- Oops; overlap

	      Nothing -> returnMaB ((key,value):(t,v):rest)
			 -- All ok; insert here
\end{code}
