\begin{code}
module TcEnv(
	TcId, TcIdSet, tcInstId,
	tcLookupDataCon,

	TcEnv, ValueEnv, TyThing(..), TyThingDetails(..), tyThingKind, 

	initEnv, getEnvTyCons, getEnvClasses, 
	
        tcExtendUVarEnv, tcLookupUVar,

	tcExtendKindEnv, tcExtendTyVarEnv, 
	tcExtendTyVarEnvForMeths, tcExtendTypeEnv, tcGetInScopeTyVars,

	tcLookupTy,
	tcLookupTyConByKey, 
	tcLookupClassByKey, tcLookupClassByKey_maybe,

	tcExtendGlobalValEnv, tcExtendLocalValEnv,
	tcGetValueEnv,        tcSetValueEnv, 
	tcAddImportedIdInfo,

	tcLookupValue,      tcLookupValueMaybe, 
	tcLookupValueByKey, tcLookupValueByKeyMaybe,
	explicitLookupValueByKey, explicitLookupValue,
	valueEnvIds,

	newLocalId, newSpecPragmaId,
	newDefaultMethodName, newDFunName,
	tcGetGlobalTyVars, tcExtendGlobalTyVars,

	InstEnv, emptyInstEnv, addToInstEnv, 
	lookupInstEnv, InstLookupResult(..),
	tcGetInstEnv, tcSetInstEnv, classInstEnv,

	badCon, badPrimOp
  ) where

#include "HsVersions.h"

import Id	( mkUserLocal, isDataConWrapId_maybe )
import MkId 	( mkSpecPragmaId )
import Var	( TyVar, Id, setVarName,
		  idType, lazySetIdInfo, idInfo, tyVarKind, UVar,
		)
import TcType	( TcType, TcTyVar, TcTyVarSet, TcThetaType,
		  tcInstTyVars, zonkTcTyVars,
		  TcKind, 
		)
import VarSet
import Type	( Kind, Type, superKind,
		  tyVarsOfType, tyVarsOfTypes,
		  splitForAllTys, splitRhoTy, splitFunTys,
		  splitAlgTyConApp_maybe, getTyVar, getDFunTyKey
		)
import Subst	( substTy )
import UsageSPUtils ( unannotTy )
import DataCon	( DataCon )
import TyCon	( TyCon, tyConKind, tyConArity, isSynTyCon )
import Class	( Class, ClassOpItem, ClassContext, classTyCon )

import TcMonad

import BasicTypes	( Arity )
import IdInfo		( vanillaIdInfo )
import Name		( Name, OccName, Provenance(..), ExportFlag(..), NamedThing(..), 
			  nameOccName, nameModule, getSrcLoc, mkGlobalName,
			  maybeWiredInTyConName, maybeWiredInIdName, isLocallyDefined,
			  NameEnv, emptyNameEnv, lookupNameEnv, nameEnvElts, 
				   extendNameEnv, extendNameEnvList
			)
import OccName		( mkDFunOcc, mkDefaultMethodOcc, occNameString )
import Module		( Module )
import Unify		( unifyTyListsX, matchTys )
import Unique		( pprUnique10, Unique, Uniquable(..) )
import UniqFM
import Unique		( Uniquable(..) )
import Util		( zipEqual, zipWith3Equal, mapAccumL )
import VarEnv		( TyVarSubstEnv )
import SrcLoc		( SrcLoc )
import FastString	( FastString )
import Maybes
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{TcId}
%*									*
%************************************************************************


\begin{code}
type TcId    = Id 			-- Type may be a TcType
type TcIdSet = IdSet

tcLookupDataCon :: Name -> TcM s (DataCon, [TcType], TcType)
tcLookupDataCon con_name
  = tcLookupValue con_name		`thenNF_Tc` \ con_id ->
    case isDataConWrapId_maybe con_id of {
	Nothing -> failWithTc (badCon con_id);
 	Just data_con ->

    tcInstId con_id			`thenNF_Tc` \ (_, _, con_tau) ->
	     -- Ignore the con_theta; overloaded constructors only
	     -- behave differently when called, not when used for
	     -- matching.
    let
	(arg_tys, result_ty) = splitFunTys con_tau
    in
    ASSERT( maybeToBool (splitAlgTyConApp_maybe result_ty) )
    returnTc (data_con, arg_tys, result_ty) }

-- A useful function that takes an occurrence of a global thing
-- and instantiates its type with fresh type variables
tcInstId :: Id
	 -> NF_TcM s ([TcTyVar], 	-- It's instantiated type
		      TcThetaType,	--
		      TcType)		--
tcInstId id
  = let
      (tyvars, rho) = splitForAllTys (unannotTy (idType id))
    in
    tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', arg_tys, tenv) ->
    let
	rho'	       = substTy tenv rho
	(theta', tau') = splitRhoTy rho' 
    in
    returnNF_Tc (tyvars', theta', tau')
\end{code}

Between the renamer and the first invocation of the UsageSP inference,
identifiers read from interface files will have usage information in
their types, whereas other identifiers will not.  The unannotTy here
in @tcInstId@ prevents this information from pointlessly propagating
further prior to the first usage inference.


%************************************************************************
%*									*
\subsection{TcEnv}
%*									*
%************************************************************************

Data type declarations
~~~~~~~~~~~~~~~~~~~~~

\begin{code}
data TcEnv = TcEnv
                  UsageEnv
		  TypeEnv
		  ValueEnv 
		  InstEnv
		  (TcTyVarSet,		-- The in-scope TyVars
		   TcRef TcTyVarSet)	-- Free type variables of the value env
					-- ...why mutable? see notes with tcGetGlobalTyVars
					-- Includes the in-scope tyvars

type UsageEnv   = NameEnv UVar
type TypeEnv	= NameEnv TyThing
type ValueEnv	= NameEnv Id	

valueEnvIds :: ValueEnv -> [Id]
valueEnvIds ve = nameEnvElts ve

data TyThing = ATyVar TyVar
	     | ATyCon TyCon
	     | AClass Class
	     | AThing TcKind	-- Used temporarily, during kind checking
-- For example, when checking (forall a. T a Int):
--	1. We first bind (a -> AThink kv), where kv is a kind variable. 
--	2. Then we kind-check the (T a Int) part.
--	3. Then we zonk the kind variable.
--	4. Now we know the kind for 'a', and we add (a -> ATyVar a::K) to the environment

tyThingKind :: TyThing -> TcKind
tyThingKind (ATyVar tv) = tyVarKind tv
tyThingKind (ATyCon tc) = tyConKind tc
tyThingKind (AClass cl) = tyConKind (classTyCon cl)	-- For some odd reason, 
							-- a class doesn't include its kind
tyThingKind (AThing k)  = k

data TyThingDetails = SynTyDetails Type
		    | DataTyDetails ClassContext [DataCon] [Class]
		    | ClassDetails ClassContext [Id] [ClassOpItem] DataCon

initEnv :: TcRef TcTyVarSet -> TcEnv
initEnv mut = TcEnv emptyNameEnv emptyNameEnv emptyNameEnv emptyInstEnv (emptyVarSet, mut)

getEnvClasses (TcEnv _ te _ _ _) = [cl | AClass cl <- nameEnvElts te]
getEnvTyCons  (TcEnv _ te _ _ _) = [tc | ATyCon tc <- nameEnvElts te]
\end{code}

%************************************************************************
%*									*
\subsection{The usage environment}
%*									*
%************************************************************************

Extending the usage environment

\begin{code}
tcExtendUVarEnv :: Name -> UVar -> TcM s r -> TcM s r
tcExtendUVarEnv uv_name uv scope
  = tcGetEnv                 `thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    tcSetEnv (TcEnv (extendNameEnv ue uv_name uv) te ve ie gtvs) scope
\end{code}

Looking up in the environments.

\begin{code}
tcLookupUVar :: Name -> NF_TcM s UVar
tcLookupUVar uv_name
  = tcGetEnv	`thenNF_Tc` \ (TcEnv ue te ve _ gtvs) ->
    case lookupNameEnv ue uv_name of
      Just uv -> returnNF_Tc uv
      Nothing -> failWithTc (uvNameOutOfScope uv_name)
\end{code}	


%************************************************************************
%*									*
\subsection{The type environment}
%*									*
%************************************************************************

\begin{code}
tcExtendKindEnv :: [(Name,TcKind)] -> TcM s r -> TcM s r
tcExtendKindEnv pairs scope
  = tcGetEnv				`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    let
 	te' = extendNameEnvList te [(n, AThing k) | (n,k) <- pairs]
	-- No need to extend global tyvars for kind checking
    in
    tcSetEnv (TcEnv ue te' ve ie gtvs) scope
    
tcExtendTyVarEnv :: [TyVar] -> TcM s r -> TcM s r
tcExtendTyVarEnv tyvars scope
  = tcGetEnv				`thenNF_Tc` \ (TcEnv ue te ve ie (in_scope_tvs, gtvs)) ->
    let
 	te'           = extendNameEnvList te [ (getName tv, ATyVar tv) | tv <- tyvars]
	new_tv_set    = mkVarSet tyvars
	in_scope_tvs' = in_scope_tvs `unionVarSet` new_tv_set
    in
	-- It's important to add the in-scope tyvars to the global tyvar set
	-- as well.  Consider
	--	f (x::r) = let g y = y::r in ...
	-- Here, g mustn't be generalised.  This is also important during
	-- class and instance decls, when we mustn't generalise the class tyvars
	-- when typechecking the methods.
    tc_extend_gtvs gtvs new_tv_set		`thenNF_Tc` \ gtvs' ->
    tcSetEnv (TcEnv ue te' ve ie (in_scope_tvs', gtvs')) scope

-- This variant, tcExtendTyVarEnvForMeths, takes *two* bunches of tyvars:
--	the signature tyvars contain the original names
--	the instance  tyvars are what those names should be mapped to
-- It's needed when typechecking the method bindings of class and instance decls
-- It does *not* extend the global tyvars; tcMethodBind does that for itself

tcExtendTyVarEnvForMeths :: [TyVar] -> [TcTyVar] -> TcM s r -> TcM s r
tcExtendTyVarEnvForMeths sig_tyvars inst_tyvars thing_inside
  = tcGetEnv					`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    let
	te' = extendNameEnvList te stuff
    in
    tcSetEnv (TcEnv ue te' ve ie gtvs) thing_inside
  where
    stuff = [ (getName sig_tv, ATyVar inst_tv)
	    | (sig_tv, inst_tv) <- zipEqual "tcMeth" sig_tyvars inst_tyvars
	    ]

tcExtendGlobalTyVars extra_global_tvs scope
  = tcGetEnv					`thenNF_Tc` \ (TcEnv ue te ve ie (in_scope,gtvs)) ->
    tc_extend_gtvs gtvs	extra_global_tvs	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (TcEnv ue te ve ie (in_scope,gtvs')) scope

tc_extend_gtvs gtvs extra_global_tvs
  = tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    let
	new_global_tyvars = global_tvs `unionVarSet` extra_global_tvs
    in
    tcNewMutVar new_global_tyvars
\end{code}

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: NF_TcM s TcTyVarSet
tcGetGlobalTyVars
  = tcGetEnv 						`thenNF_Tc` \ (TcEnv ue te ve ie (_,gtvs)) ->
    tcReadMutVar gtvs					`thenNF_Tc` \ global_tvs ->
    zonkTcTyVars (varSetElems global_tvs)		`thenNF_Tc` \ global_tys' ->
    let
	global_tvs' = (tyVarsOfTypes global_tys')
    in
    tcWriteMutVar gtvs global_tvs'			`thenNF_Tc_` 
    returnNF_Tc global_tvs'

tcGetInScopeTyVars :: NF_TcM s [TcTyVar]
tcGetInScopeTyVars
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve ie (in_scope_tvs, gtvs)) ->
    returnNF_Tc (varSetElems in_scope_tvs)
\end{code}


Type constructors and classes

\begin{code}
tcExtendTypeEnv :: [(Name, TyThing)] -> TcM s r -> TcM s r
tcExtendTypeEnv bindings scope
  = ASSERT( null [tv | (_, ATyVar tv) <- bindings] )
	-- Not for tyvars; use tcExtendTyVarEnv
    tcGetEnv				`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    let
	te' = extendNameEnvList te bindings
    in
    tcSetEnv (TcEnv ue te' ve ie gtvs) scope
\end{code}


Looking up in the environments.

\begin{code}
tcLookupTy :: Name ->  NF_TcM s TyThing
tcLookupTy name
  = tcGetEnv	`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    case lookupNameEnv te name of {
	Just thing -> returnNF_Tc thing ;
 	Nothing    -> 

    case maybeWiredInTyConName name of
	Just tc -> returnNF_Tc (ATyCon tc)

	Nothing -> 	-- This can happen if an interface-file
			-- unfolding is screwed up
		   failWithTc (tyNameOutOfScope name)
    }
	
tcLookupClassByKey :: Unique -> NF_TcM s Class
tcLookupClassByKey key
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    case lookupUFM_Directly te key of
	Just (AClass cl) -> returnNF_Tc cl
	other		 -> pprPanic "tcLookupClassByKey:" (pprUnique10 key)

tcLookupClassByKey_maybe :: Unique -> NF_TcM s (Maybe Class)
tcLookupClassByKey_maybe key
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    case lookupUFM_Directly te key of
	Just (AClass cl) -> returnNF_Tc (Just cl)
	other		 -> returnNF_Tc Nothing

tcLookupTyConByKey :: Unique -> NF_TcM s TyCon
tcLookupTyConByKey key
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    case lookupUFM_Directly te key of
	Just (ATyCon tc)  -> returnNF_Tc tc
	other		  -> pprPanic "tcLookupTyConByKey:" (pprUnique10 key)
\end{code}




%************************************************************************
%*									*
\subsection{The value environment}
%*									*
%************************************************************************

\begin{code}
tcExtendGlobalValEnv :: [Id] -> TcM s a -> TcM s a
tcExtendGlobalValEnv ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    let
	ve' = addListToUFM_Directly ve [(getUnique id, id) | id <- ids]
    in
    tcSetEnv (TcEnv ue te ve' ie gtvs) scope

tcExtendLocalValEnv :: [(Name,TcId)] -> TcM s a -> TcM s a
tcExtendLocalValEnv names_w_ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve ie (in_scope_tvs,gtvs)) ->
    tcReadMutVar gtvs	`thenNF_Tc` \ global_tvs ->
    let
	ve'		    = extendNameEnvList ve names_w_ids
	extra_global_tyvars = tyVarsOfTypes (map (idType . snd) names_w_ids)
    in
    tc_extend_gtvs gtvs extra_global_tyvars	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (TcEnv ue te ve' ie (in_scope_tvs,gtvs')) scope
\end{code}


\begin{code}
tcLookupValue :: Name -> NF_TcM s Id	-- Panics if not found
tcLookupValue name
  = case maybeWiredInIdName name of
	Just id -> returnNF_Tc id
	Nothing -> tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
		   returnNF_Tc (lookupWithDefaultUFM ve def name)
  where
    def = pprPanic "tcLookupValue:" (ppr name)

tcLookupValueMaybe :: Name -> NF_TcM s (Maybe Id)
tcLookupValueMaybe name
  = case maybeWiredInIdName name of
	Just id -> returnNF_Tc (Just id)
	Nothing -> tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
		   returnNF_Tc (lookupNameEnv ve name)

tcLookupValueByKey :: Unique -> NF_TcM s Id	-- Panics if not found
tcLookupValueByKey key
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    returnNF_Tc (explicitLookupValueByKey ve key)

tcLookupValueByKeyMaybe :: Unique -> NF_TcM s (Maybe Id)
tcLookupValueByKeyMaybe key
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    returnNF_Tc (lookupUFM_Directly ve key)

tcGetValueEnv :: NF_TcM s ValueEnv
tcGetValueEnv
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    returnNF_Tc ve


tcSetValueEnv :: ValueEnv -> TcM s a -> TcM s a
tcSetValueEnv ve scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te _ ie gtvs) ->
    tcSetEnv (TcEnv ue te ve ie gtvs) scope

-- Non-monadic version, environment given explicitly
explicitLookupValueByKey :: ValueEnv -> Unique -> Id
explicitLookupValueByKey ve key
  = lookupWithDefaultUFM_Directly ve def key
  where
    def = pprPanic "lookupValueByKey:" (pprUnique10 key)

explicitLookupValue :: ValueEnv -> Name -> Maybe Id
explicitLookupValue ve name
  = case maybeWiredInIdName name of
	Just id -> Just id
	Nothing -> lookupNameEnv ve name

	-- Extract the IdInfo from an IfaceSig imported from an interface file
tcAddImportedIdInfo :: ValueEnv -> Id -> Id
tcAddImportedIdInfo unf_env id
  | isLocallyDefined id		-- Don't look up locally defined Ids, because they
				-- have explicit local definitions, so we get a black hole!
  = id
  | otherwise
  = id `lazySetIdInfo` new_info
	-- The Id must be returned without a data dependency on maybe_id
  where
    new_info = case explicitLookupValue unf_env (getName id) of
		     Nothing	      -> vanillaIdInfo
		     Just imported_id -> idInfo imported_id
		-- ToDo: could check that types are the same
\end{code}

Constructing new Ids

\begin{code}
newLocalId :: OccName -> TcType -> SrcLoc -> NF_TcM s TcId
newLocalId name ty loc
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkUserLocal name uniq ty loc)

newSpecPragmaId :: Name -> TcType -> NF_TcM s TcId
newSpecPragmaId name ty 
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecPragmaId (nameOccName name) uniq ty (getSrcLoc name))
\end{code}


%************************************************************************
%*									*
\subsection{The instance environment}
%*									*
%************************************************************************

\begin{code}
tcGetInstEnv :: NF_TcM s InstEnv
tcGetInstEnv = tcGetEnv 	`thenNF_Tc` \ (TcEnv ue te ve ie (_,gtvs)) ->
	       returnNF_Tc ie

tcSetInstEnv :: InstEnv -> TcM s a -> TcM s a
tcSetInstEnv ie scope
  = tcGetEnv 	`thenNF_Tc` \ (TcEnv ue te ve _ gtvs) ->
    tcSetEnv (TcEnv ue te ve ie gtvs) scope
\end{code}    


\begin{code}
type InstEnv    = UniqFM ClsInstEnv		-- Maps Class to instances for that class
type ClsInstEnv = [(TyVarSet, [Type], Id)]	-- The instances for a particular class

classInstEnv :: InstEnv -> Class -> ClsInstEnv
classInstEnv env cls = lookupWithDefaultUFM env [] cls
\end{code}

A @ClsInstEnv@ lives inside a class, and identifies all the instances
of that class.  The @Id@ inside a ClsInstEnv mapping is the dfun for
that instance.  

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

	forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

	instance (...) => C t1 t2 t3,  witnessed by dfun

There is an important consistency constraint in the elements of a ClsInstEnv:

  * [a,b] must be a superset of the free vars of [t1,t2,t3]

  * The dfun must itself be quantified over [a,b]

Thus, the @ClassInstEnv@ for @Eq@ might contain the following entry:
	[a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
The "a" in the pattern must be one of the forall'd variables in
the dfun type.



Notes on overlapping instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In some ClsInstEnvs, overlap is prohibited; that is, no pair of templates unify.

In others, overlap is permitted, but only in such a way that one can make
a unique choice when looking up.  That is, overlap is only permitted if
one template matches the other, or vice versa.  So this is ok:

  [a]  [Int]

but this is not

  (Int,a)  (b,Int)

If overlap is permitted, the list is kept most specific first, so that
the first lookup is the right choice.


For now we just use association lists.

\subsection{Avoiding a problem with overlapping}

Consider this little program:

\begin{pseudocode}
     class C a        where c :: a
     class C a => D a where d :: a

     instance C Int where c = 17
     instance D Int where d = 13

     instance C a => C [a] where c = [c]
     instance ({- C [a], -} D a) => D [a] where d = c

     instance C [Int] where c = [37]

     main = print (d :: [Int])
\end{pseudocode}

What do you think `main' prints  (assuming we have overlapping instances, and
all that turned on)?  Well, the instance for `D' at type `[a]' is defined to
be `c' at the same type, and we've got an instance of `C' at `[Int]', so the
answer is `[37]', right? (the generic `C [a]' instance shouldn't apply because
the `C [Int]' instance is more specific).

Ghc-4.04 gives `[37]', while ghc-4.06 gives `[17]', so 4.06 is wrong.  That
was easy ;-)  Let's just consult hugs for good measure.  Wait - if I use old
hugs (pre-September99), I get `[17]', and stranger yet, if I use hugs98, it
doesn't even compile!  What's going on!?

What hugs complains about is the `D [a]' instance decl.

\begin{pseudocode}
     ERROR "mj.hs" (line 10): Cannot build superclass instance
     *** Instance            : D [a]
     *** Context supplied    : D a
     *** Required superclass : C [a]
\end{pseudocode}

You might wonder what hugs is complaining about.  It's saying that you
need to add `C [a]' to the context of the `D [a]' instance (as appears
in comments).  But there's that `C [a]' instance decl one line above
that says that I can reduce the need for a `C [a]' instance to the
need for a `C a' instance, and in this case, I already have the
necessary `C a' instance (since we have `D a' explicitly in the
context, and `C' is a superclass of `D').

Unfortunately, the above reasoning indicates a premature commitment to the
generic `C [a]' instance.  I.e., it prematurely rules out the more specific
instance `C [Int]'.  This is the mistake that ghc-4.06 makes.  The fix is to
add the context that hugs suggests (uncomment the `C [a]'), effectively
deferring the decision about which instance to use.

Now, interestingly enough, 4.04 has this same bug, but it's covered up
in this case by a little known `optimization' that was disabled in
4.06.  Ghc-4.04 silently inserts any missing superclass context into
an instance declaration.  In this case, it silently inserts the `C
[a]', and everything happens to work out.

(See `basicTypes/MkId:mkDictFunId' for the code in question.  Search for
`Mark Jones', although Mark claims no credit for the `optimization' in
question, and would rather it stopped being called the `Mark Jones
optimization' ;-)

So, what's the fix?  I think hugs has it right.  Here's why.  Let's try
something else out with ghc-4.04.  Let's add the following line:

    d' :: D a => [a]
    d' = c

Everyone raise their hand who thinks that `d :: [Int]' should give a
different answer from `d' :: [Int]'.  Well, in ghc-4.04, it does.  The
`optimization' only applies to instance decls, not to regular
bindings, giving inconsistent behavior.

Old hugs had this same bug.  Here's how we fixed it: like GHC, the
list of instances for a given class is ordered, so that more specific
instances come before more generic ones.  For example, the instance
list for C might contain:
    ..., C Int, ..., C a, ...  
When we go to look for a `C Int' instance we'll get that one first.
But what if we go looking for a `C b' (`b' is unconstrained)?  We'll
pass the `C Int' instance, and keep going.  But if `b' is
unconstrained, then we don't know yet if the more specific instance
will eventually apply.  GHC keeps going, and matches on the generic `C
a'.  The fix is to, at each step, check to see if there's a reverse
match, and if so, abort the search.  This prevents hugs from
prematurely chosing a generic instance when a more specific one
exists.

--Jeff

\begin{code}
emptyInstEnv :: InstEnv
emptyInstEnv = emptyUFM
\end{code}

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since
the env is kept ordered, the first match must be the only one.  The
thing we are looking up can have an arbitrary "flexi" part.

\begin{code}
lookupInstEnv :: InstEnv 			-- The envt
	      -> Class -> [Type]	-- Key
	      -> InstLookupResult

data InstLookupResult 
  = FoundInst 			-- There is a (template,substitution) pair 
				-- that makes the template match the key, 
				-- and no template is an instance of the key
	TyVarSubstEnv Id

  | NoMatch Bool	-- Boolean is true iff there is at least one
			-- template that matches the key.
			-- (but there are other template(s) that are
			--  instances of the key, so we don't report 
			--  FoundInst)
	-- The NoMatch True case happens when we look up
	--	Foo [a]
	-- in an InstEnv that has entries for
	--	Foo [Int]
	--	Foo [b]
	-- Then which we choose would depend on the way in which 'a'
	-- is instantiated.  So we say there is no match, but identify
	-- it as ambiguous case in the hope of giving a better error msg.
	-- See the notes above from Jeff Lewis

lookupInstEnv env key_cls key_tys
  = find (classInstEnv env key_cls)
  where
    key_vars = tyVarsOfTypes key_tys

    find [] = NoMatch False
    find ((tpl_tyvars, tpl, val) : rest)
      = case matchTys tpl_tyvars tpl key_tys of
	  Nothing                 ->
	    case matchTys key_vars key_tys tpl of
	      Nothing             -> find rest
	      Just (_, _)         -> NoMatch (any_match rest)
	  Just (subst, leftovers) -> ASSERT( null leftovers )
				     FoundInst subst val

    any_match rest = or [ maybeToBool (matchTys tvs tpl key_tys)
		        | (tvs,tpl,_) <- rest
			]
\end{code}

@addToClsInstEnv@ extends a @ClsInstEnv@, checking for overlaps.

A boolean flag controls overlap reporting.

True => overlap is permitted, but only if one template matches the other;
        not if they unify but neither is 

\begin{code}
addToInstEnv :: Bool                   			-- True <=> overlap permitted
             -> InstEnv					-- Envt
	     -> Class -> [TyVar] -> [Type] -> Id	-- New item
	     -> MaybeErr InstEnv		 	-- Success...
		         ([Type], Id)			-- Failure: Offending overlap

addToInstEnv overlap_ok inst_env clas ins_tvs ins_tys value
  = case insert_into (classInstEnv inst_env clas) of
	Failed stuff 	  -> Failed stuff
	Succeeded new_env -> Succeeded (addToUFM inst_env clas new_env)
	
  where
    ins_tv_set = mkVarSet ins_tvs
    ins_item = (ins_tv_set, ins_tys, value)

    insert_into [] = returnMaB [ins_item]
    insert_into env@(cur_item@(tpl_tvs, tpl_tys, val) : rest)

	-- FAIL if:
	-- (a) they are the same, or
	-- (b) they unify, and any sort of overlap is prohibited,
	-- (c) they unify but neither is more specific than t'other
      |  identical 
      || (unifiable && not overlap_ok)
      || (unifiable && not (ins_item_more_specific || cur_item_more_specific))
      =  failMaB (tpl_tys, val)

	-- New item is an instance of current item, so drop it here
      | ins_item_more_specific	= returnMaB (ins_item : env)

	-- Otherwise carry on
      | otherwise  = insert_into rest     `thenMaB` \ rest' ->
                     returnMaB (cur_item : rest')
      where
        unifiable = maybeToBool (unifyTyListsX (ins_tv_set `unionVarSet` tpl_tvs) tpl_tys ins_tys)
        ins_item_more_specific = maybeToBool (matchTys tpl_tvs    tpl_tys ins_tys)
        cur_item_more_specific = maybeToBool (matchTys ins_tv_set ins_tys tpl_tys)
	identical = ins_item_more_specific && cur_item_more_specific
\end{code}

Make a name for the dict fun for an instance decl

\begin{code}
newDFunName :: Module -> Class -> [Type] -> SrcLoc -> NF_TcM s Name
newDFunName mod clas (ty:_) loc
  = tcGetDFunUniq dfun_string	`thenNF_Tc` \ inst_uniq ->
    tcGetUnique			`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkGlobalName uniq mod
			      (mkDFunOcc dfun_string inst_uniq) 
			      (LocalDef loc Exported))
  where
	-- Any string that is somewhat unique will do
    dfun_string = occNameString (getOccName clas) ++ occNameString (getDFunTyKey ty)

newDefaultMethodName :: Name -> SrcLoc -> NF_TcM s Name
newDefaultMethodName op_name loc
  = tcGetUnique			`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkGlobalName uniq (nameModule op_name)
			      (mkDefaultMethodOcc (getOccName op_name))
			      (LocalDef loc Exported))
\end{code}


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badCon con_id
  = quotes (ppr con_id) <+> ptext SLIT("is not a data constructor")
badPrimOp op
  = quotes (ppr op) <+> ptext SLIT("is not a primop")

uvNameOutOfScope name
  = ptext SLIT("UVar") <+> quotes (ppr name) <+> ptext SLIT("is not in scope")

tyNameOutOfScope name
  = quotes (ppr name) <+> ptext SLIT("is not in scope")
\end{code}
