\begin{code}
module TcEnv(
	TcId, TcIdSet, tcInstId,

	TcEnv, TyThing(..), TyThingDetails(..),

	initEnv, 

	-- Getting stuff from the environment
	tcEnvTyCons, tcEnvClasses, tcEnvIds, tcEnvTcIds,
	
	-- Global environment
	tcLookupTy, tcLookupTyCon, tcLookupClass, tcLookupGlobalId, tcLookupDataCon,

	-- Local environment
	tcExtendKindEnv, tcExtendTyVarEnv, 
	tcExtendTyVarEnvForMeths, tcExtendTypeEnv, tcGetInScopeTyVars,

	-- Global type variables
	tcGetGlobalTyVars, tcExtendGlobalTyVars,

	tcExtendGlobalValEnv, tcExtendLocalValEnv,
	tcGetValueEnv,        tcSetValueEnv, 
	tcAddImportedIdInfo,

	tcLookupValue,      tcLookupValueMaybe, 
	explicitLookupValue,

	newLocalId, newSpecPragmaId,
	newDefaultMethodName, newDFunName,

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
\subsection{TcEnv}
%*									*
%************************************************************************

\begin{code}
data TcEnv
  = TcEnv {
	tcGST  	 :: GlobalSymbolTable,	-- The symbol table at the moment we began this compilation

	tcInst 	 :: InstEnv,		-- All instances (both imported and in this module)

	tcGEnv	 :: NameEnv TyThing	-- The global type environment we've accumulated while
					-- compiling this module:
					--	types and classes (both imported and local)
					-- 	imported Ids
					-- (Ids defined in this module are in the local envt)

	tcLEnv 	 :: NameEnv TcTyThing,	-- The local type environment: Ids and TyVars
					-- defined in this module

	tcTyVars :: TcRef TcTyVarSet	-- The "global tyvars"
					-- Namely, the in-scope TyVars bound in tcLEnv, plus the tyvars
					-- mentioned in the types of Ids bound in tcLEnv
					-- Why mutable? see notes with tcGetGlobalTyVars
    }

\end{code}

The Global-Env/Local-Env story
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type checking, we keep in the GlobalEnv
	* All types and classes
	* All Ids derived from types and classes (constructors, selectors)
	* Imported Ids

At the end of type checking, we zonk the local bindings,
and as we do so we add to the GlobalEnv
	* Locally defined top-level Ids

Why?  Because they are now Ids not TcIds.  This final GlobalEnv is
used thus:
	a) fed back (via the knot) to typechecking the 
	   unfoldings of interface signatures

	b) used to augment the GlobalSymbolTable


\begin{code}
data TcTyThing
  = AGlobal TyThing	-- Used only in the return type of a lookup
  | ATcId  TcId		-- Ids defined in this module
  | ATyVar TyVar	-- Type variables
  | AThing TcKind	-- Used temporarily, during kind checking
-- Here's an example of how the AThing guy is used
-- Suppose we are checking (forall a. T a Int):
--	1. We first bind (a -> AThink kv), where kv is a kind variable. 
--	2. Then we kind-check the (T a Int) part.
--	3. Then we zonk the kind variable.
--	4. Now we know the kind for 'a', and we add (a -> ATyVar a::K) to the environment

initEnv :: GlobalSymbolTable -> InstEnv -> NF_TcM TcEnv
initEnv gst inst_env
  = tcNewMutVar emptyVarSet	`thenNF_Tc` \ gtv_var ->
    returnTc (TcEnv { tcGST = gst,
		      tcGEnv = emptyNameEnv, 
		      tcInst = inst_env,
		      tcLEnv = emptyNameEnv,
		      tcTyVars = gtv_var
	     })

tcEnvClasses env = [cl | AClass cl <- nameEnvElts (tcGEnv env)]
tcEnvTyCons  env = [tc | ATyCon tc <- nameEnvElts (tcGEnv env)] 
tcEnvIds     env = [id | AnId   id <- nameEnvElts (tcGEnv env)] 
tcEnvTyVars  env = [tv | ATyVar tv <- nameEnvElts (tcLEnv env)]
tcEnvTcIds   env = [id | ATcId  id <- nameEnvElts (tcLEnv env)]

-- This data type is used to help tie the knot
-- when type checking type and class declarations
data TyThingDetails = SynTyDetails Type
		    | DataTyDetails ClassContext [DataCon] [Class]
		    | ClassDetails ClassContext [Id] [ClassOpItem] DataCon
\end{code}


%************************************************************************
%*									*
\subsection{Basic lookups}
%*									*
%************************************************************************

\begin{code}
lookup_global :: TcEnv -> Name -> Maybe TyThing
lookup_global env name 
  = 	-- Try the global envt
    case lookupNameEnv (tcGEnv env) name of {
	Just thing -> Just thing ;
	Nothing    ->

	-- Try the global symbol table
    case lookupModuleEnv (tcGST env) of {
	Nothing   -> Nothing ;
	Just genv -> lookupNameEnv genv name
    }}

lookup_local :: TcEnv -> Name -> Maybe TcTyThing
lookup_local env name
 = case lookupNameEnv (tcLEnv env) name of
	Just thing -> Just thing ;
	Nothing    -> case lookup_global env name of
			Just thing -> AGlobal thing
			Nothing	   -> Nothing
\end{code}


%************************************************************************
%*									*
\subsection{TcId}
%*									*
%************************************************************************


\begin{code}
type TcId    = Id 			-- Type may be a TcType
type TcIdSet = IdSet

-- A useful function that takes an occurrence of a global thing
-- and instantiates its type with fresh type variables
tcInstId :: Id
	 -> NF_TcM ([TcTyVar], 	-- It's instantiated type
		      TcThetaType,	--
		      TcType)		--
tcInstId id
  = let
      (tyvars, rho) = splitForAllTys (idType id)
    in
    tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', arg_tys, tenv) ->
    let
	rho'	       = substTy tenv rho
	(theta', tau') = splitRhoTy rho' 
    in
    returnNF_Tc (tyvars', theta', tau')
\end{code}


%************************************************************************
%*									*
\subsection{The global environment}
%*									*
%************************************************************************

\begin{code}
tcExtendGlobalEnv :: [(Name, TyThing)] -> TcM r -> TcM r
tcExtendGlobalEnv bindings thing_inside
  = tcGetEnv				`thenNF_Tc` \ env ->
    let
	ge' = extendNameEnvList (tcGEnv env) bindings
    in
    tcSetEnv (env {tcGEnv = ge'}) thing_inside

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
tcExtendGlobalValEnv ids thing_inside
  = tcExtendGlobalEnv [(getName id, AnId id) | id <- ids] thing_inside
\end{code}


\begin{code}
tcLookupGlobal_maybe :: Name -> NF_TcM (Maybe TyThing)
tcLookupGlobal_maybe name
  = tcGetEnv		`thenNF_Tc` \ env ->
    returnNF_Tc (lookup_global env name)
\end{code}

A variety of global lookups, when we know what we are looking for.

\begin{code}
tcLookupGlobal :: Name -> NF_TcM TyThing
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_thing ->
    case maybe_thing of
	Just thing -> returnNF_Tc thing
	other	   -> notFound "tcLookupGlobal:" name

tcLookupGlobalId :: Name -> NF_TcM Id
tcLookupGlobalId name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_id ->
    case maybe_id of
	Just (AnId clas) -> returnNF_Tc id
	other		 -> notFound "tcLookupGlobalId:" name
	
tcLookupDataCon :: Name -> TcM DataCon
tcLookupDataCon con_name
  = tcLookupGlobalId con_name		`thenNF_Tc` \ con_id ->
    case isDataConWrapId_maybe con_id of {
 	Just data_con -> returnTc data_con
	Nothing	      -> failWithTc (badCon con_id);


tcLookupClass :: Name -> NF_TcM Class
tcLookupClass name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_clas ->
    case maybe_clas of
	Just (AClass clas) -> returnNF_Tc clas
	other		   -> notFound "tcLookupClass:" name
	
tcLookupTyCon :: Name -> NF_TcM TyCon
tcLookupTyCon name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_tc ->
    case maybe_tc of
	Just (ATyCon tc) -> returnNF_Tc tc
	other		 -> notFound "tcLookupTyCon:" name
\end{code}


%************************************************************************
%*									*
\subsection{The local environment}
%*									*
%************************************************************************

\begin{code}
tcExtendKindEnv :: [(Name,TcKind)] -> TcM r -> TcM r
tcExtendKindEnv pairs thing_inside
  = tcGetEnv				`thenNF_Tc` \ env ->
    let
 	le' = extendNameEnvList (tcLEnv env) [(n, AThing k) | (n,k) <- pairs]
	-- No need to extend global tyvars for kind checking
    in
    tcSetEnv (env {tcLEnv = le'}) thing_inside
    
tcExtendTyVarEnv :: [TyVar] -> TcM r -> TcM r
tcExtendTyVarEnv tyvars thing_inside
  = tcGetEnv			`thenNF_Tc` \ env@(TcEnv {tcLEnv = le, tcTyVars = (in_scope_tvs, gtvs)}) ->
    let
 	le'           = extendNameEnvList le [ (getName tv, ATyVar tv) | tv <- tyvars]
	new_tv_set    = mkVarSet tyvars
    in
	-- It's important to add the in-scope tyvars to the global tyvar set
	-- as well.  Consider
	--	f (x::r) = let g y = y::r in ...
	-- Here, g mustn't be generalised.  This is also important during
	-- class and instance decls, when we mustn't generalise the class tyvars
	-- when typechecking the methods.
    tc_extend_gtvs gtvs new_tv_set		`thenNF_Tc` \ gtvs' ->
    tcSetEnv (env {tcLEnv = le', tcTyVars = gtvs'}) thing_inside

-- This variant, tcExtendTyVarEnvForMeths, takes *two* bunches of tyvars:
--	the signature tyvars contain the original names
--	the instance  tyvars are what those names should be mapped to
-- It's needed when typechecking the method bindings of class and instance decls
-- It does *not* extend the global tyvars; tcMethodBind does that for itself

tcExtendTyVarEnvForMeths :: [TyVar] -> [TcTyVar] -> TcM r -> TcM r
tcExtendTyVarEnvForMeths sig_tyvars inst_tyvars thing_inside
  = tcGetEnv					`thenNF_Tc` \ env ->
    let
	le'   = extendNameEnvList (tcLEnv env) stuff
	stuff = [ (getName sig_tv, ATyVar inst_tv)
		| (sig_tv, inst_tv) <- zipEqual "tcMeth" sig_tyvars inst_tyvars
		]
    in
    tcSetEnv (env {tcLEnv = le'}) thing_inside
\end{code}


\begin{code}
tcExtendLocalValEnv :: [(Name,TcId)] -> TcM a -> TcM a
tcExtendLocalValEnv names_w_ids thing_inside
  = tcGetEnv		`thenNF_Tc` \ env ->
    let
	extra_global_tyvars = tyVarsOfTypes [idType id | (name,id) <- names_w_ids]
	extra_env	    = [(name, ATcId id) | (name,id) <- names_w_ids]
	le'		    = extendNameEnvList (tcLEnv env) extra_env
    in
    tc_extend_gtvs (tcTyVars env) extra_global_tyvars	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (env {tcLEnv = le', tcTyVars = gtvs'}) thing_inside
\end{code}


%************************************************************************
%*									*
\subsection{The global tyvars}
%*									*
%************************************************************************

\begin{code}
tcExtendGlobalTyVars extra_global_tvs thing_inside
  = tcGetEnv						`thenNF_Tc` \ env ->
    tc_extend_gtvs (tcTyVars env) extra_global_tvs	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (env {tcTyVars = gtvs') thing_inside

tc_extend_gtvs gtvs extra_global_tvs
  = tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    tcNewMutVar (global_tvs `unionVarSet` extra_global_tvs)
\end{code}

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: NF_TcM TcTyVarSet
tcGetGlobalTyVars
  = tcGetEnv 					`thenNF_Tc` \ (TcEnv {tcTyVars = gtv_var}) ->
    tcReadMutVar gtv_var			`thenNF_Tc` \ global_tvs ->
    zonkTcTyVars (varSetElems global_tvs)	`thenNF_Tc` \ global_tys' ->
    let
	global_tvs' = (tyVarsOfTypes global_tys')
    in
    tcWriteMutVar gtv_var global_tvs'		`thenNF_Tc_` 
    returnNF_Tc global_tvs'
\end{code}


%************************************************************************
%*									*
\subsection{The local environment}
%*									*
%************************************************************************

\begin{code}
tcLookup_maybe :: Name -> NF_TcM (Maybe TcTyThing)
tcLookup_maybe name
  = tcGetEnv 		`thenNF_Tc` \ env ->
    returnNF_Tc (lookup_local env name)

tcLookup :: Name -> NF_TcM TcTyThing
tcLookup name
  = tcLookup_maybe name		`thenNF_Tc` \ maybe_thing ->
    case maybe_thing of
	Just thing -> returnNF_Tc thing
	other	   -> notFound "tcLookup:" name



tcGetValueEnv :: NF_TcM ValueEnv
tcGetValueEnv
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve ie gtvs) ->
    returnNF_Tc ve


tcSetValueEnv :: ValueEnv -> TcM a -> TcM a
tcSetValueEnv ve thing_inside
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te _ ie gtvs) ->
    tcSetEnv (TcEnv ue te ve ie gtvs) thing_inside

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


%************************************************************************
%*									*
\subsection{The instance environment}
%*									*
%************************************************************************

Constructing new Ids

\begin{code}
newLocalId :: OccName -> TcType -> SrcLoc -> NF_TcM TcId
newLocalId name ty loc
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkUserLocal name uniq ty loc)

newSpecPragmaId :: Name -> TcType -> NF_TcM TcId
newSpecPragmaId name ty 
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecPragmaId (nameOccName name) uniq ty (getSrcLoc name))
\end{code}

Make a name for the dict fun for an instance decl

\begin{code}
newDFunName :: Module -> Class -> [Type] -> SrcLoc -> NF_TcM Name
newDFunName mod clas (ty:_) loc
  = tcGetDFunUniq dfun_string	`thenNF_Tc` \ inst_uniq ->
    tcGetUnique			`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkGlobalName uniq mod
			      (mkDFunOcc dfun_string inst_uniq) 
			      (LocalDef loc Exported))
  where
	-- Any string that is somewhat unique will do
    dfun_string = occNameString (getOccName clas) ++ occNameString (getDFunTyKey ty)

newDefaultMethodName :: Name -> SrcLoc -> NF_TcM Name
newDefaultMethodName op_name loc
  = tcGetUnique			`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkGlobalName uniq (nameModule op_name)
			      (mkDefaultMethodOcc (getOccName op_name))
			      (LocalDef loc Exported))
\end{code}


%************************************************************************
%*									*
\subsection{The instance environment}
%*									*
%************************************************************************

\begin{code}
tcGetInstEnv :: NF_TcM InstEnv
tcGetInstEnv = tcGetEnv 	`thenNF_Tc` \ (TcEnv ue te ve ie (_,gtvs)) ->
	       returnNF_Tc ie

tcSetInstEnv :: InstEnv -> TcM a -> TcM a
tcSetInstEnv ie thing_inside
  = tcGetEnv 	`thenNF_Tc` \ (TcEnv ue te ve _ gtvs) ->
    tcSetEnv (TcEnv ue te ve ie gtvs) thing_inside
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


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badCon con_id = quotes (ppr con_id) <+> ptext SLIT("is not a data constructor")
badPrimOp op  = quotes (ppr op) <+> ptext SLIT("is not a primop")

notFound where name
  = failWithTc (text where <> colon <+> quotes (ppr name) <+> ptext SLIT("is not in scope"))
\end{code}
