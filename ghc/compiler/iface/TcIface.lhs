%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
module TcIface ( 
	tcImportDecl, typecheckIface,
	loadImportedInsts, loadImportedRules,
	tcExtCoreBindings
 ) where
#include "HsVersions.h"

import IfaceSyn
import LoadIface	( loadHomeInterface, predInstGates )
import IfaceEnv		( lookupIfaceTop, newGlobalBinder, lookupOrig,
			  extendIfaceIdEnv, extendIfaceTyVarEnv, newIPName,
			  tcIfaceTyVar, tcIfaceTyCon, tcIfaceClass, tcIfaceExtId,
			  tcIfaceDataCon, tcIfaceLclId,
			  newIfaceName, newIfaceNames )
import BuildTyCl	( buildSynTyCon, buildAlgTyCon, buildDataCon, buildClass )
import TcRnMonad
import Type		( liftedTypeKind, splitTyConApp, 
			  mkTyVarTys, mkGenTyConApp, mkTyVarTys, ThetaType, pprClassPred )
import TypeRep		( Type(..), PredType(..) )
import TyCon		( TyCon, tyConName )
import HscTypes		( ExternalPackageState(..), PackageInstEnv, PackageRuleBase,
			  HscEnv, TyThing(..), implicitTyThings, typeEnvIds,
			  ModIface(..), ModDetails(..), InstPool, ModGuts,
			  TypeEnv, mkTypeEnv, extendTypeEnvList, lookupTypeEnv,
			  RulePool, Pool(..) )
import InstEnv		( extendInstEnv )
import CoreSyn
import PprCore		( pprIdRules )
import Rules		( extendRuleBaseList )
import CoreUtils	( exprType )
import CoreUnfold
import CoreLint		( lintUnfolding )
import WorkWrap		( mkWrapper )
import InstEnv		( DFunId )
import Id		( Id, mkVanillaGlobal, mkLocalId )
import MkId		( mkFCallId )
import IdInfo		( IdInfo, CafInfo(..), WorkerInfo(..), 
			  setUnfoldingInfoLazily, setAllStrictnessInfo, setWorkerInfo,
			  setArityInfo, setInlinePragInfo, setCafInfo, 
			  vanillaIdInfo, newStrictnessInfo )
import Class		( Class )
import TyCon		( DataConDetails(..), tyConDataCons, tyConTyVars, isTupleTyCon, mkForeignTyCon )
import DataCon		( dataConWorkId, dataConExistentialTyVars, dataConArgTys )
import TysWiredIn	( tupleCon )
import Var		( TyVar, mkTyVar, tyVarKind )
import Name		( Name, NamedThing(..), nameModuleName, nameModule, nameOccName, 
			  isWiredInName, wiredInNameTyThing_maybe, nameParent )
import NameEnv
import OccName		( OccName )
import Module		( Module, ModuleName, moduleName )
import UniqSupply	( initUs_ )
import Outputable	
import SrcLoc		( noSrcLoc )
import Util		( zipWithEqual, dropList, equalLength, zipLazy )
import Maybes		( expectJust )
import CmdLineOpts	( DynFlag(..) )
\end{code}

This module takes

	IfaceDecl -> TyThing
	IfaceType -> Type
	etc

An IfaceDecl is populated with RdrNames, and these are not renamed to
Names before typechecking, because there should be no scope errors etc.

	-- For (b) consider: f = $(...h....)
	-- where h is imported, and calls f via an hi-boot file.  
	-- This is bad!  But it is not seen as a staging error, because h
	-- is indeed imported.  We don't want the type-checker to black-hole 
	-- when simplifying and compiling the splice!
	--
	-- Simple solution: discard any unfolding that mentions a variable
	-- bound in this module (and hence not yet processed).
	-- The discarding happens when forkM finds a type error.

%************************************************************************
%*									*
%*	tcImportDecl is the key function for "faulting in" 		*
%*	imported things
%*									*
%************************************************************************

The main idea is this.  We are chugging along type-checking source code, and
find a reference to GHC.Base.map.  We call tcLookupGlobal, which doesn't find
it in the EPS type envt.  So it 
	1 loads GHC.Base.hi
	2 gets the decl for GHC.Base.map
	3 typechecks it via tcIfaceDecl
	4 and adds it to the type env in the EPS

Note that DURING STEP 4, we may find that map's type mentions a type 
constructor that also 

Notice that for imported things we read the current version from the EPS
mutable variable.  This is important in situations like
	...$(e1)...$(e2)...
where the code that e1 expands to might import some defns that 
also turn out to be needed by the code that e2 expands to.

\begin{code}
tcImportDecl :: Name -> IfG TyThing
-- Get the TyThing for this Name from an interface file
tcImportDecl name
  = do	{ 
    -- Make sure the interface is loaded
	; let { nd_doc = ptext SLIT("Need decl for") <+> ppr name }
	; traceIf (nd_doc <+> char '{')		-- Brace matches the later message
	; loadHomeInterface nd_doc name

    -- Get the real name of the thing, with a correct nameParent field.
    -- Before the interface is loaded, we may have a non-committal 'Nothing'
    -- in the namePareent field (made up by IfaceEnv.lookupOrig), but 
    -- loading the interface updates the name cache.
    -- We need the right nameParent field in getThing
  	; real_name <- lookupOrig (nameModuleName name) (nameOccName name)

    -- Get the decl out of the EPS
	; main_thing <- ASSERT( real_name == name )	-- Unique should not change!
			getThing real_name

    -- Record the import in the type env, 
    -- slurp any rules it allows in
	; recordImportOf main_thing

	; let { extra | getName main_thing == real_name = empty
		      | otherwise = brackets (ptext SLIT("when seeking") <+> ppr real_name) }
	; traceIf (ptext SLIT(" ...imported decl for") <+> ppr main_thing <+> extra <+> char '}')


    -- Look up the wanted Name in the type envt; it might be
    -- one of the subordinate members of the input thing
	; if real_name == getName main_thing 
	  then return main_thing
	  else do
	{ eps <- getEps
	; return (expectJust "tcImportDecl" $
		  lookupTypeEnv (eps_PTE eps) real_name) }}

recordImportOf :: TyThing -> IfG ()
-- Update the EPS to record the import of the Thing
--   (a) augment the type environment; this is done even for wired-in 
--	 things, so that we don't go through this rigmarole a second time
--   (b) slurp in any rules to maintain the invariant that any rule
--	     whose gates are all in the type envt, is in eps_rule_base

recordImportOf thing
  = do 	{ new_things <- updateEps (\ eps -> 
	    let { new_things   = thing : implicitTyThings thing 
	 	; new_type_env = extendTypeEnvList (eps_PTE eps) new_things
		-- NB: opportunity for a very subtle loop here!
		-- If working out what the implicitTyThings are involves poking
		-- any of the fork'd thunks in 'thing', then here's what happens	
		--	* recordImportOf succeed, extending type-env with a thunk
		--	* the next guy to pull on type-env forces the thunk
		--	* which pokes the suspended forks
		--	* which, to execute, need to consult type-env (to check
		--	  entirely unrelated types, perhaps)
	    }
	    in (eps { eps_PTE = new_type_env }, new_things)
	  )
	; traceIf (text "tcImport: extend type env" <+> ppr new_things)
	}
	
getThing :: Name -> IfG TyThing
-- Find and typecheck the thing; the Name might be a "subordinate name"
-- of the "main thing" (e.g. the constructor of a data type declaration)
-- The Thing we return is the parent "main thing"

getThing name
  | Just thing <- wiredInNameTyThing_maybe name
   = return thing

  | otherwise = do	-- The normal case, not wired in
  {	-- Get the decl from the pool
    mb_decl <- updateEps (\ eps -> selectDecl eps name)

    ; case mb_decl of
	Just decl -> initIfaceLcl (nameModuleName name) (tcIfaceDecl decl)
		-- Typecheck it
		-- Side-effects EPS by faulting in any needed decls
		-- (via nested calls to tcImportDecl)
		     

	Nothing -> do { ioToIOEnv (printErrs (msg defaultErrStyle)); failM }
		-- Declaration not found
		-- No errors-var to accumulate errors in, so just
		-- print out the error right now
		     
    }
  where
     msg = hang (ptext SLIT("Can't find interface-file declaration for") <+> ppr (nameParent name))
	      2 (vcat [ptext SLIT("Probable cause: bug in .hi-boot file, or inconsistent .hi file"),
		       ptext SLIT("Use -ddump-if-trace to get an idea of which file caused the error")])

selectDecl :: ExternalPackageState -> Name -> (ExternalPackageState, Maybe IfaceDecl)
-- Use nameParent to get the parent name of the thing
selectDecl eps@(EPS { eps_decls = Pool decls_map n_in n_out}) name
   = case lookupNameEnv decls_map main_name of
	Nothing   -> (eps, Nothing)
	Just decl -> (eps {eps_decls = Pool decls' n_in (n_out+1)}, Just decl)
   where
     main_name = nameParent name
     decls'    = delFromNameEnv decls_map main_name
\end{code}

%************************************************************************
%*									*
		Type-checking a complete interface
%*									*
%************************************************************************

Suppose we discover we don't need to recompile.  Then we must type
check the old interface file.  This is a bit different to the
incremental type checking we do as we suck in interface files.  Instead
we do things similarly as when we are typechecking source decls: we
bring into scope the type envt for the interface all at once, using a
knot.  Remember, the decls aren't necessarily in dependency order --
and even if they were, the type decls might be mutually recursive.

\begin{code}
typecheckIface :: HscEnv
	       -> ModIface 	-- Get the decls from here
	       -> IO ModDetails
typecheckIface hsc_env iface@(ModIface { mi_module = mod, mi_decls = ver_decls,
				         mi_rules = rules, mi_insts = dfuns })
  = initIfaceTc hsc_env iface $ \ tc_env_var -> do
	{ 	-- Typecheck the decls
	  names <- mappM (lookupOrig (moduleName mod) . ifName) decls
	; ty_things <- fixM (\ rec_ty_things -> do
		{ writeMutVar tc_env_var (mkNameEnv (names `zipLazy` rec_ty_things))
			-- This only makes available the "main" things,
			-- but that's enough for the strictly-checked part
		; mapM tcIfaceDecl decls })
	
		-- Now augment the type envt with all the implicit things
		-- These will be needed when type-checking the unfoldings for
		-- the IfaceIds, but this is done lazily, so writing the thing
		-- now is sufficient
	; let	{ add_implicits main_thing = main_thing : implicitTyThings main_thing
		; type_env = mkTypeEnv (concatMap add_implicits ty_things) }
	; writeMutVar tc_env_var type_env

		-- Now do those rules and instances
	; dfuns <- mapM tcIfaceInst (mi_insts iface)
	; rules <- mapM tcIfaceRule (mi_rules iface)

		-- Finished
	; return (ModDetails { md_types = type_env, md_insts = dfuns, md_rules = rules }) 
    }
  where
    decls = map snd ver_decls
\end{code}


%************************************************************************
%*									*
		Type and class declarations
%*									*
%************************************************************************

When typechecking a data type decl, we *lazily* (via forkM) typecheck
the constructor argument types.  This is in the hope that we may never
poke on those argument types, and hence may never need to load the
interface files for types mentioned in the arg types.

E.g.	
	data Foo.S = MkS Baz.T
Mabye we can get away without even loading the interface for Baz!

This is not just a performance thing.  Suppose we have
	data Foo.S = MkS Baz.T
	data Baz.T = MkT Foo.S
(in different interface files, of course).
Now, first we load and typecheck Foo.S, and add it to the type envt.  
If we do explore MkS's argument, we'll load and typecheck Baz.T.
If we explore MkT's argument we'll find Foo.S already in the envt.  

If we typechecked constructor args eagerly, when loading Foo.S we'd try to
typecheck the type Baz.T.  So we'd fault in Baz.T... and then need Foo.S...
which isn't done yet.

All very cunning. However, there is a rather subtle gotcha which bit
me when developing this stuff.  When we typecheck the decl for S, we
extend the type envt with S, MkS, and all its implicit Ids.  Suppose
(a bug, but it happened) that the list of implicit Ids depended in
turn on the constructor arg types.  Then the following sequence of
events takes place:
	* we build a thunk <t> for the constructor arg tys
	* we build a thunk for the extended type environment (depends on <t>)
	* we write the extended type envt into the global EPS mutvar
	
Now we look something up in the type envt
	* that pulls on <t>
	* which reads the global type envt out of the global EPS mutvar
	* but that depends in turn on <t>

It's subtle, because, it'd work fine if we typechecked the constructor args 
eagerly -- they don't need the extended type envt.  They just get the extended
type envt by accident, because they look at it later.

What this means is that the implicitTyThings MUST NOT DEPEND on any of
the forkM stuff.


\begin{code}
tcIfaceDecl :: IfaceDecl -> IfL TyThing

tcIfaceDecl (IfaceId {ifName = occ_name, ifType = iface_type, ifIdInfo = info})
  = do	{ name <- lookupIfaceTop occ_name
	; ty <- tcIfaceType iface_type
	; info <- tcIdInfo name ty info
	; return (AnId (mkVanillaGlobal name ty info)) }

tcIfaceDecl (IfaceData {ifND = new_or_data, ifName = occ_name, 
			ifTyVars = tv_bndrs, ifCtxt = rdr_ctxt,
			ifCons = rdr_cons, 
			ifVrcs = arg_vrcs, ifRec = is_rec, 
			ifGeneric = want_generic })
  = do	{ tc_name <- lookupIfaceTop occ_name
	; bindIfaceTyVars tv_bndrs $ \ tyvars -> do

	{ traceIf (text "tcIfaceDecl" <+> ppr rdr_ctxt)

	; ctxt <- forkM (ptext SLIT("Ctxt of data decl") <+> ppr tc_name) $
		     tcIfaceCtxt rdr_ctxt
	 	-- The reason for laziness here is to postpone
		-- looking at the context, because the class may not
		-- be in the type envt yet.  E.g. 
		--	class Real a where { toRat :: a -> Ratio Integer }
		--	data (Real a) => Ratio a = ...
		-- We suck in the decl for Real, and type check it, which sucks
		-- in the data type Ratio; but we must postpone typechecking the
		-- context

	; tycon <- fixM ( \ tycon -> do
	    { cons <- tcIfaceDataCons tycon tyvars ctxt rdr_cons
	    ; tycon <- buildAlgTyCon new_or_data tc_name tyvars ctxt cons 
			    arg_vrcs is_rec want_generic
	    ; return tycon
	    })
        ; traceIf (text "tcIfaceDecl4" <+> ppr tycon)
	; return (ATyCon tycon)
    } }

tcIfaceDecl (IfaceSyn {ifName = occ_name, ifTyVars = tv_bndrs, 
		       ifSynRhs = rdr_rhs_ty, ifVrcs = arg_vrcs})
   = bindIfaceTyVars tv_bndrs $ \ tyvars -> do
     { tc_name <- lookupIfaceTop occ_name
     ; rhs_ty <- tcIfaceType rdr_rhs_ty
     ; return (ATyCon (buildSynTyCon tc_name tyvars rhs_ty arg_vrcs))
     }

tcIfaceDecl (IfaceClass {ifCtxt = rdr_ctxt, ifName = occ_name, ifTyVars = tv_bndrs, 
			 ifFDs = rdr_fds, ifSigs = rdr_sigs, 
			 ifVrcs = tc_vrcs, ifRec = tc_isrec })
  = bindIfaceTyVars tv_bndrs $ \ tyvars -> do
    { cls_name <- lookupIfaceTop occ_name
    ; ctxt <- tcIfaceCtxt rdr_ctxt
    ; sigs <- mappM tc_sig rdr_sigs
    ; fds  <- mappM tc_fd rdr_fds
    ; cls  <- buildClass cls_name tyvars ctxt fds sigs tc_isrec tc_vrcs
    ; return (AClass cls) }
  where
   tc_sig (IfaceClassOp occ dm rdr_ty)
     = do { op_name <- lookupIfaceTop occ
	  ; op_ty   <- forkM (mk_doc op_name rdr_ty) (tcIfaceType rdr_ty)
		-- Must be done lazily for just the same reason as the 
		-- context of a data decl: the type sig might mention the
		-- class being defined
	  ; return (op_name, dm, op_ty) }

   mk_doc op_name op_ty = ptext SLIT("Class op") <+> sep [ppr op_name, ppr op_ty]

   tc_fd (tvs1, tvs2) = do { tvs1' <- mappM tcIfaceTyVar tvs1
			   ; tvs2' <- mappM tcIfaceTyVar tvs2
			   ; return (tvs1', tvs2') }

tcIfaceDecl (IfaceForeign {ifName = rdr_name, ifExtName = ext_name})
  = do	{ name <- lookupIfaceTop rdr_name
	; return (ATyCon (mkForeignTyCon name ext_name 
					 liftedTypeKind 0 [])) }

tcIfaceDataCons tycon tyvars ctxt Unknown
  = returnM Unknown

tcIfaceDataCons tycon tyvars ctxt (DataCons cs)
  = mappM tc_con_decl cs	`thenM` \ data_cons ->
    returnM (DataCons data_cons)
  where
    tc_con_decl (IfaceConDecl occ ex_tvs ex_ctxt args stricts field_lbls)
      = bindIfaceTyVars ex_tvs	$ \ ex_tyvars -> do
	{ name <- lookupIfaceTop occ
	; ex_theta <- tcIfaceCtxt ex_ctxt	-- Laziness seems not worth the bother here

	-- Read the argument types, but lazily to avoid faulting in
	-- the component types unless they are really needed
 	; arg_tys <- forkM (mk_doc name args) (mappM tcIfaceType args) ;

	; lbl_names <- mappM lookupIfaceTop field_lbls

	; buildDataCon name stricts lbl_names
		       tyvars ctxt ex_tyvars ex_theta 
		       arg_tys tycon
	}
    mk_doc con_name args = ptext SLIT("Constructor") <+> sep [ppr con_name, ppr args]
\end{code}	


%************************************************************************
%*									*
		Instances
%*									*
%************************************************************************

The gating story for instance declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are looking for a dict (C t1..tn), we slurp in instance decls for
C that 
	mention at least one of the type constructors 
	at the roots of t1..tn

Why "at least one" rather than "all"?  Because functional dependencies 
complicate the picture.  Consider
	class C a b | a->b where ...
	instance C Foo Baz where ...
Here, the gates are really only C and Foo, *not* Baz.
That is, if C and Foo are visible, even if Baz isn't, we must
slurp the decl, even if Baz is thus far completely unknown to the
system.

Why "roots of the types"?  Reason is overlap.  For example, suppose there 
are interfaces in the pool for
  (a)	C Int b
 (b)	C a [b]
  (c)	C a [T] 
Then, if we are trying to resolve (C Int x), we need (a)
if we are trying to resolve (C x [y]), we need *both* (b) and (c),
even though T is not involved yet, so that we spot the overlap.


NOTE: if you use an instance decl with NO type constructors
	instance C a where ...
and look up an Inst that only has type variables such as (C (n o))
then GHC won't necessarily suck in the instances that overlap with this.


\begin{code}
loadImportedInsts :: Class -> [Type] -> TcM PackageInstEnv
loadImportedInsts cls tys
  = do	{ 	-- Get interfaces for wired-in things, such as Integer
		-- Any non-wired-in tycons will already be loaded, else
		-- we couldn't have them in the Type
	; this_mod <- getModule 
	; let { (cls_gate, tc_gates) = predInstGates cls tys
	      ; imp_wi n = isWiredInName n && this_mod /= nameModule n
	      ;	wired_tcs = filter imp_wi tc_gates }
			-- Wired-in tycons not from this module.  The "this-module"
			-- test bites only when compiling Base etc, because loadHomeInterface
			-- barfs if it's asked to load a non-existent interface
	; if null wired_tcs then returnM ()
	  else initIfaceTcRn (mapM_ (loadHomeInterface wired_doc) wired_tcs)

	; eps_var <- getEpsVar
	; eps <- readMutVar eps_var

	-- Suck in the instances
	; let { (inst_pool', iface_insts) 
		    = WARN( null tc_gates, ptext SLIT("Interesting! No tycons in Inst:") 
						<+> pprClassPred cls tys )
		      selectInsts (eps_insts eps) cls_gate tc_gates }

	-- Empty => finish up rapidly, without writing to eps
	; if null iface_insts then
		return (eps_inst_env eps)
	  else do
	{ writeMutVar eps_var (eps {eps_insts = inst_pool'})

	; traceIf (sep [ptext SLIT("Importing instances for") <+> pprClassPred cls tys, 
			nest 2 (vcat (map ppr iface_insts))])

	-- Typecheck the new instances
	; dfuns <- initIfaceTcRn (mappM tc_inst iface_insts)

	-- And put them in the package instance environment
	; updateEps ( \ eps ->
	    let 
		inst_env' = foldl extendInstEnv (eps_inst_env eps) dfuns
	    in
	    (eps { eps_inst_env = inst_env' }, inst_env')
	)}}
  where
    wired_doc = ptext SLIT("Need home inteface for wired-in thing")

tc_inst (mod, inst) = initIfaceLcl mod (tcIfaceInst inst)

tcIfaceInst :: IfaceInst -> IfL DFunId
tcIfaceInst (IfaceInst { ifDFun = dfun_occ })
  = tcIfaceExtId (LocalTop dfun_occ)

selectInsts :: InstPool -> Name -> [Name] -> (InstPool, [(ModuleName, IfaceInst)])
selectInsts pool@(Pool insts n_in n_out) cls tycons
  = (Pool insts' n_in (n_out + length iface_insts), iface_insts)
  where
    (insts', iface_insts) 
	= case lookupNameEnv insts cls of {
		Nothing -> (insts, []) ;
		Just gated_insts ->
	
	  case choose1 gated_insts  of {
	    (_, []) -> (insts, []) ;	-- None picked
	    (gated_insts', iface_insts') -> 

	  (extendNameEnv insts cls gated_insts', iface_insts') }}

    choose1 gated_insts
	| null tycons 			-- Bizarre special case of C (a b); then there are no tycons
	= ([], map snd gated_insts)	-- Just grab all the instances, no real alternative
	| otherwise 			-- Normal case
	= foldl choose2 ([],[]) gated_insts

	-- Reverses the gated decls, but that doesn't matter
    choose2 (gis, decls) (gates, decl)
	|  null gates 	-- Happens when we have 'instance T a where ...'
        || any (`elem` tycons) gates = (gis, 	           decl:decls)
	| otherwise		     = ((gates,decl) : gis, decls)
\end{code}

%************************************************************************
%*									*
		Rules
%*									*
%************************************************************************

We move a IfaceRule from eps_rules to eps_rule_base when all its LHS free vars
are in the type environment.  However, remember that typechecking a Rule may 
(as a side effect) augment the type envt, and so we may need to iterate the process.

\begin{code}
loadImportedRules :: HscEnv -> ModGuts -> IO PackageRuleBase
loadImportedRules hsc_env guts
  = initIfaceRules hsc_env guts $ do 
	{ -- Get new rules
	  if_rules <- updateEps (\ eps ->
		let { (new_pool, if_rules) = selectRules (eps_rules eps) (eps_PTE eps) }
		in (eps { eps_rules = new_pool }, if_rules) )

	; traceIf (ptext SLIT("Importing rules:") <+> vcat (map ppr if_rules))

	; let tc_rule (mod, rule) = initIfaceLcl mod (tcIfaceRule rule)
	; core_rules <- mapM tc_rule if_rules

	-- Debug print
	; traceIf (ptext SLIT("Imported rules:") <+> pprIdRules core_rules)
	
	-- Update the rule base and return it
	; updateEps (\ eps -> 
	    let { new_rule_base = extendRuleBaseList (eps_rule_base eps) core_rules }
	    in (eps { eps_rule_base = new_rule_base }, new_rule_base)
	  ) 

	-- Strictly speaking, at this point we should go round again, since
	-- typechecking one set of rules may bring in new things which enable
	-- some more rules to come in.  But we call loadImportedRules several
	-- times anyway, so I'm going to be lazy and ignore this.
    }


selectRules :: RulePool -> TypeEnv -> (RulePool, [(ModuleName, IfaceRule)])
-- Not terribly efficient.  Look at each rule in the pool to see if
-- all its gates are in the type env.  If so, take it out of the pool.
-- If not, trim its gates for next time.
selectRules (Pool rules n_in n_out) type_env
  = (Pool rules' n_in (n_out + length if_rules), if_rules)
  where
    (rules', if_rules) = foldl do_one ([], []) rules

    do_one (pool, if_rules) (gates, rule)
	| null gates' = (pool, rule:if_rules)
	| otherwise   = ((gates',rule) : pool, if_rules)
	where
	  gates' = filter (not . (`elemNameEnv` type_env)) gates


tcIfaceRule :: IfaceRule -> IfL IdCoreRule
tcIfaceRule (IfaceRule {ifRuleName = rule_name, ifActivation = act, ifRuleBndrs = bndrs,
			ifRuleHead = fn_rdr, ifRuleArgs = args, ifRuleRhs = rhs })
  = bindIfaceBndrs bndrs 	$ \ bndrs' ->
    do	{ fn <- tcIfaceExtId fn_rdr
	; args' <- mappM tcIfaceExpr args
	; rhs'  <- tcIfaceExpr rhs
	; returnM (fn, (Rule rule_name act bndrs' args' rhs')) }

tcIfaceRule (IfaceBuiltinRule fn_rdr core_rule)
  = do	{ fn <- tcIfaceExtId fn_rdr
	; returnM (fn, core_rule) }
\end{code}


%************************************************************************
%*									*
			Types
%*									*
%************************************************************************

\begin{code}
tcIfaceType :: IfaceType -> IfL Type
tcIfaceType (IfaceTyVar n)        = do { tv <- tcIfaceTyVar n; return (TyVarTy tv) }
tcIfaceType (IfaceAppTy t1 t2)    = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (AppTy t1' t2') }
tcIfaceType (IfaceFunTy t1 t2)    = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (FunTy t1' t2') }
tcIfaceType (IfaceTyConApp tc ts) = do { tc' <- tcIfaceTyCon tc; ts' <- tcIfaceTypes ts; return (mkGenTyConApp tc' ts') }
tcIfaceType (IfaceForAllTy tv t)  = bindIfaceTyVar tv $ \ tv' -> do { t' <- tcIfaceType t; return (ForAllTy tv' t') }
tcIfaceType (IfacePredTy st)      = do { st' <- tcIfacePredType st; return (PredTy st') }

tcIfaceTypes tys = mapM tcIfaceType tys

-----------------------------------------
tcIfacePredType :: IfacePredType -> IfL PredType
tcIfacePredType (IfaceClassP cls ts) = do { cls' <- tcIfaceClass cls; ts' <- tcIfaceTypes ts; return (ClassP cls' ts') }
tcIfacePredType (IfaceIParam ip t)   = do { ip' <- newIPName ip; t' <- tcIfaceType t; return (IParam ip' t') }

-----------------------------------------
tcIfaceCtxt :: IfaceContext -> IfL ThetaType
tcIfaceCtxt sts = mappM tcIfacePredType sts
\end{code}


%************************************************************************
%*									*
			Core
%*									*
%************************************************************************

\begin{code}
tcIfaceExpr :: IfaceExpr -> IfL CoreExpr
tcIfaceExpr (IfaceType ty)
  = tcIfaceType ty		`thenM` \ ty' ->
    returnM (Type ty')

tcIfaceExpr (IfaceLcl name)
  = tcIfaceLclId name 	`thenM` \ id ->
    returnM (Var id)

tcIfaceExpr (IfaceExt gbl)
  = tcIfaceExtId gbl 	`thenM` \ id ->
    returnM (Var id)

tcIfaceExpr (IfaceLit lit)
  = returnM (Lit lit)

tcIfaceExpr (IfaceFCall cc ty)
  = tcIfaceType ty 	`thenM` \ ty' ->
    newUnique		`thenM` \ u ->
    returnM (Var (mkFCallId u cc ty'))

tcIfaceExpr (IfaceTuple boxity args) 
  = mappM tcIfaceExpr args	`thenM` \ args' ->
    let
	-- Put the missing type arguments back in
	con_args = map (Type . exprType) args' ++ args'
    in
    returnM (mkApps (Var con_id) con_args)
  where
    arity = length args
    con_id = dataConWorkId (tupleCon boxity arity)
    

tcIfaceExpr (IfaceLam bndr body)
  = bindIfaceBndr bndr 		$ \ bndr' ->
    tcIfaceExpr body		`thenM` \ body' ->
    returnM (Lam bndr' body')

tcIfaceExpr (IfaceApp fun arg)
  = tcIfaceExpr fun		`thenM` \ fun' ->
    tcIfaceExpr arg		`thenM` \ arg' ->
    returnM (App fun' arg')

tcIfaceExpr (IfaceCase scrut case_bndr alts) 
  = tcIfaceExpr scrut		`thenM` \ scrut' ->
    newIfaceName case_bndr	`thenM` \ case_bndr_name ->
    let
	scrut_ty   = exprType scrut'
	case_bndr' = mkLocalId case_bndr_name scrut_ty
	tc_app     = splitTyConApp scrut_ty
		-- NB: Won't always succeed (polymoprhic case)
		--     but won't be demanded in those cases
		-- NB: not tcSplitTyConApp; we are looking at Core here
		--     look through non-rec newtypes to find the tycon that
		--     corresponds to the datacon in this case alternative
    in
    extendIfaceIdEnv [case_bndr']	$
    mappM (tcIfaceAlt tc_app) alts	`thenM` \ alts' ->
    returnM (Case scrut' case_bndr' alts')

tcIfaceExpr (IfaceLet (IfaceNonRec bndr rhs) body)
  = tcIfaceExpr rhs		`thenM` \ rhs' ->
    bindIfaceId bndr 		$ \ bndr' ->
    tcIfaceExpr body		`thenM` \ body' ->
    returnM (Let (NonRec bndr' rhs') body')

tcIfaceExpr (IfaceLet (IfaceRec pairs) body)
  = bindIfaceIds bndrs		$ \ bndrs' ->
    mappM tcIfaceExpr rhss	`thenM` \ rhss' ->
    tcIfaceExpr body		`thenM` \ body' ->
    returnM (Let (Rec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs

tcIfaceExpr (IfaceNote note expr) 
  = tcIfaceExpr expr		`thenM` \ expr' ->
    case note of
	IfaceCoerce to_ty -> tcIfaceType to_ty	`thenM` \ to_ty' ->
			     returnM (Note (Coerce to_ty'
                                                   (exprType expr')) expr')
	IfaceInlineCall   -> returnM (Note InlineCall expr')
	IfaceInlineMe     -> returnM (Note InlineMe   expr')
	IfaceSCC cc       -> returnM (Note (SCC cc)   expr')
	IfaceCoreNote n   -> returnM (Note (CoreNote n) expr')

-------------------------
tcIfaceAlt _ (IfaceDefault, names, rhs)
  = ASSERT( null names )
    tcIfaceExpr rhs		`thenM` \ rhs' ->
    returnM (DEFAULT, [], rhs')
  
tcIfaceAlt _ (IfaceLitAlt lit, names, rhs)
  = ASSERT( null names )
    tcIfaceExpr rhs		`thenM` \ rhs' ->
    returnM (LitAlt lit, [], rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcIfaceAlt (tycon, inst_tys) (IfaceDataAlt data_occ, arg_occs, rhs)
  = let	
	tycon_mod = nameModuleName (tyConName tycon)
    in
    tcIfaceDataCon (ExtPkg tycon_mod data_occ)	`thenM` \ con ->
    newIfaceNames arg_occs			`thenM` \ arg_names ->
    let
	ex_tyvars   = dataConExistentialTyVars con
	main_tyvars = tyConTyVars tycon
	ex_tyvars'  = [mkTyVar name (tyVarKind tv) | (name,tv) <- arg_names `zip` ex_tyvars] 
	ex_tys'	    = mkTyVarTys ex_tyvars'
	arg_tys	    = dataConArgTys con (inst_tys ++ ex_tys')
	id_names    = dropList ex_tyvars arg_names
	arg_ids
#ifdef DEBUG
		| not (equalLength id_names arg_tys)
		= pprPanic "tcIfaceAlts" (ppr (con, arg_names, rhs) $$
					 (ppr main_tyvars <+> ppr ex_tyvars) $$
					 ppr arg_tys)
		| otherwise
#endif
		= zipWithEqual "tcIfaceAlts" mkLocalId id_names arg_tys
    in
    ASSERT2( con `elem` tyConDataCons tycon && equalLength inst_tys main_tyvars,
	     ppr con $$ ppr tycon $$ ppr (tyConDataCons tycon) $$ ppr arg_tys $$  ppr main_tyvars  )
    extendIfaceTyVarEnv ex_tyvars'	$
    extendIfaceIdEnv arg_ids		$
    tcIfaceExpr rhs			`thenM` \ rhs' ->
    returnM (DataAlt con, ex_tyvars' ++ arg_ids, rhs')

tcIfaceAlt (tycon, inst_tys) (IfaceTupleAlt boxity, arg_occs, rhs)
  = newIfaceNames arg_occs	`thenM` \ arg_names ->
    let
	[con]   = tyConDataCons tycon
	arg_ids = zipWithEqual "tcIfaceAlts" mkLocalId arg_names inst_tys
    in
    ASSERT( isTupleTyCon tycon )
    extendIfaceIdEnv arg_ids		$
    tcIfaceExpr rhs			`thenM` \ rhs' ->
    returnM (DataAlt con, arg_ids, rhs')
\end{code}


\begin{code}
tcExtCoreBindings :: Module -> [IfaceBinding] -> IfL [CoreBind]	-- Used for external core
tcExtCoreBindings mod []     = return []
tcExtCoreBindings mod (b:bs) = do_one mod b (tcExtCoreBindings mod bs)

do_one :: Module -> IfaceBinding -> IfL [CoreBind] -> IfL [CoreBind]
do_one mod (IfaceNonRec bndr rhs) thing_inside
  = do	{ rhs' <- tcIfaceExpr rhs
	; bndr' <- newExtCoreBndr mod bndr
	; extendIfaceIdEnv [bndr'] $ do 
	{ core_binds <- thing_inside
	; return (NonRec bndr' rhs' : core_binds) }}

do_one mod (IfaceRec pairs) thing_inside
  = do	{ bndrs' <- mappM (newExtCoreBndr mod) bndrs
	; extendIfaceIdEnv bndrs' $ do
 	{ rhss' <- mappM tcIfaceExpr rhss
	; core_binds <- thing_inside
	; return (Rec (bndrs' `zip` rhss') : core_binds) }}
  where
    (bndrs,rhss) = unzip pairs
\end{code}


%************************************************************************
%*									*
		IdInfo
%*									*
%************************************************************************

\begin{code}
tcIdInfo name ty NoInfo        = return vanillaIdInfo
tcIdInfo name ty DiscardedInfo = return vanillaIdInfo
tcIdInfo name ty (HasInfo iface_info)
  = foldlM tcPrag init_info iface_info
  where
    -- Set the CgInfo to something sensible but uninformative before
    -- we start; default assumption is that it has CAFs
    init_info = vanillaIdInfo

    tcPrag info HsNoCafRefs         = returnM (info `setCafInfo`   NoCafRefs)
    tcPrag info (HsArity arity)     = returnM (info `setArityInfo` arity)
    tcPrag info (HsStrictness str)  = returnM (info `setAllStrictnessInfo` Just str)

	-- The next two are lazy, so they don't transitively suck stuff in
    tcPrag info (HsWorker nm arity) = tcWorkerInfo ty info nm arity
    tcPrag info (HsUnfold inline_prag expr)
	= tcPragExpr name expr 	`thenM` \ maybe_expr' ->
	  let
		-- maybe_expr' doesn't get looked at if the unfolding
		-- is never inspected; so the typecheck doesn't even happen
		unfold_info = case maybe_expr' of
				Nothing    -> noUnfolding
				Just expr' -> mkTopUnfolding expr' 
	  in
 	  returnM (info `setUnfoldingInfoLazily` unfold_info
			`setInlinePragInfo`      inline_prag)
\end{code}

\begin{code}
tcWorkerInfo ty info wkr_name arity
  = do 	{ mb_wkr_id <- forkM_maybe doc (tcIfaceExtId (LocalTop wkr_name))

	-- We return without testing maybe_wkr_id, but as soon as info is
	-- looked at we will test it.  That's ok, because its outside the
	-- knot; and there seems no big reason to further defer the
	-- tcIfaceId lookup.  (Contrast with tcPragExpr, where postponing walking
	-- over the unfolding until it's actually used does seem worth while.)
	; us <- newUniqueSupply

	; returnM (case mb_wkr_id of
		     Nothing     -> info
		     Just wkr_id -> add_wkr_info us wkr_id info) }
  where
    doc = text "Worker for" <+> ppr wkr_name
    add_wkr_info us wkr_id info
	= info `setUnfoldingInfoLazily`  mk_unfolding us wkr_id
	       `setWorkerInfo`           HasWorker wkr_id arity

    mk_unfolding us wkr_id = mkTopUnfolding (initUs_ us (mkWrapper ty strict_sig) wkr_id)

    	-- We are relying here on strictness info always appearing 
	-- before worker info,  fingers crossed ....
    strict_sig = case newStrictnessInfo info of
		   Just sig -> sig
		   Nothing  -> pprPanic "Worker info but no strictness for" (ppr wkr_name)
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcPragExpr :: Name -> IfaceExpr -> IfL (Maybe CoreExpr)
tcPragExpr name expr
  = forkM_maybe doc $
    tcIfaceExpr expr		`thenM` \ core_expr' ->

		-- Check for type consistency in the unfolding
    ifOptM Opt_DoCoreLinting (
	get_in_scope_ids			`thenM` \ in_scope -> 
	case lintUnfolding noSrcLoc in_scope core_expr' of
	  Nothing       -> returnM ()
	  Just fail_msg -> pprPanic "Iface Lint failure" (doc <+> fail_msg)
    )				`thenM_`

   returnM core_expr'	
  where
    doc = text "Unfolding of" <+> ppr name
    get_in_scope_ids 	-- Urgh; but just for linting
	= setLclEnv () $ 
	  do	{ env <- getGblEnv 
		; case if_rec_types env of {
			  Nothing -> return [] ;
			  Just (_, get_env) -> do
		{ type_env <- get_env
		; return (typeEnvIds type_env) }}}
\end{code}



%************************************************************************
%*									*
		Bindings
%*									*
%************************************************************************

\begin{code}
bindIfaceBndr :: IfaceBndr -> (CoreBndr -> IfL a) -> IfL a
bindIfaceBndr (IfaceIdBndr bndr) thing_inside
  = bindIfaceId bndr thing_inside
bindIfaceBndr (IfaceTvBndr bndr) thing_inside
  = bindIfaceTyVar bndr thing_inside
    
bindIfaceBndrs :: [IfaceBndr] -> ([CoreBndr] -> IfL a) -> IfL a
bindIfaceBndrs []     thing_inside = thing_inside []
bindIfaceBndrs (b:bs) thing_inside
  = bindIfaceBndr b	$ \ b' ->
    bindIfaceBndrs bs	$ \ bs' ->
    thing_inside (b':bs')

-----------------------
bindIfaceId :: (OccName, IfaceType) -> (Id -> IfL a) -> IfL a
bindIfaceId (occ, ty) thing_inside
  = do	{ name <- newIfaceName occ
	; ty' <- tcIfaceType ty
	; let {	id = mkLocalId name ty' }
	; extendIfaceIdEnv [id] (thing_inside id) }
    
bindIfaceIds :: [(OccName, IfaceType)] -> ([Id] -> IfL a) -> IfL a
bindIfaceIds bndrs thing_inside
  = do 	{ names <- newIfaceNames occs
	; tys' <- mappM tcIfaceType tys
	; let {	ids = zipWithEqual "tcCoreValBndr" mkLocalId names tys' }
	; extendIfaceIdEnv ids (thing_inside ids) }
  where
    (occs,tys) = unzip bndrs


-----------------------
newExtCoreBndr :: Module -> (OccName, IfaceType) -> IfL Id
newExtCoreBndr mod (occ, ty)
  = do	{ name <- newGlobalBinder mod occ Nothing noSrcLoc
	; ty' <- tcIfaceType ty
	; return (mkLocalId name ty') }

-----------------------
bindIfaceTyVar :: IfaceTvBndr -> (TyVar -> IfL a) -> IfL a
bindIfaceTyVar (occ,kind) thing_inside
  = do	{ name <- newIfaceName occ
   	; let tyvar = mk_iface_tyvar name kind
	; extendIfaceTyVarEnv [tyvar] (thing_inside tyvar) }

bindIfaceTyVars :: [IfaceTvBndr] -> ([TyVar] -> IfL a) -> IfL a
bindIfaceTyVars bndrs thing_inside
  = do	{ names <- newIfaceNames occs
  	; let tyvars = zipWith mk_iface_tyvar names kinds
	; extendIfaceTyVarEnv tyvars (thing_inside tyvars) }
  where
    (occs,kinds) = unzip bndrs

mk_iface_tyvar name kind = mkTyVar name kind
\end{code}
