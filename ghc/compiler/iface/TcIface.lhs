%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
module TcIface ( 
	tcImportDecl, typecheckIface, tcIfaceDecl, tcIfaceGlobal,
	loadImportedInsts, loadImportedRules,
	tcExtCoreBindings
 ) where

#include "HsVersions.h"

import IfaceSyn
import LoadIface	( loadHomeInterface, loadInterface, predInstGates,
			  loadDecls )
import IfaceEnv		( lookupIfaceTop, lookupIfaceExt, newGlobalBinder, 
			  extendIfaceIdEnv, extendIfaceTyVarEnv, newIPName,
			  tcIfaceTyVar, tcIfaceLclId,
			  newIfaceName, newIfaceNames )
import BuildTyCl	( buildSynTyCon, buildAlgTyCon, buildDataCon, buildClass,
			  mkAbstractTyConRhs, mkDataTyConRhs, mkNewTyConRhs )
import TcRnMonad
import TcType		( hoistForAllTys )	-- TEMPORARY HACK
import Type		( liftedTypeKind, splitTyConApp, mkSynTy, mkTyConApp,
			  mkTyVarTys, mkGenTyConApp, ThetaType, pprClassPred )
import TypeRep		( Type(..), PredType(..) )
import TyCon		( TyCon, tyConName, isSynTyCon )
import HscTypes		( ExternalPackageState(..), EpsStats(..), PackageInstEnv, 
			  HscEnv, TyThing(..), tyThingClass, tyThingTyCon, 
			  ModIface(..), ModDetails(..), ModGuts,
			  extendTypeEnv, lookupTypeEnv, lookupType, typeEnvIds )
import InstEnv		( extendInstEnvList )
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
import TyCon		( tyConDataCons, isTupleTyCon, mkForeignTyCon )
import DataCon		( DataCon, dataConWorkId, dataConTyVars, dataConArgTys, isVanillaDataCon )
import TysWiredIn	( tupleCon, tupleTyCon, listTyCon, intTyCon, boolTyCon, charTyCon, parrTyCon )
import Var		( TyVar, mkTyVar, tyVarKind )
import Name		( Name, nameModule, nameIsLocalOrFrom, 
			  isWiredInName, wiredInNameTyThing_maybe, nameParent )
import NameEnv
import OccName		( OccName )
import Module		( Module )
import UniqSupply	( initUs_ )
import Outputable	
import ErrUtils		( Message )
import Maybes		( MaybeErr(..) )
import SrcLoc		( noSrcLoc )
import Util		( zipWithEqual, dropList, equalLength )
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
tcImportDecl :: Name -> TcM TyThing
-- Entry point for source-code uses of importDecl
tcImportDecl name 
  = do 	{ traceIf (text "tcLookupGlobal" <+> ppr name)
	; mb_thing <- initIfaceTcRn (importDecl name)
	; case mb_thing of
	    Succeeded thing -> return thing
	    Failed err      -> failWithTc err }

importDecl :: Name -> IfM lcl (MaybeErr Message TyThing)
-- Get the TyThing for this Name from an interface file
importDecl name 
  | Just thing <- wiredInNameTyThing_maybe name
	-- This case definitely happens for tuples, because we
	-- don't know how many of them we'll find
	-- It also now happens for all other wired in things.  We used
	-- to pre-populate the eps_PTE with other wired-in things, but
	-- we don't seem to do that any more.  I guess it keeps the PTE smaller?
  = do 	{ updateEps_ (\ eps -> eps { eps_PTE = extendTypeEnv (eps_PTE eps) thing })
	; return (Succeeded thing) }

  | otherwise
  = do	{ traceIf nd_doc

	-- Load the interface, which should populate the PTE
	; mb_iface <- loadInterface nd_doc (nameModule name) ImportBySystem
	; case mb_iface of {
		Failed err_msg  -> return (Failed err_msg) ;
		Succeeded iface -> do

	-- Now look it up again; this time we should find it
	{ eps <- getEps	
	; case lookupTypeEnv (eps_PTE eps) name of
	    Just thing -> return (Succeeded thing)
	    Nothing    -> return (Failed not_found_msg)
    }}}
  where
    nd_doc = ptext SLIT("Need decl for") <+> ppr name
    not_found_msg = hang (ptext SLIT("Can't find interface-file declaration for") <+> ppr (nameParent name))
	  	       2 (vcat [ptext SLIT("Probable cause: bug in .hi-boot file, or inconsistent .hi file"),
		                ptext SLIT("Use -ddump-if-trace to get an idea of which file caused the error")])
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
typecheckIface hsc_env iface
  = initIfaceTc hsc_env iface $ \ tc_env_var -> do
	{ 	-- Get the right set of decls and rules.  If we are compiling without -O
		-- we discard pragmas before typechecking, so that we don't "see"
		-- information that we shouldn't.  From a versioning point of view
		-- It's not actually *wrong* to do so, but in fact GHCi is unable 
		-- to handle unboxed tuples, so it must not see unfoldings.
	  ignore_prags <- doptM Opt_IgnoreInterfacePragmas

		-- Load & typecheck the decls
	; decl_things <- loadDecls ignore_prags (mi_decls iface)

	; let type_env = mkNameEnv decl_things
	; writeMutVar tc_env_var type_env

		-- Now do those rules and instances
	; let { rules | ignore_prags = []
		      | otherwise    = mi_rules iface
	      ; dfuns = mi_insts iface
	      } 
	; dfuns <- mapM tcIfaceInst dfuns
	; rules <- mapM tcIfaceRule rules

		-- Finished
	; return (ModDetails { md_types = type_env, md_insts = dfuns, md_rules = rules }) 
    }
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

tcIfaceDecl (IfaceData {ifName = occ_name, 
			ifTyVars = tv_bndrs, 
			ifCons = rdr_cons, 
			ifVrcs = arg_vrcs, ifRec = is_rec, 
			ifGeneric = want_generic })
  = do	{ tc_name <- lookupIfaceTop occ_name
	; bindIfaceTyVars tv_bndrs $ \ tyvars -> do

	{ tycon <- fixM ( \ tycon -> do
	    { cons  <- tcIfaceDataCons tycon tyvars rdr_cons
	    ; tycon <- buildAlgTyCon tc_name tyvars cons 
			    arg_vrcs is_rec want_generic
	    ; return tycon
	    })
        ; traceIf (text "tcIfaceDecl4" <+> ppr tycon)
	; return (ATyCon tycon)
    }}

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

tcIfaceDataCons tycon tc_tyvars if_cons
  = case if_cons of
	IfAbstractTyCon		 -> return mkAbstractTyConRhs
	IfDataTyCon mb_ctxt cons -> do 	{ mb_theta <- tc_ctxt mb_ctxt
					; data_cons <- mappM tc_con_decl cons
					; return (mkDataTyConRhs mb_theta data_cons) }
	IfNewTyCon con		 -> do 	{ data_con <- tc_con_decl con
					; return (mkNewTyConRhs tycon data_con) }
  where
    tc_ctxt Nothing     = return Nothing
    tc_ctxt (Just ctxt) = do { theta <- tcIfaceCtxt ctxt; return (Just theta) }

    tc_con_decl (IfVanillaCon {	ifConOcc = occ, ifConInfix = is_infix, ifConArgTys = args, 
				ifConStricts = stricts, ifConFields = field_lbls})
      = do { name  <- lookupIfaceTop occ
		-- Read the argument types, but lazily to avoid faulting in
		-- the component types unless they are really needed
 	   ; arg_tys <- forkM (mk_doc name) (mappM tcIfaceType args)
	   ; lbl_names <- mappM lookupIfaceTop field_lbls
	   ; buildDataCon name is_infix True {- Vanilla -} 
			  stricts lbl_names
			  tc_tyvars [] arg_tys tycon
			  (mkTyVarTys tc_tyvars)	-- Vanilla => we know result tys
	   }  

    tc_con_decl (IfGadtCon {	ifConTyVars = con_tvs,
				ifConOcc = occ, ifConCtxt = ctxt, 
				ifConArgTys = args, ifConResTys = ress, 
				ifConStricts = stricts})
      = bindIfaceTyVars con_tvs	$ \ con_tyvars -> do
	{ name  <- lookupIfaceTop occ
	; theta <- tcIfaceCtxt ctxt	-- Laziness seems not worth the bother here
	 	-- At one stage I thought that this context checking *had*
		-- to be lazy, because of possible mutual recursion between the
		-- type and the classe: 
		-- E.g. 
		--	class Real a where { toRat :: a -> Ratio Integer }
		--	data (Real a) => Ratio a = ...
		-- But now I think that the laziness in checking class ops breaks 
		-- the loop, so no laziness needed

	-- Read the argument types, but lazily to avoid faulting in
	-- the component types unless they are really needed
 	; arg_tys <- forkM (mk_doc name) (mappM tcIfaceType args)
 	; res_tys <- forkM (mk_doc name) (mappM tcIfaceType ress)

	; buildDataCon name False {- Not infix -} False {- Not vanilla -}
		       stricts [{- No fields -}]
		       con_tyvars theta 
		       arg_tys tycon res_tys
	}
    mk_doc con_name = ptext SLIT("Constructor") <+> ppr con_name
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

		-- Now suck in the relevant instances
	; iface_insts <- updateEps (selectInsts cls_gate tc_gates)

	-- Empty => finish up rapidly, without writing to eps
	; if null iface_insts then
		do { eps <- getEps; return (eps_inst_env eps) }
	  else do
	{ traceIf (sep [ptext SLIT("Importing instances for") <+> pprClassPred cls tys, 
			nest 2 (vcat [ppr i | (_,_,i) <- iface_insts])])

	-- Typecheck the new instances
	; dfuns <- initIfaceTcRn (mappM tc_inst iface_insts)

	-- And put them in the package instance environment
	; updateEps ( \ eps ->
	    let 
		inst_env' = extendInstEnvList (eps_inst_env eps) dfuns
	    in
	    (eps { eps_inst_env = inst_env' }, inst_env')
	)}}
  where
    wired_doc = ptext SLIT("Need home inteface for wired-in thing")

tc_inst (mod, loc, inst) = initIfaceLcl mod full_loc (tcIfaceInst inst)
  where
    full_loc = loc $$ (nest 2 (ptext SLIT("instance decl") <+> ppr inst))

tcIfaceInst :: IfaceInst -> IfL DFunId
tcIfaceInst (IfaceInst { ifDFun = dfun_occ })
  = tcIfaceExtId (LocalTop dfun_occ)

selectInsts :: Name -> [Name] -> ExternalPackageState 
	    -> (ExternalPackageState, [(Module, SDoc, IfaceInst)])
selectInsts cls tycons eps
  = (eps { eps_insts = insts', eps_stats = stats' }, iface_insts)
  where
    insts  = eps_insts eps
    stats  = eps_stats eps
    stats' = stats { n_insts_out = n_insts_out stats + length iface_insts } 

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
loadImportedRules :: HscEnv -> ModGuts -> IO [IdCoreRule]
-- Returns just the new rules added
loadImportedRules hsc_env guts
  = initIfaceRules hsc_env guts $ do 
	{ -- Get new rules
	  if_rules <- updateEps selectRules

	; traceIf (ptext SLIT("Importing rules:") <+> vcat [ppr r | (_,_,r) <- if_rules])

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
	; return core_rules
    }

tc_rule (mod, loc, rule) = initIfaceLcl mod full_loc (tcIfaceRule rule)
  where
    full_loc = loc $$ (nest 2 (ptext SLIT("rule") <+> ppr rule))
   
selectRules :: ExternalPackageState -> (ExternalPackageState, [(Module, SDoc, IfaceRule)])
-- Not terribly efficient.  Look at each rule in the pool to see if
-- all its gates are in the type env.  If so, take it out of the pool.
-- If not, trim its gates for next time.
selectRules eps
  = (eps { eps_rules = rules', eps_stats = stats' }, if_rules)
  where
    stats    = eps_stats eps
    rules    = eps_rules eps
    type_env = eps_PTE eps
    stats'   = stats { n_rules_out = n_rules_out stats + length if_rules }

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
	; let rule = Rule rule_name act bndrs' args' rhs'
	; returnM (IdCoreRule fn (isOrphNm fn_rdr) rule) }
  where

tcIfaceRule (IfaceBuiltinRule fn_rdr core_rule)
  = do	{ fn <- tcIfaceExtId fn_rdr
	; returnM (IdCoreRule fn (isOrphNm fn_rdr) core_rule) }

isOrphNm :: IfaceExtName -> Bool
-- An orphan name comes from somewhere other than this module,
-- so it has a non-local name
isOrphNm name = not (isLocalIfaceExtName name)
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
tcIfaceType (IfaceTyConApp tc ts) = do { tc' <- tcIfaceTyCon tc; ts' <- tcIfaceTypes ts; return (mkIfTcApp tc' ts') }
tcIfaceType (IfaceForAllTy tv t)  = bindIfaceTyVar tv $ \ tv' -> do { t' <- tcIfaceType t; return (ForAllTy tv' t') }
tcIfaceType (IfacePredTy st)      = do { st' <- tcIfacePredType st; return (PredTy st') }

tcIfaceTypes tys = mapM tcIfaceType tys

mkIfTcApp :: TyCon -> [Type] -> Type
-- In interface files we retain type synonyms (for brevity and better error
-- messages), but type synonyms can expand into non-hoisted types (ones with
-- foralls to the right of an arrow), so we must be careful to hoist them here.
-- This hack should go away when we get rid of hoisting.
mkIfTcApp tc tys
  | isSynTyCon tc = hoistForAllTys (mkSynTy tc tys)
  | otherwise	  = mkTyConApp tc tys

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

tcIfaceExpr (IfaceCase scrut case_bndr ty alts) 
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
    tcIfaceType ty		`thenM` \ ty' ->
    returnM (Case scrut' case_bndr' ty' alts')

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
  = do	{ let tycon_mod = nameModule (tyConName tycon)
	; con <- tcIfaceDataCon (ExtPkg tycon_mod data_occ)
	; ASSERT2( con `elem` tyConDataCons tycon,
		   ppr con $$ ppr tycon $$ ppr (tyConDataCons tycon) )
		  
	  if isVanillaDataCon con then
		tcVanillaAlt con inst_tys arg_occs rhs
	  else
    do 	{ 	-- General case
	  arg_names <- newIfaceNames arg_occs
	; let	tyvars   = [ mkTyVar name (tyVarKind tv) 
			   | (name,tv) <- arg_names `zip` dataConTyVars con] 
		arg_tys	 = dataConArgTys con (mkTyVarTys tyvars)
		id_names = dropList tyvars arg_names
		arg_ids  = ASSERT2( equalLength id_names arg_tys,
				    ppr (con, arg_names, rhs) $$ ppr tyvars $$ ppr arg_tys )
			   zipWith mkLocalId id_names arg_tys

	; rhs' <- extendIfaceTyVarEnv tyvars	$
		  extendIfaceIdEnv arg_ids	$
		  tcIfaceExpr rhs
	; return (DataAlt con, tyvars ++ arg_ids, rhs') }}

tcIfaceAlt (tycon, inst_tys) (IfaceTupleAlt boxity, arg_occs, rhs)
  = ASSERT( isTupleTyCon tycon )
    do	{ let [data_con] = tyConDataCons tycon
	; tcVanillaAlt data_con inst_tys arg_occs rhs }

tcVanillaAlt data_con inst_tys arg_occs rhs
  = do	{ arg_names <- newIfaceNames arg_occs
	; let arg_tys = dataConArgTys data_con inst_tys
	; let arg_ids = ASSERT2( equalLength arg_names arg_tys,
				 ppr data_con <+> ppr inst_tys <+> ppr arg_occs $$ ppr rhs )
			zipWith mkLocalId arg_names arg_tys
	; rhs' <- extendIfaceIdEnv arg_ids (tcIfaceExpr rhs)
	; returnM (DataAlt data_con, arg_ids, rhs') }
\end{code}


\begin{code}
tcExtCoreBindings :: [IfaceBinding] -> IfL [CoreBind]	-- Used for external core
tcExtCoreBindings []     = return []
tcExtCoreBindings (b:bs) = do_one b (tcExtCoreBindings bs)

do_one :: IfaceBinding -> IfL [CoreBind] -> IfL [CoreBind]
do_one (IfaceNonRec bndr rhs) thing_inside
  = do	{ rhs' <- tcIfaceExpr rhs
	; bndr' <- newExtCoreBndr bndr
	; extendIfaceIdEnv [bndr'] $ do 
	{ core_binds <- thing_inside
	; return (NonRec bndr' rhs' : core_binds) }}

do_one (IfaceRec pairs) thing_inside
  = do	{ bndrs' <- mappM newExtCoreBndr bndrs
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
tcIdInfo :: Name -> Type -> IfaceIdInfo -> IfL IdInfo
tcIdInfo name ty NoInfo		= return vanillaIdInfo
tcIdInfo name ty (HasInfo info) = foldlM tcPrag init_info info
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
tcWorkerInfo ty info wkr arity
  = do 	{ mb_wkr_id <- forkM_maybe doc (tcIfaceExtId wkr)

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
    doc = text "Worker for" <+> ppr wkr
    add_wkr_info us wkr_id info
	= info `setUnfoldingInfoLazily`  mk_unfolding us wkr_id
	       `setWorkerInfo`           HasWorker wkr_id arity

    mk_unfolding us wkr_id = mkTopUnfolding (initUs_ us (mkWrapper ty strict_sig) wkr_id)

    	-- We are relying here on strictness info always appearing 
	-- before worker info,  fingers crossed ....
    strict_sig = case newStrictnessInfo info of
		   Just sig -> sig
		   Nothing  -> pprPanic "Worker info but no strictness for" (ppr wkr)
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
		Getting from Names to TyThings
%*									*
%************************************************************************

\begin{code}
tcIfaceGlobal :: Name -> IfL TyThing
tcIfaceGlobal name
  = do	{ (eps,hpt) <- getEpsAndHpt
	; case lookupType hpt (eps_PTE eps) name of {
	    Just thing -> return thing ;
	    Nothing    -> do

	{ env <- getGblEnv
	; case if_rec_types env of {
	    Just (mod, get_type_env) 
		| nameIsLocalOrFrom mod name
		-> do 		-- It's defined in the module being compiled
	  	{ type_env <- setLclEnv () get_type_env		-- yuk
		; case lookupNameEnv type_env name of
			Just thing -> return thing
			Nothing	   -> pprPanic "tcIfaceGlobal (local): not found:"  
						(ppr name $$ ppr type_env) }

	  ; other -> do

	{ mb_thing <- importDecl name 	-- It's imported; go get it
	; case mb_thing of
	    Failed err      -> failIfM err
	    Succeeded thing -> return thing
    }}}}}

tcIfaceTyCon :: IfaceTyCon -> IfL TyCon
tcIfaceTyCon IfaceIntTc  = return intTyCon
tcIfaceTyCon IfaceBoolTc = return boolTyCon
tcIfaceTyCon IfaceCharTc = return charTyCon
tcIfaceTyCon IfaceListTc = return listTyCon
tcIfaceTyCon IfacePArrTc = return parrTyCon
tcIfaceTyCon (IfaceTupTc bx ar) = return (tupleTyCon bx ar)
tcIfaceTyCon (IfaceTc ext_nm) = do { name <- lookupIfaceExt ext_nm
				   ; thing <- tcIfaceGlobal name
				   ; return (tyThingTyCon thing) }

tcIfaceClass :: IfaceExtName -> IfL Class
tcIfaceClass rdr_name = do { name <- lookupIfaceExt rdr_name
			   ; thing <- tcIfaceGlobal name
			   ; return (tyThingClass thing) }

tcIfaceDataCon :: IfaceExtName -> IfL DataCon
tcIfaceDataCon gbl = do { name <- lookupIfaceExt gbl
			; thing <- tcIfaceGlobal name
		 	; case thing of
				ADataCon dc -> return dc
				other   -> pprPanic "tcIfaceExtDC" (ppr gbl $$ ppr name$$ ppr thing) }

tcIfaceExtId :: IfaceExtName -> IfL Id
tcIfaceExtId gbl = do { name <- lookupIfaceExt gbl
		      ; thing <- tcIfaceGlobal name
		      ; case thing of
			  AnId id -> return id
			  other   -> pprPanic "tcIfaceExtId" (ppr gbl $$ ppr name$$ ppr thing) }
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
newExtCoreBndr :: (OccName, IfaceType) -> IfL Id
newExtCoreBndr (occ, ty)
  = do	{ mod <- getIfModule
	; name <- newGlobalBinder mod occ Nothing noSrcLoc
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

