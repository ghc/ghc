%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
#include "HsVersions.h"

module TcClassDcl (
	tcClassDecls1, tcClassDecls2,
	ClassInfo   -- abstract
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty	-- add proper one below

import TcMonad		-- typechecking monad machinery
import TcMonadFns	( newDicts, newClassOpLocals, copyTyVars )
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( pAT_ERROR_ID )
import AbsUniType	( mkClass, getClassKey, getClassBigSig,
			  getClassOpString, getClassOps, splitType,
			  mkSuperDictSelType, InstTyEnv(..),
			  instantiateTy, instantiateThetaTy, UniType
			)
import BackSubst	( applyTcSubstToBinds )
import CE		-- ( nullCE, unitCE, plusCE, CE(..), UniqFM )
import E		( mkE, getE_TCE, getE_CE, tvOfE, nullGVE, plusGVE, E, TCE(..), UniqFM, GVE(..) )
import Errors		( confusedNameErr, Error(..) )
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import Id		( mkSuperDictSelId, mkInstId, getIdUniType,
			  Id, DictFun(..)
			)
import IdInfo
import Inst		( InstOrigin(..), Inst )
import InstEnv
import LIE		( nullLIE, mkLIE, plusLIE, LIE )
import Maybes		( Maybe(..) )
import Name		( Name(..) )
import PlainCore	( escErrorMsg )
import Spec		( specTy )
import TVE		( mkTVE, TVE(..)
			  IF_ATTACK_PRAGMAS(COMMA u2i)
			)
import TcClassSig	( tcClassSigs )
import TcContext	( tcContext )
import TcInstDcls	( processInstBinds )
import TcPragmas	( tcGenPragmas )
import Util
\end{code}

@ClassInfo@ communicates the essential information about
locally-defined classes between passes 1 and 2.

\begin{code}
data ClassInfo
  = ClassInfo	Class
		RenamedMonoBinds
\end{code}


%************************************************************************
%*									*
\subsection[TcClassDcl]{Does the real work (apart from default methods)}
%*									*
%************************************************************************

\begin{code}
tcClassDecls1
    :: E			-- Consult the CE/TCE args only to build knots
    -> InstanceMapper		-- Maps class name to its instances,
				-- ...and its ops to their instances,
    -> [RenamedClassDecl]
    -> TcM ([ClassInfo],	-- boiled-down info related to classes
	    CE,			-- env so we can look up classes elsewhere
	    GVE)		-- env so we can look up class ops elsewhere

tcClassDecls1 e rec_inst_mapper []
  = returnTc ([], nullCE, nullGVE)

tcClassDecls1 e rec_inst_mapper (cd:cds)
  = tc_clas1			    cd  `thenTc` \ (cinfo1_maybe, ce1, gve1) ->
    tcClassDecls1 e rec_inst_mapper cds `thenTc` \ (cinfo2, ce2, gve2) ->
    let
	glued_cinfos
	  = case cinfo1_maybe of
	      Nothing -> cinfo2
	      Just xx -> xx : cinfo2
    in
    returnTc (glued_cinfos, ce1 `plusCE` ce2, gve1 `plusGVE` gve2)
  where
    rec_ce  = getE_CE  e
    rec_tce = getE_TCE e

    tc_clas1 (ClassDecl context class_name
			tyvar_name class_sigs def_methods pragmas src_loc)

      = addSrcLocTc src_loc	(

	    -- The knot is needed so that the signatures etc can point
	    -- back to the class itself
	fixTc (\ ~(rec_clas, _) ->
	  let
	     (rec_clas_inst_env, rec_class_op_inst_fn) = rec_inst_mapper rec_clas
	  in
	    -- Get new (template) type variables for the class
	  let  (tve, [clas_tyvar], [alpha]) = mkTVE [tyvar_name]  in

	    -- Typecheck the class context; since there is only one type
	    -- variable in scope, we are assured that the it will be of
	    -- the form (C1 a, C2 a...)
	  babyTcMtoTcM (tcContext rec_ce rec_tce tve context) `thenTc` \ theta ->

	    -- Make the superclass selector ids; the "class" pragmas
	    -- may have info about the superclass dict selectors;
	    -- so it is only tcClassPragmas that gives back the
	    -- final Ids.
	  getUniquesTc (length theta)		`thenNF_Tc` \ uniqs ->
	  let
	      super_classes = [ supers | (supers, _) <- theta ]
	      super_tys
	        = [ mkSuperDictSelType rec_clas super | super <- super_classes ]
	      super_info = zip3 super_classes uniqs super_tys
	  in
	  (case pragmas of
	    NoClassPragmas ->
	      returnNF_Tc [ mk_super_id rec_clas info noIdInfo | info <- super_info ]

	    SuperDictPragmas prags ->
--	      pprTrace "SuperDictPragmas:" (ppAboves (ppr PprDebug prags : map pp super_info)) (
	      mapNF_Tc (mk_super_id_w_info rec_clas) (super_info `zipEqual` prags)
--	      )
--	      where
--		pp (sc, u, ty) = ppCat [ppr PprDebug sc, ppr PprDebug ty]

	  ) `thenNF_Tc` \ super_class_sel_ids ->

	    -- Typecheck the class signatures, checking that each mentions
	    -- the class type variable somewhere, and manufacturing
	    -- suitable Ids for selectors and default methods.
	  babyTcMtoTcM
	    (tcClassSigs e tve rec_clas rec_class_op_inst_fn
			       clas_tyvar defm_names class_sigs)
		   `thenTc` \ (ops, ops_gve, op_sel_ids, defm_ids) ->

	     -- Make the class object itself, producing clas::Class
	  let
	     clas
		= mkClass class_name clas_tyvar
			  super_classes super_class_sel_ids
			  ops op_sel_ids defm_ids
			  rec_clas_inst_env
	  in
	  returnTc (clas, ops_gve)
	)				`thenTc` \ (clas, ops_gve) ->

	     -- Return the class decl for further work if it is
	     -- local, otherwise just return the CE
	returnTc (if (isLocallyDefined class_name) then
		     Just (ClassInfo clas def_methods)
		  else
		     Nothing,
		  unitCE (getClassKey clas) clas,
		  ops_gve
    	))
      where
	defm_names = collectMonoBinders def_methods

	-----------
	mk_super_id clas (super_clas, uniq, ty) id_info
	  = mkSuperDictSelId uniq clas super_clas ty id_info

	-----------
	mk_super_id_w_info clas ((super_clas, uniq, ty), gen_prags)
	  = fixNF_Tc ( \ rec_super_id ->
		babyTcMtoNF_TcM
		    (tcGenPragmas e{-fake_E-} Nothing{-ty unknown-} rec_super_id gen_prags)
			`thenNF_Tc` \ id_info ->

		returnNF_Tc(mkSuperDictSelId uniq clas super_clas ty id_info)
	    )

{- SOMETHING LIKE THIS NEEDED? ToDo [WDP]
    tc_clas1 (ClassDecl _ bad_name _ _ _ _ src_loc)
      = failTc (confusedNameErr
		    "Bad name for a class (a type constructor, or Prelude name?)"
		    bad_name src_loc)
-}
\end{code}


%************************************************************************
%*									*
\subsection[ClassDcl-pass2]{Class decls pass 2: default methods}
%*									*
%************************************************************************

The purpose of pass 2 is
\begin{enumerate}
\item
to beat on the explicitly-provided default-method decls (if any),
using them to produce a complete set of default-method decls.
(Omitted ones elicit an error message.)
\item
to produce a definition for the selector function for each method
\end{enumerate}

Pass~2 only applies to locally-defined class declarations.

The function @tcClassDecls2@ just arranges to apply
@tcClassDecls2_help@ to each local class decl.

\begin{code}
tcClassDecls2 e class_info
  = let
	-- Get type variables free in environment. Sadly, there may be
	-- some, because of the dreaded monomorphism restriction
	free_tyvars = tvOfE e
    in
    tcClassDecls2_help e free_tyvars class_info

tcClassDecls2_help
	:: E
	-> [TyVar]
	-> [ClassInfo]
	-> NF_TcM (LIE, TypecheckedBinds)

tcClassDecls2_help e free_tyvars [] = returnNF_Tc (nullLIE, EmptyBinds)

tcClassDecls2_help e free_tyvars ((ClassInfo clas default_binds) : rest)
  = tcClassDecl2 e free_tyvars clas default_binds `thenNF_Tc` \ (lie1, binds1) ->
    tcClassDecls2_help e free_tyvars rest	  `thenNF_Tc` \ (lie2, binds2) ->
    returnNF_Tc (lie1 `plusLIE` lie2, binds1 `ThenBinds` binds2)
\end{code}

@tcClassDecl2@ is the business end of things.

\begin{code}
tcClassDecl2 :: E
	     -> [TyVar]			-- Free in the envt
	     -> Class
	     -> RenamedMonoBinds	-- The default decls
	     -> NF_TcM (LIE, TypecheckedBinds)

tcClassDecl2 e free_tyvars clas default_binds
  = let 
	src_loc = getSrcLoc clas
	origin  = ClassDeclOrigin src_loc
	(clas_tyvar_tmpl, scs, sc_sel_ids, ops, op_sel_ids, defm_ids)
	  = getClassBigSig clas
    in
	 -- Prune the substitution when we are finished, and arrange error recovery
    recoverTc (nullLIE, EmptyBinds) (
    addSrcLocTc src_loc		    (
    pruneSubstTc free_tyvars	    (

	 -- Generate bindings for the selector functions
    buildSelectors origin clas clas_tyvar_tmpl scs sc_sel_ids ops op_sel_ids
						`thenNF_Tc` \ sel_binds ->
	 -- Ditto for the methods
    buildDefaultMethodBinds e free_tyvars origin clas clas_tyvar_tmpl
		defm_ids default_binds		`thenTc` \ (const_insts, meth_binds) ->

	 -- Back-substitute through the definitions
    applyTcSubstToInsts const_insts			   `thenNF_Tc` \ final_const_insts ->
    applyTcSubstToBinds (sel_binds `ThenBinds` meth_binds) `thenNF_Tc` \ final_binds ->
    returnTc (mkLIE final_const_insts, final_binds)
    )))
\end{code}

%************************************************************************
%*									*
\subsection[ClassDcl-bld-sels]{Building the selector functions for methods and superclasses}
%*									*
%************************************************************************

\begin{code}
buildSelectors :: InstOrigin
	       -> Class			-- The class object
	       -> TyVarTemplate		-- Class type variable
	       -> [Class] -> [Id]	-- Superclasses and selectors
	       -> [ClassOp] -> [Id]	-- Class ops and selectors
	       -> NF_TcM TypecheckedBinds

buildSelectors origin clas clas_tyvar_tmpl
	scs sc_sel_ids
	ops op_sel_ids
  =
	 -- Instantiate the class variable
    copyTyVars [clas_tyvar_tmpl] `thenNF_Tc` \ (inst_env, [clas_tyvar], [clas_tyvar_ty]) ->
	 -- Make an Inst for each class op, and
	 -- dicts for the superclasses.	 These are used to
	 -- construct the selector functions
    newClassOpLocals inst_env ops			`thenNF_Tc` \ method_ids ->
    newDicts origin [ (super_clas, clas_tyvar_ty)
		    | super_clas <- scs
		    ]					`thenNF_Tc` \ dicts ->
    let dict_ids = map mkInstId dicts  in

	 -- Make suitable bindings for the selectors
    let mk_op_sel op sel_id method_id
	  = mkSelExpr origin clas_tyvar dict_ids method_ids method_id	`thenNF_Tc` \ rhs ->
	    returnNF_Tc (VarMonoBind sel_id rhs)
	mk_sc_sel sc sel_id dict_id
	 = mkSelExpr origin clas_tyvar dict_ids method_ids dict_id	`thenNF_Tc` \ rhs ->
	   returnNF_Tc (VarMonoBind sel_id rhs)
    in
    listNF_Tc (zipWith3 mk_op_sel ops op_sel_ids method_ids)	`thenNF_Tc` \ op_sel_binds ->
    listNF_Tc (zipWith3 mk_sc_sel scs sc_sel_ids dict_ids)	`thenNF_Tc` \ sc_sel_binds ->

    returnNF_Tc (SingleBind (
		 NonRecBind (
		 foldr AndMonoBinds EmptyMonoBinds (
		 op_sel_binds ++ sc_sel_binds))))
\end{code}

%************************************************************************
%*									*
\subsection[ClassDcl-misc]{Miscellaneous}
%*									*
%************************************************************************

Make a selector expression for @local@ from a dictionary consisting of
@dicts@ and @op_locals@.

We have to do a bit of jiggery pokery to get the type variables right.
Suppose we have the class decl:
\begin{verbatim}
	class Foo a where
		op1 :: Ord b => a -> b -> a
		op2 :: ...
\end{verbatim}
Then the method selector for \tr{op1} is like this:
\begin{verbatim}
	op1_sel = /\ab -> \dFoo -> case dFoo of
					(op1_method,op2_method) -> op1_method b
\end{verbatim}
Note that the type variable for \tr{b} is lifted to the top big lambda, and
\tr{op1_method} is applied to it.  This is preferable to the alternative:
\begin{verbatim}
	op1_sel' = /\a -> \dFoo -> case dFoo of
					(op1_method,op2_method) -> op1_method
\end{verbatim}
because \tr{op1_sel'} then has the rather strange type
\begin{verbatim}
	op1_sel' :: forall a. Foo a -> forall b. Ord b -> a -> b -> a
\end{verbatim}
whereas \tr{op1_sel} (the one we use) has the decent type
\begin{verbatim}
	op1_sel :: forall a b. Foo a -> Ord b -> a -> b -> a
\end{verbatim}

{\em NOTE:}
We could do the same thing for the dictionaries, giving
\begin{verbatim}
	op1_sel = /\ab -> \dFoo -> \dOrd -> case dFoo of
						(m1,m2) -> m1 b dOrd
\end{verbatim}
but WE ASSUME THAT DICTIONARY APPLICATION IS CURRIED, so the two are
precisely equivalent, and have the same type, namely
\begin{verbatim}
	op1_sel :: forall a b. Foo a -> Ord b -> a -> b -> a
\end{verbatim}

WDP 95/03: Quite false (``DICTIONARY APPLICATION IS CURRIED'').
Specialisation now wants to see all type- and dictionary-applications
absolutely explicitly.

\begin{code}
mkSelExpr :: InstOrigin -> TyVar -> [Id] -> [Id] -> Id -> NF_TcM TypecheckedExpr

mkSelExpr origin clas_tyvar dicts op_locals local
  = let
	(op_tyvar_tmpls,local_theta,_) = splitType (getIdUniType local)
    in
    copyTyVars op_tyvar_tmpls	`thenNF_Tc` \ (inst_env, op_tyvars, tys) ->
    let
	inst_theta = instantiateThetaTy inst_env local_theta
    in
    newDicts origin inst_theta	`thenNF_Tc` \ local_dict_insts ->
    let
	local_dicts = map mkInstId local_dict_insts
    in
    returnNF_Tc (TyLam (clas_tyvar:op_tyvars)
		   (ClassDictLam
		      dicts
		      op_locals
		      (mkDictLam local_dicts
			(mkDictApp (mkTyApp (Var local) tys) local_dicts))))
\end{code}


%************************************************************************
%*									*
\subsection[Default methods]{Default methods}
%*									*
%************************************************************************

The default methods for a class are each passed a dictionary for the
class, so that they get access to the other methods at the same type.
So, given the class decl
\begin{verbatim}
class Foo a where
	op1 :: a -> Bool
	op2 :: Ord b => a -> b -> b -> b

	op1 x = True
	op2 x y z = if (op1 x) && (y < z) then y else z
\end{verbatim}
we get the default methods:
\begin{verbatim}
defm.Foo.op1 :: forall a. Foo a => a -> Bool
defm.Foo.op1 = /\a -> \dfoo -> \x -> True

defm.Foo.op2 :: forall a, b. (Foo a, Ord b) => a -> b -> b -> b
defm.Foo.op2 = /\ a b -> \ dfoo dord -> \x y z ->
		  if (op1 a dfoo x) && (< b dord y z) then y else z
\end{verbatim}
Notice that, like all ids, the foralls of defm.Foo.op2 are at the top.

When we come across an instance decl, we may need to use the default
methods:
\begin{verbatim}
instance Foo Int where {}
\end{verbatim}
gives
\begin{verbatim}
const.Foo.Int.op1 :: Int -> Bool
const.Foo.Int.op1 = defm.Foo.op1 Int dfun.Foo.Int

const.Foo.Int.op2 :: forall b. Ord b => Int -> b -> b -> b
const.Foo.Int.op2 = /\b -> defm.Foo.op2 Int b dfun.Foo.Int

dfun.Foo.Int :: Foo Int
dfun.Foo.Int = (const.Foo.Int.op1, const.Foo.Int.op2)
\end{verbatim}
Notice that, as with method selectors above, we assume that dictionary
application is curried, so there's no need to mention the Ord dictionary
in const.Foo.Int.op2
\begin{verbatim}
instance Foo a => Foo [a] where {}

dfun.Foo.List :: forall a. Foo a -> Foo [a]
dfun.Foo.List
  = /\ a -> \ dfoo_a ->
    let rec
	op1 = defm.Foo.op1 [a] dfoo_list
	op2 = /\b -> defm.Foo.op2 [a] b dfoo_list
	dfoo_list = (op1, op2)
    in
	dfoo_list
\end{verbatim}

\begin{code}
buildDefaultMethodBinds
	:: E
	-> [TyVar]
	-> InstOrigin
	-> Class
	-> TyVarTemplate
	-> [Id]
	-> RenamedMonoBinds
	-> TcM ([Inst], TypecheckedBinds)

buildDefaultMethodBinds e free_tyvars origin clas clas_tyvar_tmpl
			default_method_ids default_binds
  =	-- Deal with the method declarations themselves
    processInstBinds e
	 free_tyvars
	 (makeClassDeclDefaultMethodRhs clas origin default_method_ids)
	 []	-- No tyvars in scope for "this inst decl"
	 []	-- No insts available
	 default_method_ids
	 default_binds		`thenTc` \ (dicts_needed, default_binds') ->

    returnTc (dicts_needed, SingleBind (NonRecBind default_binds'))
\end{code}

@makeClassDeclDefaultMethodRhs@ builds the default method for a
class declaration when no explicit default method is given.

\begin{code}
makeClassDeclDefaultMethodRhs
	:: Class
	-> InstOrigin
	-> [Id]
	-> Int
	-> NF_TcM TypecheckedExpr

makeClassDeclDefaultMethodRhs clas origin method_ids tag
  = specTy origin (getIdUniType method_id) `thenNF_Tc` \ (tyvars, dicts, tau) ->

    returnNF_Tc (mkTyLam tyvars (
		 mkDictLam (map mkInstId dicts) (
		 App (mkTyApp (Var pAT_ERROR_ID) [tau])
		     (Lit (StringLit (_PK_ error_msg))))))
  where
    method_id = method_ids  !! (tag-1)
    class_op = (getClassOps clas) !! (tag-1)

    error_msg = "%D" -- => No default method for \"
	     ++ unencoded_part_of_msg

    unencoded_part_of_msg = escErrorMsg (
	_UNPK_ clas_mod ++ "." ++ _UNPK_ clas_name ++ "."
	     ++ (ppShow 80 (ppr PprForUser class_op))
	     ++ "\"" )

    (clas_mod, clas_name) = getOrigName clas
\end{code}
