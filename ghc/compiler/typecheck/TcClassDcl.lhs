%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
#include "HsVersions.h"

module TcClassDcl (
	tcClassDecl1, tcClassDecls2
    ) where

import Ubiq

import HsSyn		( ClassDecl(..), HsBinds(..), Bind(..), MonoBinds(..),
			  Match(..), GRHSsAndBinds(..), GRHS(..), HsExpr(..),
			  HsLit(..), OutPat(..), Sig(..), PolyType(..), MonoType, 
			  Stmt, Qual, ArithSeqInfo, InPat, Fake )
import HsPragmas	( ClassPragmas(..) )
import RnHsSyn		( RenamedClassDecl(..), RenamedClassPragmas(..),
			  RenamedClassOpSig(..), RenamedMonoBinds(..),
			  RenamedGenPragmas(..), RenamedContext(..) )
import TcHsSyn		( TcIdOcc(..), TcHsBinds(..), TcMonoBinds(..), TcExpr(..),
			  mkHsTyApp, mkHsTyLam, mkHsDictApp, mkHsDictLam, unZonkId )

import TcMonad
import GenSpecEtc	( specTy )
import Inst		( Inst, InstOrigin(..), LIE(..), emptyLIE, plusLIE, newDicts )
import TcEnv		( tcLookupClass, tcLookupTyVar, tcLookupTyCon, newLocalIds)
import TcInstDcls	( processInstBinds )
import TcKind		( unifyKind )
import TcMonoType	( tcMonoType, tcContext )
import TcType		( TcTyVar(..), tcInstType, tcInstTyVar )
import TcKind		( TcKind )

import Bag		( foldBag )
import Class		( GenClass, mkClass, mkClassOp, getClassBigSig, 
			  getClassOps, getClassOpString, getClassOpLocalType )
import CoreUtils	( escErrorMsg )
import Id		( mkSuperDictSelId, mkMethodSelId, mkDefaultMethodId,
			  idType )
import IdInfo		( noIdInfo )
import Name		( Name, getNameFullName, getTagFromClassOpName )
import PrelVals		( pAT_ERROR_ID )
import PprStyle
import Pretty
import PprType		( GenType, GenTyVar, GenClassOp )
import SpecEnv		( SpecEnv(..) )
import SrcLoc		( mkGeneratedSrcLoc )
import Type		( mkFunTy, mkTyVarTy, mkDictTy,
			  mkForAllTy, mkSigmaTy, splitSigmaTy)
import TysWiredIn	( stringTy )
import TyVar		( GenTyVar )			 
import Unique		( Unique )			 
import Util

-- import TcPragmas	( tcGenPragmas, tcClassOpPragmas )
tcGenPragmas ty id ps = returnNF_Tc noIdInfo
tcClassOpPragmas ty sel def spec ps = returnNF_Tc (noIdInfo, noIdInfo)

\end{code}

\begin{code}
tcClassDecl1 rec_inst_mapper
      	     (ClassDecl context class_name
			tyvar_name class_sigs def_methods pragmas src_loc)
  = tcAddSrcLoc src_loc	$
    tcAddErrCtxt (classDeclCtxt class_name) $

	-- LOOK THINGS UP IN THE ENVIRONMENT
    tcLookupClass class_name	`thenNF_Tc` \ (class_kind, rec_class) ->
    tcLookupTyVar tyvar_name	`thenNF_Tc` \ (tyvar_kind, rec_tyvar) ->
    let
	(rec_class_inst_env, rec_class_op_inst_fn) = rec_inst_mapper rec_class
    in

	-- FORCE THE CLASS AND ITS TYVAR TO HAVE SAME KIND
    unifyKind class_kind tyvar_kind	`thenTc_`

	-- CHECK THE CONTEXT
    tcClassContext rec_class rec_tyvar context pragmas	
				`thenTc` \ (scs, sc_sel_ids) ->

	-- CHECK THE CLASS SIGNATURES,
    mapTc (tcClassSig rec_class rec_tyvar rec_class_op_inst_fn) class_sigs
				`thenTc` \ sig_stuff ->

	-- MAKE THE CLASS OBJECT ITSELF
    tcGetUnique			`thenNF_Tc` \ uniq ->
    let
	(ops, op_sel_ids, defm_ids) = unzip3 sig_stuff
	clas = mkClass uniq (getNameFullName class_name) rec_tyvar
		       scs sc_sel_ids ops op_sel_ids defm_ids
		       rec_class_inst_env
    in
    returnTc clas
\end{code}


\begin{code}
tcClassContext :: Class -> TyVar
	       -> RenamedContext 	-- class context
	       -> RenamedClassPragmas	-- pragmas for superclasses  
	       -> TcM s ([Class],	-- the superclasses
			 [Id])  	-- superclass selector Ids

tcClassContext rec_class rec_tyvar context pragmas
  = 	-- Check the context.
	-- The renamer has already checked that the context mentions
	-- only the type variable of the class decl.
    tcContext context			`thenTc` \ theta ->
    let
      super_classes = [ supers | (supers, _) <- theta ]
    in

	-- Make super-class selector ids
    mapTc (mk_super_id rec_class) 
	  (super_classes `zip` maybe_pragmas)	`thenTc` \ sc_sel_ids ->

	-- Done
    returnTc (super_classes, sc_sel_ids)

  where
    mk_super_id rec_class (super_class, maybe_pragma)
        = fixTc ( \ rec_super_id ->
	    tcGetUnique			`thenNF_Tc` \ uniq ->

		-- GET THE PRAGMA INFO FOR THE SUPERCLASS
	    (case maybe_pragma of
		Nothing   -> returnNF_Tc noIdInfo
		Just prag -> tcGenPragmas Nothing{-ty unknown-} rec_super_id prag
	    )				`thenNF_Tc` \ id_info ->
	    let
	      ty = mkForAllTy rec_tyvar (
	           mkFunTy (mkDictTy rec_class   (mkTyVarTy rec_tyvar))
		           (mkDictTy super_class (mkTyVarTy rec_tyvar))
		   )
	    in
		-- BUILD THE SUPERCLASS ID
	    returnTc (mkSuperDictSelId uniq rec_class super_class ty id_info)
	  )

    maybe_pragmas :: [Maybe RenamedGenPragmas]
    maybe_pragmas = case pragmas of
			NoClassPragmas	       -> repeat Nothing
			SuperDictPragmas prags -> ASSERT(length prags == length context)
						  map Just prags
			-- If there are any pragmas there should
			-- be one for each superclass



tcClassSig :: Class	    		-- Knot tying only!
	   -> TyVar		 	-- The class type variable, used for error check only
	   -> (ClassOp -> SpecEnv)	-- Ditto; the spec info for the class ops
	   -> RenamedClassOpSig
	   -> TcM s (ClassOp,		-- class op
		     Id,		-- selector id
		     Id)		-- default-method ids

tcClassSig rec_clas rec_clas_tyvar rec_classop_spec_fn
	   (ClassOpSig op_name
		       (HsForAllTy tyvar_names context monotype)
		       pragmas src_loc)
  = tcAddSrcLoc src_loc $
    fixTc ( \ ~(_, rec_sel_id, rec_defm_id) ->	-- Knot for pragmas

	-- Check the type signature.  NB that the envt *already has*
	-- bindings for the type variables; see comments in TcTyAndClassDcls.
    tcContext context				`thenTc`    \ theta ->
    tcMonoType monotype				`thenTc`    \ tau ->
    mapAndUnzipNF_Tc tcLookupTyVar tyvar_names	`thenNF_Tc` \ (_,tyvars) ->
    let
	full_tyvars = rec_clas_tyvar : tyvars
	full_theta  = (rec_clas, mkTyVarTy rec_clas_tyvar) : theta
	global_ty   = mkSigmaTy full_tyvars full_theta tau
	local_ty    = mkSigmaTy tyvars theta tau
	class_op    = mkClassOp (getOccurrenceName op_name)
				(getTagFromClassOpName op_name)
				local_ty
    in

	-- Munch the pragmas
    tcClassOpPragmas
		global_ty
		rec_sel_id rec_defm_id
		(rec_classop_spec_fn class_op)
		pragmas				`thenNF_Tc` \ (op_info, defm_info) ->

	-- Build the selector id and default method id
    tcGetUnique					`thenNF_Tc` \ d_uniq ->
    let
	op_uniq = getItsUnique op_name
	sel_id  = mkMethodSelId     op_uniq rec_clas class_op global_ty op_info
	defm_id = mkDefaultMethodId d_uniq  rec_clas class_op False global_ty defm_info
			-- ToDo: improve the "False"
    in
    returnTc (class_op, sel_id, defm_id)
    )
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
and superclass dictionary.
\end{enumerate}

Pass~2 only applies to locally-defined class declarations.

The function @tcClassDecls2@ just arranges to apply @tcClassDecl2@ to
each local class decl.

\begin{code}
tcClassDecls2 :: Bag RenamedClassDecl
	      -> NF_TcM s (LIE s, TcHsBinds s)

tcClassDecls2 decls
  = foldBag combine
	    tcClassDecl2
	    (returnNF_Tc (emptyLIE, EmptyBinds))
	    decls
  where
    combine tc1 tc2 = tc1 `thenNF_Tc` \ (lie1, binds1) ->
		      tc2 `thenNF_Tc` \ (lie2, binds2) ->
		      returnNF_Tc (lie1 `plusLIE` lie2,
				   binds1 `ThenBinds` binds2)
\end{code}

@tcClassDecl2@ is the business end of things.

\begin{code}
tcClassDecl2 :: RenamedClassDecl	-- The class declaration
	     -> NF_TcM s (LIE s, TcHsBinds s)

tcClassDecl2 (ClassDecl context class_name
			tyvar_name class_sigs default_binds pragmas src_loc)
  = recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyBinds)) $
    tcAddSrcLoc src_loc		     		      $

	-- Get the relevant class
    tcLookupClass class_name		`thenNF_Tc` \ (_, clas) ->
    let
	(tyvar, scs, sc_sel_ids, ops, op_sel_ids, defm_ids)
	  = getClassBigSig clas
    in
    tcInstTyVar tyvar			`thenNF_Tc` \ clas_tyvar ->

	-- Generate bindings for the selector functions
    buildSelectors clas clas_tyvar scs sc_sel_ids ops op_sel_ids
						`thenNF_Tc` \ sel_binds ->
	-- Ditto for the methods
    buildDefaultMethodBinds clas clas_tyvar defm_ids default_binds
						`thenTc` \ (const_insts, meth_binds) ->

    returnTc (const_insts, sel_binds `ThenBinds` meth_binds)
\end{code}

%************************************************************************
%*									*
\subsection[ClassDcl-bld-sels]{Building the selector functions for methods and superclasses}
%*									*
%************************************************************************

\begin{code}
buildSelectors :: Class			-- The class object
	       -> TcTyVar s		-- Class type variable
	       -> [Class] -> [Id]	-- Superclasses and selectors
	       -> [ClassOp] -> [Id]	-- Class ops and selectors
	       -> NF_TcM s (TcHsBinds s)

buildSelectors clas clas_tyvar scs sc_sel_ids ops op_sel_ids
  =
	-- Make new Ids for the components of the dictionary
    mapNF_Tc (tcInstType [] . getClassOpLocalType) ops	`thenNF_Tc` \ op_tys ->

    newLocalIds (map getClassOpString ops) op_tys	`thenNF_Tc` \ method_ids ->

    newDicts ClassDeclOrigin 
	     [ (super_clas, mkTyVarTy clas_tyvar)
	     | super_clas <- scs ]			`thenNF_Tc` \ (_,dict_ids) ->

    newDicts ClassDeclOrigin 
	     [ (clas, mkTyVarTy clas_tyvar) ]		`thenNF_Tc` \ (_,[clas_dict]) ->

	 -- Make suitable bindings for the selectors
    let
        tc_method_ids = map TcId method_ids

	mk_sel sel_id method_or_dict
	  = mkSelBind sel_id clas_tyvar clas_dict dict_ids tc_method_ids method_or_dict
    in
    listNF_Tc (zipWithEqual mk_sel op_sel_ids tc_method_ids) `thenNF_Tc` \ op_sel_binds ->
    listNF_Tc (zipWithEqual mk_sel sc_sel_ids dict_ids)      `thenNF_Tc` \ sc_sel_binds ->

    returnNF_Tc (SingleBind (
		 NonRecBind (
		 foldr AndMonoBinds
		       (foldr AndMonoBinds EmptyMonoBinds op_sel_binds)
		       sc_sel_binds
		 )))
\end{code}

%************************************************************************
%*									*
\subsection[ClassDcl-misc]{Miscellaneous}
%*									*
%************************************************************************

Make a selector expression for @sel_id@ from a dictionary @clas_dict@
consisting of @dicts@ and @methods@.

We have to do a bit of jiggery pokery to get the type variables right.
Suppose we have the class decl:
\begin{verbatim}
	class Foo a where
		op1 :: Ord b => a -> b -> a
		op2 :: ...
\end{verbatim}
Then the method selector for \tr{op1} is like this:
\begin{verbatim}
	op1_sel = /\a b -> \dFoo dOrd -> case dFoo of
					 (op1_method,op2_method) -> op1_method b dOrd
\end{verbatim}
Note that the type variable for \tr{b} and the (Ord b) dictionary
are lifted to the top lambda, and
\tr{op1_method} is applied to them.  This is preferable to the alternative:
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

NOTE that we return a TcMonoBinds (which is later zonked) even though
there's no real back-substitution to do. It's just simpler this way!

NOTE ALSO that the selector has no free type variables, so we
don't bother to instantiate the class-op's local type; instead
we just use the variables inside it.

\begin{code}
mkSelBind :: Id 			-- the selector id
	  -> TcTyVar s -> TcIdOcc s	-- class tyvar and dict
	  -> [TcIdOcc s] -> [TcIdOcc s] -- superclasses and methods in class dict
	  -> TcIdOcc s 			-- the superclass/method being slected
	  -> NF_TcM s (TcMonoBinds s)

mkSelBind sel_id clas_tyvar clas_dict dicts methods method_or_dict@(TcId op)
  = let
	(op_tyvars,op_theta,op_tau) = splitSigmaTy (idType op)
	op_tys = map mkTyVarTy op_tyvars
    in
    newDicts ClassDeclOrigin op_theta	`thenNF_Tc` \ (_, op_dicts) ->

	-- sel_id = /\ clas_tyvar op_tyvars -> \ clas_dict op_dicts ->
	--	    case clas_dict of 
	--		 <dicts..methods> -> method_or_dict op_tyvars op_dicts

    returnNF_Tc (VarMonoBind (RealId sel_id)  (
		 TyLam (clas_tyvar:op_tyvars) (
		 DictLam (clas_dict:op_dicts) (
		 HsCase
		   (HsVar clas_dict)
                   ([PatMatch  (DictPat dicts methods) (
		     GRHSMatch (GRHSsAndBindsOut
			[OtherwiseGRHS
			   (mkHsDictApp (mkHsTyApp (HsVar method_or_dict) op_tys) op_dicts)
		  	   mkGeneratedSrcLoc]
			EmptyBinds
			op_tau))])
		    mkGeneratedSrcLoc
		 ))))
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
	:: Class
	-> TcTyVar s
	-> [Id]
	-> RenamedMonoBinds
	-> TcM s (LIE s, TcHsBinds s)

buildDefaultMethodBinds clas clas_tyvar
			default_method_ids default_binds
  =	-- Deal with the method declarations themselves
    mapNF_Tc unZonkId default_method_ids	`thenNF_Tc` \ tc_defm_ids ->
    processInstBinds
	 (makeClassDeclDefaultMethodRhs clas default_method_ids)
	 []		-- No tyvars in scope for "this inst decl"
	 emptyLIE 	-- No insts available
	 (map TcId tc_defm_ids)
	 default_binds		`thenTc` \ (dicts_needed, default_binds') ->

    returnTc (dicts_needed, SingleBind (NonRecBind default_binds'))
\end{code}

@makeClassDeclDefaultMethodRhs@ builds the default method for a
class declaration when no explicit default method is given.

\begin{code}
makeClassDeclDefaultMethodRhs
	:: Class
	-> [Id]
	-> Int
	-> NF_TcM s (TcExpr s)

makeClassDeclDefaultMethodRhs clas method_ids tag
  = specTy ClassDeclOrigin (idType method_id) `thenNF_Tc` \ (tyvars, dicts, tau, dict_ids) ->

    returnNF_Tc (mkHsTyLam tyvars (
		 mkHsDictLam dict_ids (
		 HsApp (mkHsTyApp (HsVar (RealId pAT_ERROR_ID)) [tau])
		     (HsLitOut (HsString (_PK_ error_msg)) stringTy))))
  where
    (clas_mod, clas_name) = getOrigName clas

    method_id = method_ids  !! (tag-1)
    class_op = (getClassOps clas) !! (tag-1)

    error_msg = "%D" -- => No default method for \"
	     ++ unencoded_part_of_msg

    unencoded_part_of_msg = escErrorMsg (
	_UNPK_ clas_mod ++ "." ++ _UNPK_ clas_name ++ "."
	     ++ (ppShow 80 (ppr PprForUser class_op))
	     ++ "\"" )
\end{code}


Contexts
~~~~~~~~
\begin{code}
classDeclCtxt class_name sty
  = ppCat [ppStr "In the class declaration for", ppr sty class_name]
\end{code}
