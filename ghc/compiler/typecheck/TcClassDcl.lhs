%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
#include "HsVersions.h"

module TcClassDcl ( tcClassDecl1, tcClassDecls2 ) where

IMP_Ubiq()

import HsSyn		( ClassDecl(..), HsBinds(..), Bind(..), MonoBinds(..),
			  Match(..), GRHSsAndBinds(..), GRHS(..), HsExpr(..),
			  HsLit(..), OutPat(..), Sig(..), PolyType(..), MonoType, 
			  Stmt, Qualifier, ArithSeqInfo, InPat, Fake )
import HsPragmas	( ClassPragmas(..) )
import RnHsSyn		( RenamedClassDecl(..), RenamedClassPragmas(..),
			  RenamedClassOpSig(..), SYN_IE(RenamedMonoBinds),
			  RenamedGenPragmas(..), RenamedContext(..),
			  RnName{-instance Uniquable-}
			)
import TcHsSyn		( TcIdOcc(..), SYN_IE(TcHsBinds), SYN_IE(TcMonoBinds), SYN_IE(TcExpr),
			  mkHsTyApp, mkHsTyLam, mkHsDictApp, mkHsDictLam, tcIdType )

import Inst		( Inst, InstOrigin(..), SYN_IE(LIE), emptyLIE, plusLIE, newDicts, newMethod )
import TcEnv		( tcLookupClass, tcLookupTyVar, tcLookupTyCon, newLocalIds, tcExtendGlobalTyVars )
import TcInstDcls	( processInstBinds )
import TcKind		( unifyKind, TcKind )
import TcMonad		hiding ( rnMtoTcM )
import TcMonoType	( tcPolyType, tcMonoType, tcContext )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( SYN_IE(TcType), SYN_IE(TcTyVar), tcInstType, tcInstSigTyVars, tcInstSigType )

import Bag		( foldBag, unionManyBags )
import Class		( GenClass, mkClass, mkClassOp, classBigSig, 
			  classOps, classOpString, classOpLocalType,
			  classOpTagByString, SYN_IE(ClassOp)
			)
import Id		( mkSuperDictSelId, mkMethodSelId, mkDefaultMethodId,
			  idType )
import IdInfo
import Name		( isLocallyDefined, origName, getLocalName )
import PrelVals		( nO_DEFAULT_METHOD_ERROR_ID )
import PprStyle
import Pretty
import PprType		( GenType, GenTyVar, GenClassOp )
import SpecEnv		( SpecEnv )
import SrcLoc		( mkGeneratedSrcLoc )
import Type		( mkFunTy, mkTyVarTy, mkTyVarTys, mkDictTy,
			  mkForAllTy, mkSigmaTy, splitSigmaTy)
import TysWiredIn	( stringTy )
import TyVar		( unitTyVarSet, GenTyVar )
import Unique		( Unique )			 
import Util


-- import TcPragmas	( tcGenPragmas, tcClassOpPragmas )
tcGenPragmas ty id ps = returnNF_Tc noIdInfo
tcClassOpPragmas ty sel def spec ps = returnNF_Tc (noIdInfo `addInfo` spec, 
						   noIdInfo)
\end{code}



Dictionary handling
~~~~~~~~~~~~~~~~~~~
Every class implicitly declares a new data type, corresponding to dictionaries
of that class. So, for example:

	class (D a) => C a where
	  op1 :: a -> a
	  op2 :: forall b. Ord b => a -> b -> b

would implicitly declare

	data CDict a = CDict (D a)	
			     (a -> a)
			     (forall b. Ord b => a -> b -> b)

(We could use a record decl, but that means changing more of the existing apparatus.
One step at at time!)

For classes with just one superclass+method, we use a newtype decl instead:

	class C a where
	  op :: forallb. a -> b -> b

generates

	newtype CDict a = CDict (forall b. a -> b -> b)

Now DictTy in Type is just a form of type synomym: 
	DictTy c t = TyConTy CDict `AppTy` t

Death to "ExpandingDicts".


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
    let
	(ops, op_sel_ids, defm_ids) = unzip3 sig_stuff
	clas = mkClass (uniqueOf class_name) (getName class_name) rec_tyvar
		       scs sc_sel_ids ops op_sel_ids defm_ids
		       rec_class_inst_env
    in
    returnTc clas
\end{code}


    let
	clas_ty = mkTyVarTy clas_tyvar
	dict_component_tys = [mkDictTy sc clas_ty | sc <- scs] ++
			     [classOpLocalType op | op <- ops])
 	new_or_data = case dict_component_tys of
			[_]   -> NewType
			other -> DataType

        dict_con_id = mkDataCon class_name
			   [NotMarkedStrict]
			   [{- No labelled fields -}]
		      	   [clas_tyvar]
		      	   [{-No context-}]
			   dict_component_tys
		      	   tycon

	tycon = mkDataTyCon class_name
			    (tyVarKind rec_tyvar `mkArrowKind` mkBoxedTypeKind)
			    [rec_tyvar]
			    [{- Empty context -}]
			    [dict_con_id]
			    [{- No derived classes -}]
			    new_or_data
    in


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
	  (super_classes `zip` maybe_pragmas) `thenTc` \ sc_sel_ids ->
	  -- NB: we worry about matching list lengths below

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
		rec_tyvar_ty = mkTyVarTy rec_tyvar
		ty = mkForAllTy rec_tyvar $
		     mkFunTy (mkDictTy rec_class   rec_tyvar_ty)
			     (mkDictTy super_class rec_tyvar_ty)
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
		       op_ty
		       pragmas src_loc)
  = tcAddSrcLoc src_loc $
    fixTc ( \ ~(_, rec_sel_id, rec_defm_id) ->	-- Knot for pragmas

	-- Check the type signature.  NB that the envt *already has*
	-- bindings for the type variables; see comments in TcTyAndClassDcls.

    -- NB: Renamer checks that the class type variable is mentioned in local_ty,
    -- and that it is not constrained by theta
    tcPolyType op_ty				`thenTc` \ local_ty ->
    let
	global_ty   = mkSigmaTy [rec_clas_tyvar] 
			        [(rec_clas, mkTyVarTy rec_clas_tyvar)]
			        local_ty
	class_op_nm = getLocalName op_name
	class_op    = mkClassOp class_op_nm
				(classOpTagByString rec_clas{-yeeps!-} class_op_nm)
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
	op_uniq = uniqueOf op_name
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

  | not (isLocallyDefined class_name)
  = returnNF_Tc (emptyLIE, EmptyBinds)

  | otherwise	-- It is locally defined
  = recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyBinds)) $
    tcAddSrcLoc src_loc		     		      $

	-- Get the relevant class
    tcLookupClass class_name		`thenNF_Tc` \ (_, clas) ->
    let
	(tyvar, scs, sc_sel_ids, ops, op_sel_ids, defm_ids)
	  = classBigSig clas
    in
    tcInstSigTyVars [tyvar]		`thenNF_Tc` \ ([clas_tyvar], _, _) ->

	-- Generate bindings for the selector functions
    buildSelectors clas tyvar clas_tyvar scs sc_sel_ids ops op_sel_ids
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
	       -> TyVar			-- Class type variable
	       -> TcTyVar s		-- Instantiated class type variable (TyVarTy)
	       -> [Class] -> [Id]	-- Superclasses and selectors
	       -> [ClassOp] -> [Id]	-- Class ops and selectors
	       -> NF_TcM s (TcHsBinds s)

buildSelectors clas clas_tyvar clas_tc_tyvar scs sc_sel_ids ops op_sel_ids
  =
	-- Make new Ids for the components of the dictionary
    let
	clas_tyvar_ty = mkTyVarTy clas_tc_tyvar
	mk_op_ty = tcInstType [(clas_tyvar, clas_tyvar_ty)] . classOpLocalType 
    in
    mapNF_Tc mk_op_ty ops  				`thenNF_Tc` \ op_tys ->
    newLocalIds (map classOpString ops) op_tys	`thenNF_Tc` \ method_ids ->

    newDicts ClassDeclOrigin 
	     [ (super_clas, clas_tyvar_ty)
	     | super_clas <- scs ]			`thenNF_Tc` \ (_,dict_ids) ->

    newDicts ClassDeclOrigin 
	     [ (clas, clas_tyvar_ty) ]			`thenNF_Tc` \ (_,[clas_dict]) ->

	 -- Make suitable bindings for the selectors
    let
	mk_sel sel_id method_or_dict
	  = mkSelBind sel_id clas_tc_tyvar clas_dict dict_ids method_ids method_or_dict
    in
    listNF_Tc (zipWithEqual "mk_sel1" mk_sel op_sel_ids method_ids) `thenNF_Tc` \ op_sel_binds ->
    listNF_Tc (zipWithEqual "mk_sel2" mk_sel sc_sel_ids dict_ids)   `thenNF_Tc` \ sc_sel_binds ->

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

======================	OLD ============================
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
========================= END OF OLD ===========================

NEW COMMENT: instead we now go for op1_sel' above.  Seems tidier and
the rest of the compiler darn well ought to cope.



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
  = 
	-- sel_id = /\ clas_tyvar -> \ clas_dict ->
	--	    case clas_dict of 
	--		 <dicts..methods> -> method_or_dict

    returnNF_Tc (VarMonoBind (RealId sel_id)  (
		 TyLam [clas_tyvar] (
		 DictLam [clas_dict] (
		 HsCase
		   (HsVar clas_dict)
                   ([PatMatch  (DictPat dicts methods) (
		     GRHSMatch (GRHSsAndBindsOut
			[OtherwiseGRHS
			   (HsVar method_or_dict)
		  	   mkGeneratedSrcLoc]
			EmptyBinds
			(idType op)))])
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

====================== OLD ==================
\begin{verbatim}
defm.Foo.op2 :: forall a, b. (Foo a, Ord b) => a -> b -> b -> b
defm.Foo.op2 = /\ a b -> \ dfoo dord -> \x y z ->
		  if (op1 a dfoo x) && (< b dord y z) then y else z
\end{verbatim}
Notice that, like all ids, the foralls of defm.Foo.op2 are at the top.
====================== END OF OLD ===================

NEW:
\begin{verbatim}
defm.Foo.op2 :: forall a. Foo a => forall b. Ord b => a -> b -> b -> b
defm.Foo.op2 = /\ a -> \ dfoo -> /\ b -> \ dord -> \x y z ->
		  if (op1 a dfoo x) && (< b dord y z) then y else z
\end{verbatim}


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
const.Foo.Int.op2 = defm.Foo.op2 Int dfun.Foo.Int

dfun.Foo.Int :: Foo Int
dfun.Foo.Int = (const.Foo.Int.op1, const.Foo.Int.op2)
\end{verbatim}
Notice that, as with method selectors above, we assume that dictionary
application is curried, so there's no need to mention the Ord dictionary
in const.Foo.Int.op2 (or the type variable).

\begin{verbatim}
instance Foo a => Foo [a] where {}

dfun.Foo.List :: forall a. Foo a -> Foo [a]
dfun.Foo.List
  = /\ a -> \ dfoo_a ->
    let rec
	op1 = defm.Foo.op1 [a] dfoo_list
	op2 = defm.Foo.op2 [a] dfoo_list
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
  = newDicts origin [(clas,inst_ty)]			`thenNF_Tc` \ (this_dict, [this_dict_id]) ->
    mapAndUnzipNF_Tc mk_method default_method_ids	`thenNF_Tc` \ (insts_s, local_defm_ids) ->
    let
	avail_insts    = this_dict `plusLIE` unionManyBags insts_s 	-- Insts available
	clas_tyvar_set = unitTyVarSet clas_tyvar
    in
    tcExtendGlobalTyVars clas_tyvar_set (
	processInstBinds
	   clas
	   (makeClassDeclDefaultMethodRhs clas local_defm_ids)
	   avail_insts
	   local_defm_ids
	   default_binds
    )					`thenTc` \ (insts_needed, default_binds') ->

    tcSimplifyAndCheck
	clas_tyvar_set
	avail_insts
	insts_needed			`thenTc` \ (const_lie, dict_binds) ->
	

    let
	defm_binds = AbsBinds
		 	[clas_tyvar]
			[this_dict_id]
			(local_defm_ids `zip` map RealId default_method_ids)
			dict_binds
			(RecBind default_binds')
    in
    returnTc (const_lie, defm_binds)
  where
    inst_ty = mkTyVarTy clas_tyvar
    mk_method defm_id = newMethod origin (RealId defm_id) [inst_ty]
    origin = ClassDeclOrigin
\end{code}

@makeClassDeclDefaultMethodRhs@ builds the default method for a
class declaration when no explicit default method is given.

\begin{code}
makeClassDeclDefaultMethodRhs
	:: Class
	-> [TcIdOcc s]
	-> Int
	-> NF_TcM s (TcExpr s)

makeClassDeclDefaultMethodRhs clas method_ids tag
  = 	-- Return the expression
	--	error ty "No default method for ..."
	-- The interesting thing is that method_ty is a for-all type;
	-- this is fun, although unusual in a type application!

    returnNF_Tc (HsApp (mkHsTyApp (HsVar (RealId nO_DEFAULT_METHOD_ERROR_ID)) [tcIdType method_id])
		       (HsLitOut (HsString (_PK_ error_msg)) stringTy))

{-	OLD AND COMPLICATED
    tcInstSigType () 	`thenNF_Tc` \ method_ty ->
    let 
	(tyvars, theta, tau) = splitSigmaTy method_ty 
    in	
    newDicts ClassDeclOrigin theta	`thenNF_Tc` \ (lie, dict_ids) ->

    returnNF_Tc (mkHsTyLam tyvars (
		 mkHsDictLam dict_ids (
		 HsApp (mkHsTyApp (HsVar (RealId nO_DEFAULT_METHOD_ERROR_ID)) [tau])
		     (HsLitOut (HsString (_PK_ error_msg)) stringTy))))
-}

  where
    (OrigName clas_mod clas_name) = origName "makeClassDeclDefaultMethodRhs" clas

    method_id = method_ids  !! (tag-1)
    class_op  = (classOps clas) !! (tag-1)

    error_msg = _UNPK_ clas_mod ++ "." ++ _UNPK_ clas_name ++ "."
		 ++ (ppShow 80 (ppr PprForUser class_op))
		 ++ "\""
\end{code}


Contexts
~~~~~~~~
\begin{code}
classDeclCtxt class_name sty
  = ppCat [ppStr "In the class declaration for", ppr sty class_name]
\end{code}
