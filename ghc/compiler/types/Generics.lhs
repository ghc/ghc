\begin{code}
module Generics ( mkTyConGenInfo, mkGenericRhs, 
		  validGenericInstanceType, validGenericMethodType
    ) where


import RnHsSyn		( RenamedHsExpr )
import HsSyn		( HsExpr(..), InPat(..), mkSimpleMatch, placeHolderType )

import Type             ( Type, isUnLiftedType, tyVarsOfType, tyVarsOfTypes,
			  mkTyVarTys, mkForAllTys, mkTyConApp, 
			  mkFunTy, isTyVarTy, getTyVar_maybe,
			  funTyCon
			)
import TcType		( tcSplitTyConApp_maybe, tcSplitSigmaTy, tcSplitSigmaTy )
import DataCon          ( DataCon, dataConOrigArgTys, dataConWrapId, dataConId, isExistentialDataCon )

import TyCon            ( TyCon, tyConTyVars, tyConDataCons_maybe, 
			  tyConGenInfo, isNewTyCon, newTyConRep, isBoxedTupleTyCon
			)
import Name		( Name, mkSysLocalName )
import CoreSyn          ( mkLams, Expr(..), CoreExpr, AltCon(..), 
			  mkConApp, Alt, mkTyApps, mkVarApps )
import CoreUtils	( exprArity )
import BasicTypes       ( EP(..), Boxity(..) )
import Var              ( TyVar )
import VarSet		( varSetElems )
import Id               ( Id, mkVanillaGlobal, idType, idName, 
			  mkTemplateLocal, mkTemplateLocalsNum
			) 
import TysWiredIn       ( genericTyCons,
			  genUnitTyCon, genUnitDataCon, plusTyCon, inrDataCon,
			  inlDataCon, crossTyCon, crossDataCon
			)
import IdInfo           ( noCafNoTyGenIdInfo, setUnfoldingInfo, setArityInfo )
import CoreUnfold       ( mkTopUnfolding ) 

import Maybe		( isNothing )
import SrcLoc		( builtinSrcLoc )
import Unique		( mkBuiltinUnique )
import Util             ( takeList )
import Outputable 

#include "HsVersions.h"
\end{code}

Roadmap of what's where in the Generics work.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Parser
No real checks.

RnSource.rnHsType
  Checks that HsNumTy has a "1" in it.

TcInstDcls.mkGenericInstance:
  Checks for invalid type patterns, such as f {| Int |}

TcClassDcl.tcClassSig
  Checks for a method type that is too complicated;
	e.g. has for-alls or lists in it
  We could lift this restriction

TcClassDecl.mkDefMethRhs
  Checks that the instance type is simple, in an instance decl 
  where we let the compiler fill in a generic method.
	e.g.  instance C (T Int)
  	is not valid if C has generic methods.

TcClassDecl.checkGenericClassIsUnary
  Checks that we don't have generic methods in a multi-parameter class

TcClassDecl.checkDefaultBinds
  Checks that all the equations for a method in a class decl
  are generic, or all are non-generic


			
Checking that the type constructors which are present in Generic
patterns (not Unit, this is done differently) is done in mk_inst_info
(TcInstDecls) in a call to tcHsType (TcMonoBinds). This means that
HsOpTy is tied to Generic definitions which is not a very good design
feature, indeed a bug. However, the check is easy to move from
tcHsType back to mk_inst_info and everything will be fine. Also see
bug #5.

Generics.lhs

Making generic information to put into a tycon. Constructs the
representation type, which, I think, are not used later. Perhaps it is
worth removing them from the GI datatype. Although it does get used in
the construction of conversion functions (internally).

TyCon.lhs

Just stores generic information, accessible by tyConGenInfo or tyConGenIds.

TysWiredIn.lhs

Defines generic and other type and data constructors.

This is sadly incomplete, but will be added to.


Bugs & shortcomings of existing implementation:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2. Another pretty big bug I dscovered at the last minute when I was
testing the code is that at the moment the type variable of the class
is scoped over the entire declaration, including the patterns. For
instance, if I have the following code,

class Er a where
 ...
  er {| Plus a b |} (Inl x) (Inl y) = er x y 
  er {| Plus a b |} (Inr x) (Inr y) = er x y 
  er {| Plus a b |} _ _ = False
 
and I print out the types of the generic patterns, I get the
following.  Note that all the variable names for "a" are the same,
while for "b" they are all different.

check_ty
    [std.Generics.Plus{-33u,i-} a{-r6Z-} b{-r7g-},
     std.Generics.Plus{-33u,i-} a{-r6Z-} b{-r7m-},
     std.Generics.Plus{-33u,i-} a{-r6Z-} b{-r7p-}]

This is a bug as if I change the code to

 er {| Plus c b |} (Inl x)  (Inl y) = er x y 

all the names come out to be different.

Thus, all the types (Plus a b) come out to be different, so I cannot
compare them and test whether they are all the same and thus cannot
return an error if the type variables are different.

Temporary fix/hack. I am not checking for this, I just assume they are
the same, see line "check_ty = True" in TcInstDecls. When we resolve
the issue with variables, though - I assume that we will make them to
be the same in all the type patterns, jus uncomment the check and
everything should work smoothly.

Hence, I have also left the rather silly construction of:
* extracting all the type variables from all the types
* putting them *all* into the environment
* typechecking all the types
* selecting one of them and using it as the instance_ty.

(the alternative is to make sure that all the types are the same,
taking one, extracting its variables, putting them into the environment,
type checking it, using it as the instance_ty)
 
6. What happens if we do not supply all of the generic patterns? At
the moment, the compiler crashes with an error message "Non-exhaustive
patterns in a generic declaration" 


What has not been addressed:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Contexts. In the generated instance declarations for the 3 primitive
type constructors, we need contexts. It is unclear what those should
be. At the moment, we always say eg. (Eq a, Eq b) => Eq (Plus a b)

Type application. We have type application in expressions
(essentially) on the lhs of an equation. Do we want to allow it on the
RHS?

Scoping of type variables in a generic definition. At the moment, (see
TcInstDecls) we extract the type variables inside the type patterns
and add them to the environment. See my bug #2 above. This seems pretty
important.



%************************************************************************
%*									*
\subsection{Getting the representation type out}
%*									*
%************************************************************************

\begin{code}
validGenericInstanceType :: Type -> Bool
  -- Checks for validity of the type pattern in a generic
  -- declaration.  It's ok to have  
  --	f {| a + b |} ...
  -- but it's not OK to have
  --	f {| a + Int |}

validGenericInstanceType inst_ty
  = case tcSplitTyConApp_maybe inst_ty of
	Just (tycon, tys) ->  all isTyVarTy tys && tycon `elem` genericTyCons
	Nothing		  ->  False

validGenericMethodType :: Type -> Bool
  -- At the moment we only allow method types built from
  -- 	* type variables
  --	* function arrow
  --	* boxed tuples
  --	* an arbitrary type not involving the class type variables
  --		e.g. this is ok: 	forall b. Ord b => [b] -> a
  --	             where a is the class variable
validGenericMethodType ty 
  = valid tau
  where
    (local_tvs, _, tau) = tcSplitSigmaTy ty

    valid ty
      | isTyVarTy ty    = True
      | no_tyvars_in_ty	= True
      | otherwise	= case tcSplitTyConApp_maybe ty of
				Just (tc,tys) -> valid_tycon tc && all valid tys
				Nothing	      -> False
      where
	no_tyvars_in_ty = all (`elem` local_tvs) (varSetElems (tyVarsOfType ty))

    valid_tycon tc = tc == funTyCon || isBoxedTupleTyCon tc 
	-- Compare bimapApp, below
\end{code}


%************************************************************************
%*									*
\subsection{Generating representation types}
%*									*
%************************************************************************

\begin{code}
mkTyConGenInfo :: TyCon -> [Name] -> Maybe (EP Id)
-- mkTyConGenInfo is called twice
--	once from TysWiredIn for Tuples
--	once the typechecker TcTyDecls 
-- to generate generic types and conversion functions for all datatypes.
-- 
-- Must only be called with an algebraic type.
-- 
-- The two names are the names constructed by the renamer
-- for the fromT and toT conversion functions.

mkTyConGenInfo tycon [from_name, to_name]
  | isNothing maybe_datacons	-- Abstractly imported types don't have
  = Nothing			-- to/from operations, (and should not need them)

	-- If any of the constructor has an unboxed type as argument,
	-- then we can't build the embedding-projection pair, because
	-- it relies on instantiating *polymorphic* sum and product types
	-- at the argument types of the constructors
	-- Nor can we do the job if it's an existential data constructor,
  | or [ any isUnLiftedType (dataConOrigArgTys dc) || isExistentialDataCon dc
       | dc <- datacons ]
  = Nothing

  | otherwise
  = Just (EP { fromEP = mkVanillaGlobal from_name from_ty from_id_info,
	       toEP   = mkVanillaGlobal to_name   to_ty   to_id_info })
  where
    maybe_datacons = tyConDataCons_maybe tycon
    Just datacons  = maybe_datacons		-- [C, D]

    tyvars	   = tyConTyVars tycon		-- [a, b, c]
    tycon_ty	   = mkTyConApp tycon tyvar_tys	-- T a b c
    tyvar_tys      = mkTyVarTys tyvars

    from_id_info = noCafNoTyGenIdInfo `setUnfoldingInfo` mkTopUnfolding from_fn
				      `setArityInfo`     exprArity from_fn
    to_id_info   = noCafNoTyGenIdInfo `setUnfoldingInfo` mkTopUnfolding to_fn
				      `setArityInfo`     exprArity to_fn
	-- It's important to set the arity info, so that
	-- the calling convention (gotten from arity) 
	-- matches reality.

    from_ty = mkForAllTys tyvars (mkFunTy tycon_ty rep_ty)
    to_ty   = mkForAllTys tyvars (mkFunTy rep_ty tycon_ty)

    (from_fn, to_fn, rep_ty) 
	| isNewTyCon tycon
	= ( mkLams tyvars $ Lam x  $ Var x,
	    Var (dataConWrapId the_datacon),
	    newrep_ty )

	| otherwise
	= ( mkLams tyvars $ Lam x     $ Case (Var x) x from_alts,
	    mkLams tyvars $ Lam rep_var to_inner,
	    idType rep_var )

    -- x :: T a b c
    x  = mkTemplateLocal 1 tycon_ty

	    ----------------------
	    -- 	Newtypes only
    [the_datacon]  = datacons
    (_, newrep_ty) = newTyConRep tycon
       
	    ----------------------
	    -- 	Non-newtypes only
    -- Recurse over the sum first
    -- The "2" is the first free unique
    (from_alts, to_inner, rep_var) = mk_sum_stuff 2 tyvars datacons
    
    

----------------------------------------------------
--	Dealing with sums
----------------------------------------------------
mk_sum_stuff :: Int	 	-- Base for generating unique names
	     -> [TyVar]		-- Type variables over which the tycon is abstracted
	     -> [DataCon] 	-- The data constructors
	     -> ([Alt Id], CoreExpr, Id)

-- For example, given
--	data T = C | D Int Int Int
-- 
-- mk_sum_stuff v [C,D] = ([C -> Inl Unit, D a b c -> Inr (a :*: (b :*: c))],
--			   case cd of { Inl u -> C; 
--  					Inr abc -> case abc of { a :*: bc ->
--						   case bc  of { b :*: c ->
--						   D a b c }} },
--			   cd)

mk_sum_stuff i tyvars [datacon]
   = ([from_alt], to_body_fn app_exp, rep_var)
   where
     types        = dataConOrigArgTys datacon 
     datacon_vars = mkTemplateLocalsNum i types
     new_i        = i + length types 
     app_exp      = mkVarApps (Var (dataConId datacon)) (tyvars ++ datacon_vars)
     from_alt     = (DataAlt datacon, datacon_vars, from_alt_rhs)
     
     (_, from_alt_rhs, to_body_fn, rep_var) = mk_prod_stuff new_i datacon_vars

mk_sum_stuff i tyvars datacons
  = (wrap inlDataCon l_from_alts ++ wrap inrDataCon r_from_alts,
     Case (Var rep_var) rep_var [(DataAlt inlDataCon, [l_rep_var], l_to_body),
			 	 (DataAlt inrDataCon, [r_rep_var], r_to_body)],
     rep_var)
  where
    (l_datacons, r_datacons)	        = splitInHalf datacons
    (l_from_alts, l_to_body, l_rep_var) = mk_sum_stuff (i+2) tyvars l_datacons
    (r_from_alts, r_to_body, r_rep_var) = mk_sum_stuff (i+2) tyvars r_datacons
    rep_tys				= [idType l_rep_var, idType r_rep_var]
    rep_ty				= mkTyConApp plusTyCon rep_tys
    rep_var 				= mkTemplateLocal i rep_ty

    wrap :: DataCon -> [Alt Id] -> [Alt Id] 
	-- Wrap an application of the Inl or Inr constructor round each alternative
    wrap datacon alts
	= [(dc, args, App datacon_app rhs) | (dc,args,rhs) <- alts]
	where
	  datacon_app = mkTyApps (Var (dataConWrapId datacon)) rep_tys

----------------------------------------------------
--	Dealing with products
----------------------------------------------------
mk_prod_stuff :: Int			-- Base for unique names
	      -> [Id] 			-- arg-ids; args of the original user-defined constructor
					-- 	They are bound enclosing from_rhs
					-- 	Please bind these in the to_body_fn 
	      -> (Int,			-- Depleted unique-name supply
		  CoreExpr, 		-- from-rhs: puts together the representation from the arg_ids
		  CoreExpr -> CoreExpr,	-- to_body_fn: takes apart the representation
		  Id)			-- The rep-id; please bind this to the representation

-- For example:
-- mk_prod_stuff [a,b,c] = ( a :*: (b :*: c),
--			     \x -> case abc of { a :*: bc ->
--			 	   case bc  of { b :*: c  -> 
--				   x,
--		             abc )

-- We need to use different uqiques in the branches 
-- because the returned to_body_fns are nested.  
-- Hence the returned unqique-name supply

mk_prod_stuff i []		-- Unit case
  = (i,
     Var (dataConWrapId genUnitDataCon),
     \x -> x, 
     mkTemplateLocal i (mkTyConApp genUnitTyCon []))

mk_prod_stuff i [arg_var]	-- Singleton case
  = (i, Var arg_var, \x -> x, arg_var)

mk_prod_stuff i arg_vars	-- Two or more
  = (r_i, 
     mkConApp crossDataCon (map Type rep_tys ++ [l_alt_rhs, r_alt_rhs]),
     \x -> Case (Var rep_var) rep_var 
		[(DataAlt crossDataCon, [l_rep_var, r_rep_var], l_to_body_fn (r_to_body_fn x))],
     rep_var)
  where
    (l_arg_vars, r_arg_vars) 		 = splitInHalf arg_vars
    (l_i, l_alt_rhs, l_to_body_fn, l_rep_var) = mk_prod_stuff (i+1) l_arg_vars
    (r_i, r_alt_rhs, r_to_body_fn, r_rep_var) = mk_prod_stuff l_i   r_arg_vars
    rep_var = mkTemplateLocal i (mkTyConApp crossTyCon rep_tys)
    rep_tys = [idType l_rep_var, idType r_rep_var]
\end{code}

A little utility function

\begin{code}
splitInHalf :: [a] -> ([a],[a])
splitInHalf list = (left, right)
		 where
		   half  = length list `div` 2
		   left  = take half list
		   right = drop half list
\end{code}

%************************************************************************
%*									*
\subsection{Generating the RHS of a generic default method}
%*									*
%************************************************************************

Generating the Generic default method.  Uses the bimaps to generate the
actual method. All of this is rather incomplete, but it would be nice
to make even this work.  Example

 	class Foo a where
	  op :: Op a

	instance Foo T

Then we fill in the RHS for op, RenamedHsExpr, by calling mkGenericRhs:

	instance Foo T where
	   op = <mkGenericRhs op a T>

To do this, we generate a pair of RenamedHsExprs (EP toOp fromOp), where

	toOp   :: Op Trep -> Op T
	fromOp :: Op T    -> Op Trep

(the bimap) and then fill in the RHS with

	instance Foo T where
	   op = toOp op

Remember, we're generating a RenamedHsExpr, so the result of all this
will be fed to the type checker.  So the 'op' on the RHS will be 
at the representation type for T, Trep.


A note about polymorphism.  Suppose the class op is polymorphic:

	class Baz a where
	  op :: forall b. Ord b => a -> b -> b

Then we can still generate a bimap with

	toOP :: forall b. (Trep -> b -> b) -> (T -> b -> b)

and fill in the instance decl thus

	instance Foo T where
	   op = toOp op

By the time the type checker has done its stuff we'll get

	instance Foo T where
	   op = \b. \dict::Ord b. toOp b (op Trep b dict)

\begin{code}
mkGenericRhs :: Id -> TyVar -> TyCon -> RenamedHsExpr
mkGenericRhs sel_id tyvar tycon
  = HsApp (toEP bimap) (HsVar (idName sel_id))
  where 
	-- Initialising the "Environment" with the from/to functions
	-- on the datatype (actually tycon) in question
	Just (EP from to) = tyConGenInfo tycon	-- Caller checked this will succeed
        ep        	  = EP (HsVar (idName from)) (HsVar (idName to)) 

        -- Takes out the ForAll and the Class restrictions 
        -- in front of the type of the method.
	(_,_,op_ty) = tcSplitSigmaTy (idType sel_id)

        -- Do it again!  This deals with the case where the method type 
	-- is polymorphic -- see notes above
	(local_tvs,_,final_ty) = tcSplitSigmaTy op_ty

	-- Now we probably have a tycon in front
        -- of us, quite probably a FunTyCon.
        bimap = generate_bimap (tyvar, ep, local_tvs) final_ty

type EPEnv = (TyVar,		-- The class type variable
	      EP RenamedHsExpr,	-- The EP it maps to
	      [TyVar]		-- Other in-scope tyvars; they have an identity EP
	     )

-------------------
generate_bimap :: EPEnv
	       -> Type
	       -> EP RenamedHsExpr
-- Top level case - splitting the TyCon.
generate_bimap env@(tv,ep,local_tvs) ty 
  = case getTyVar_maybe ty of
	Just tv1 |  tv == tv1 -> ep				-- The class tyvar
		 |  otherwise -> ASSERT( tv1 `elem` local_tvs)	-- One of the polymorphic tyvars of the method
				 idEP	
	Nothing	 -> bimapApp env (tcSplitTyConApp_maybe ty)

-------------------
bimapApp :: EPEnv -> Maybe (TyCon, [Type]) -> EP RenamedHsExpr
bimapApp env Nothing		    = panic "TcClassDecl: Type Application!"
bimapApp env (Just (tycon, ty_args)) 
  | tycon == funTyCon       = bimapArrow arg_eps
  | isBoxedTupleTyCon tycon = bimapTuple arg_eps
  | otherwise		    =	-- Otherwise validGenericMethodType will 
				-- have checked that the type is a constant type
			      ASSERT( all (`elem` local_tvs) (varSetElems (tyVarsOfTypes ty_args)) )
			      idEP
    where
      arg_eps = map (generate_bimap env) ty_args
      (_,_,local_tvs) = env

-------------------
-- bimapArrow :: [EP a a', EP b b'] -> EP (a->b) (a'->b')
bimapArrow [ep1, ep2]
  = EP { fromEP = mk_hs_lam [VarPatIn g1, VarPatIn g2] from_body, 
	 toEP   = mk_hs_lam [VarPatIn g1, VarPatIn g2] to_body }
  where
    from_body = fromEP ep2 `HsApp` (HsPar $ HsVar g1 `HsApp` (HsPar $ toEP   ep1 `HsApp` HsVar g2))
    to_body   = toEP   ep2 `HsApp` (HsPar $ HsVar g1 `HsApp` (HsPar $ fromEP ep1 `HsApp` HsVar g2))

-------------------
bimapTuple eps 
  = EP { fromEP = mk_hs_lam [tuple_pat] from_body,
	 toEP   = mk_hs_lam [tuple_pat] to_body }
  where
    names	= takeList eps genericNames
    tuple_pat	= TuplePatIn (map VarPatIn names) Boxed
    eps_w_names = eps `zip` names
    to_body     = ExplicitTuple [toEP   ep `HsApp` HsVar g | (ep,g) <- eps_w_names] Boxed
    from_body   = ExplicitTuple [fromEP ep `HsApp` HsVar g | (ep,g) <- eps_w_names] Boxed

-------------------
genericNames :: [Name]
genericNames = [mkSysLocalName (mkBuiltinUnique i) (_PK_ ('g' : show i)) | i <- [1..]]
(g1:g2:g3:_) = genericNames

mk_hs_lam pats body = HsPar (HsLam (mkSimpleMatch pats body placeHolderType builtinSrcLoc))

idEP :: EP RenamedHsExpr
idEP = EP idexpr idexpr
     where
       idexpr = mk_hs_lam [VarPatIn g3] (HsVar g3)
\end{code}
