\begin{code}
module Generics ( canDoGenerics, mkTyConGenericBinds,
		  mkGenericRhs, 
		  validGenericInstanceType, validGenericMethodType
    ) where


import HsSyn
import Type             ( Type, isUnLiftedType, tyVarsOfType, tyVarsOfTypes,
			  isTyVarTy, getTyVar_maybe, funTyCon
			)
import TcHsSyn		( mkSimpleHsAlt )
import TcType		( tcSplitTyConApp_maybe, tcSplitSigmaTy, tcSplitPhiTy, applyTy, 
			  isTauTy, mkTyVarTy )
import DataCon          ( DataCon, dataConOrigArgTys, isVanillaDataCon,
			  dataConSourceArity )

import TyCon            ( TyCon, tyConName, tyConDataCons, 
			  isBoxedTupleTyCon
			)
import Name		( nameModule, nameOccName, getSrcLoc )
import OccName		( mkGenOcc1, mkGenOcc2 )
import RdrName		( RdrName, getRdrName, mkVarUnqual, mkOrig )
import BasicTypes       ( EP(..), Boxity(..) )
import Var              ( TyVar )
import VarSet		( varSetElems )
import Id               ( Id, idType )
import TysWiredIn	( listTyCon )
import PrelNames
	
import SrcLoc		( srcLocSpan, noLoc, Located(..) )
import Util             ( takeList, isSingleton )
import Bag
import Outputable 
import FastString

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
bug #5. [I don't think that this is the case anymore after SPJ's latest
changes in that regard.  Delete this comment?  -=chak/7Jun2]

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
	Just (tycon, tys) ->  all isTyVarTy tys && tyConName tycon `elem` genericTyConNames
	Nothing		  ->  False

validGenericMethodType :: Type -> Bool
  -- At the moment we only allow method types built from
  -- 	* type variables
  --	* function arrow
  --	* boxed tuples
  --    * lists
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

    valid_tycon tc = tc == funTyCon || tc == listTyCon || isBoxedTupleTyCon tc 
	-- Compare bimapApp, below
\end{code}


%************************************************************************
%*									*
\subsection{Generating representation types}
%*									*
%************************************************************************

\begin{code}
canDoGenerics :: [DataCon] -> Bool
-- Called on source-code data types, to see if we should generate
-- generic functions for them.  (This info is recorded in the interface file for
-- imported data types.)

canDoGenerics data_cons
  =  not (any bad_con data_cons) 	-- See comment below
  && not (null data_cons)		-- No values of the type
  where
    bad_con dc = any bad_arg_type (dataConOrigArgTys dc) || not (isVanillaDataCon dc)
  	-- If any of the constructor has an unboxed type as argument,
	-- then we can't build the embedding-projection pair, because
	-- it relies on instantiating *polymorphic* sum and product types
	-- at the argument types of the constructors

	-- Nor can we do the job if it's an existential data constructor,

	-- Nor if the args are polymorphic types (I don't think)
    bad_arg_type ty = isUnLiftedType ty || not (isTauTy ty)
\end{code}

%************************************************************************
%*									*
\subsection{Generating the RHS of a generic default method}
%*									*
%************************************************************************

\begin{code}
type US = Int	-- Local unique supply, just a plain Int
type FromAlt = (LPat RdrName, LHsExpr RdrName)

mkTyConGenericBinds :: TyCon -> LHsBinds RdrName
mkTyConGenericBinds tycon
  = unitBag (L loc (mkFunBind (L loc from_RDR) from_matches))
	`unionBags`
    unitBag (L loc (mkFunBind (L loc to_RDR) to_matches))
  where
    from_matches = [mkSimpleHsAlt pat rhs | (pat,rhs) <- from_alts]
    to_matches   = [mkSimpleHsAlt to_pat to_body]
    loc	     = srcLocSpan (getSrcLoc tycon)
    datacons = tyConDataCons tycon
    (from_RDR, to_RDR) = mkGenericNames tycon

    -- Recurse over the sum first
    from_alts :: [FromAlt]
    (from_alts, to_pat, to_body) = mk_sum_stuff init_us datacons
    init_us = 1::Int		-- Unique supply

----------------------------------------------------
--	Dealing with sums
----------------------------------------------------

mk_sum_stuff :: US 			-- Base for generating unique names
	     -> [DataCon]	 	-- The data constructors
	     -> ([FromAlt],				-- Alternatives for the T->Trep "from" function
		 InPat RdrName, LHsExpr RdrName)	-- Arg and body of the Trep->T "to" function

-- For example, given
--	data T = C | D Int Int Int
-- 
-- mk_sum_stuff v [C,D] = ([C -> Inl Unit, D a b c -> Inr (a :*: (b :*: c))],
--			   case cd of { Inl u -> C; 
--  					Inr abc -> case abc of { a :*: bc ->
--						   case bc  of { b :*: c ->
--						   D a b c }} },
--			   cd)

mk_sum_stuff us [datacon]
   = ([from_alt], to_pat, to_body_fn app_exp)
   where
     n_args = dataConSourceArity datacon	-- Existentials already excluded

     datacon_vars = map mkGenericLocal [us .. us+n_args-1]
     us'          = us + n_args

     datacon_rdr  = getRdrName datacon
     app_exp      = nlHsVarApps datacon_rdr datacon_vars
     from_alt     = (nlConVarPat datacon_rdr datacon_vars, from_alt_rhs)

     (_, from_alt_rhs, to_pat, to_body_fn) = mk_prod_stuff us' datacon_vars

mk_sum_stuff us datacons
  = (wrap inlDataCon_RDR l_from_alts ++ wrap inrDataCon_RDR r_from_alts,
     nlVarPat to_arg,
     noLoc (HsCase (nlHsVar to_arg) 
	    (mkMatchGroup [mkSimpleHsAlt (nlConPat inlDataCon_RDR [l_to_pat]) l_to_body,
			   mkSimpleHsAlt (nlConPat inrDataCon_RDR [r_to_pat]) r_to_body])))
  where
    (l_datacons, r_datacons)		= splitInHalf datacons
    (l_from_alts, l_to_pat, l_to_body)	= mk_sum_stuff us' l_datacons
    (r_from_alts, r_to_pat, r_to_body)	= mk_sum_stuff us' r_datacons

    to_arg = mkGenericLocal us
    us'	   = us+1

    wrap :: RdrName -> [FromAlt] -> [FromAlt]
	-- Wrap an application of the Inl or Inr constructor round each alternative
    wrap dc alts = [(pat, noLoc (HsApp (nlHsVar dc) rhs)) | (pat,rhs) <- alts]


----------------------------------------------------
--	Dealing with products
----------------------------------------------------
mk_prod_stuff :: US			-- Base for unique names
	      -> [RdrName]		-- arg-ids; args of the original user-defined constructor
					-- 	They are bound enclosing from_rhs
					-- 	Please bind these in the to_body_fn 
	      -> (US,			-- Depleted unique-name supply
		  LHsExpr RdrName, 			-- from-rhs: puts together the representation from the arg_ids
		  InPat RdrName,			-- to_pat: 
		  LHsExpr RdrName -> LHsExpr RdrName)	-- to_body_fn: takes apart the representation

-- For example:
-- mk_prod_stuff abc [a,b,c] = ( a :*: (b :*: c),
--				 abc,
--			         \<body-code> -> case abc of { a :*: bc ->
--				 	         case bc  of { b :*: c  -> 
--					         <body-code> )

-- We need to use different uniques in the branches 
-- because the returned to_body_fns are nested.  
-- Hence the returned unqique-name supply

mk_prod_stuff us []		-- Unit case
  = (us+1,
     nlHsVar genUnitDataCon_RDR,
     noLoc (SigPatIn (nlVarPat (mkGenericLocal us)) 
	    	     (noLoc (HsTyVar (getRdrName genUnitTyConName)))),
	-- Give a signature to the pattern so we get 
	--	data S a = Nil | S a
	--	toS = \x -> case x of { Inl (g :: Unit) -> Nil
	--				Inr x -> S x }
	-- The (:: Unit) signature ensures that we'll infer the right
	-- type for toS. If we leave it out, the type is too polymorphic

     \x -> x)

mk_prod_stuff us [arg_var]	-- Singleton case
  = (us, nlHsVar arg_var, nlVarPat arg_var, \x -> x)

mk_prod_stuff us arg_vars	-- Two or more
  = (us'', 
     nlHsApps crossDataCon_RDR [l_alt_rhs, r_alt_rhs],
     nlVarPat to_arg, 
-- gaw 2004 FIX?
     \x -> noLoc (HsCase (nlHsVar to_arg) 
		  (mkMatchGroup [mkSimpleHsAlt pat (l_to_body_fn (r_to_body_fn x))])))
  where
    to_arg = mkGenericLocal us
    (l_arg_vars, r_arg_vars) 		      = splitInHalf arg_vars
    (us',  l_alt_rhs, l_to_pat, l_to_body_fn) = mk_prod_stuff (us+1)  l_arg_vars
    (us'', r_alt_rhs, r_to_pat, r_to_body_fn) = mk_prod_stuff us' r_arg_vars
    pat = nlConPat crossDataCon_RDR [l_to_pat, r_to_pat]

splitInHalf :: [a] -> ([a],[a])
splitInHalf list = (left, right)
		 where
		   half  = length list `div` 2
		   left  = take half list
		   right = drop half list

mkGenericLocal :: US -> RdrName
mkGenericLocal u = mkVarUnqual (mkFastString ("g" ++ show u))

mkGenericNames tycon
  = (from_RDR, to_RDR)
  where
    tc_name  = tyConName tycon
    tc_occ   = nameOccName tc_name
    tc_mod   = nameModule tc_name
    from_RDR = mkOrig tc_mod (mkGenOcc1 tc_occ)
    to_RDR   = mkOrig tc_mod (mkGenOcc2 tc_occ)
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


Note [Polymorphic methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose the class op is polymorphic:

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
mkGenericRhs :: Id -> TyVar -> TyCon -> LHsExpr RdrName
mkGenericRhs sel_id tyvar tycon
  = ASSERT( isSingleton ctxt ) 	-- Checks shape of selector-id context
--    pprTrace "mkGenericRhs" (vcat [ppr sel_id, ppr (idType sel_id), ppr tyvar, ppr tycon, ppr local_tvs, ppr final_ty]) $
    mkHsApp (toEP bimap) (nlHsVar (getRdrName sel_id))
  where 
	-- Initialising the "Environment" with the from/to functions
	-- on the datatype (actually tycon) in question
	(from_RDR, to_RDR) = mkGenericNames tycon 

        -- Instantiate the selector type, and strip off its class context
	(ctxt, op_ty) = tcSplitPhiTy (applyTy (idType sel_id) (mkTyVarTy tyvar))

        -- Do it again!  This deals with the case where the method type 
	-- is polymorphic -- see Note [Polymorphic methods] above
	(local_tvs,_,final_ty) = tcSplitSigmaTy op_ty

	-- Now we probably have a tycon in front
        -- of us, quite probably a FunTyCon.
        ep    = EP (nlHsVar from_RDR) (nlHsVar to_RDR) 
        bimap = generate_bimap (tyvar, ep, local_tvs) final_ty

type EPEnv = (TyVar,			-- The class type variable
	      EP (LHsExpr RdrName),	-- The EP it maps to
	      [TyVar]			-- Other in-scope tyvars; they have an identity EP
	     )

-------------------
generate_bimap :: EPEnv
	       -> Type
	       -> EP (LHsExpr RdrName)
-- Top level case - splitting the TyCon.
generate_bimap env@(tv,ep,local_tvs) ty 
  = case getTyVar_maybe ty of
	Just tv1 |  tv == tv1 -> ep				-- The class tyvar
		 |  otherwise -> ASSERT( tv1 `elem` local_tvs)	-- One of the polymorphic tyvars of the method
				 idEP	
	Nothing	 -> bimapApp env (tcSplitTyConApp_maybe ty)

-------------------
bimapApp :: EPEnv -> Maybe (TyCon, [Type]) -> EP (LHsExpr RdrName)
bimapApp env Nothing		    = panic "TcClassDecl: Type Application!"
bimapApp env (Just (tycon, ty_args)) 
  | tycon == funTyCon       = bimapArrow arg_eps
  | tycon == listTyCon      = bimapList arg_eps
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
  = EP { fromEP = mkHsLam [nlVarPat a_RDR, nlVarPat b_RDR] from_body, 
	 toEP   = mkHsLam [nlVarPat a_RDR, nlVarPat b_RDR] to_body }
  where
    from_body = fromEP ep2 `mkHsApp` (mkHsPar $ nlHsVar a_RDR `mkHsApp` (mkHsPar $ toEP   ep1 `mkHsApp` nlHsVar b_RDR))
    to_body   = toEP   ep2 `mkHsApp` (mkHsPar $ nlHsVar a_RDR `mkHsApp` (mkHsPar $ fromEP ep1 `mkHsApp` nlHsVar b_RDR))

-------------------
-- bimapTuple :: [EP a1 b1, ... EP an bn] -> EP (a1,...an) (b1,..bn)
bimapTuple eps 
  = EP { fromEP = mkHsLam [noLoc tuple_pat] (noLoc from_body),
	 toEP   = mkHsLam [noLoc tuple_pat] (noLoc to_body) }
  where
    names	= takeList eps gs_RDR
    tuple_pat	= TuplePat (map nlVarPat names) Boxed
    eps_w_names = eps `zip` names
    to_body     = ExplicitTuple [toEP   ep `mkHsApp` nlHsVar g | (ep,g) <- eps_w_names] Boxed
    from_body   = ExplicitTuple [fromEP ep `mkHsApp` nlHsVar g | (ep,g) <- eps_w_names] Boxed

-------------------
-- bimapList :: EP a b -> EP [a] [b]
bimapList [ep]
  = EP { fromEP = nlHsApp (nlHsVar map_RDR) (fromEP ep),
	 toEP   = nlHsApp (nlHsVar map_RDR) (toEP ep) }

-------------------
a_RDR	= mkVarUnqual FSLIT("a")
b_RDR	= mkVarUnqual FSLIT("b")
gs_RDR	= [ mkVarUnqual (mkFastString ("g"++show i)) | i <- [(1::Int) .. ] ]

idEP :: EP (LHsExpr RdrName)
idEP = EP idexpr idexpr
     where
       idexpr = mkHsLam [nlVarPat a_RDR] (nlHsVar a_RDR)
\end{code}
