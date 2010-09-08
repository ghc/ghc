%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn
import DynFlags

import Generics
import TcRnMonad
import TcEnv
import TcClassDcl( tcAddDeclCtxt )	-- Small helper
import TcGenDeriv			-- Deriv stuff
import InstEnv
import Inst
import TcHsType
import TcMType
import TcSimplify

import RnBinds
import RnEnv
import HscTypes

import Class
import Type
import Coercion
import ErrUtils
import MkId
import DataCon
import Maybes
import RdrName
import Name
import NameSet
import TyCon
import TcType
import Var
import VarSet
import PrelNames
import SrcLoc
import Util
import ListSetOps
import Outputable
import FastString
import Bag

import Control.Monad
\end{code}

%************************************************************************
%*									*
		Overview
%*									*
%************************************************************************

Overall plan
~~~~~~~~~~~~
1.  Convert the decls (i.e. data/newtype deriving clauses, 
    plus standalone deriving) to [EarlyDerivSpec]

2.  Infer the missing contexts for the Left DerivSpecs

3.  Add the derived bindings, generating InstInfos

\begin{code}
-- DerivSpec is purely  local to this module
data DerivSpec  = DS { ds_loc     :: SrcSpan 
		     , ds_orig    :: InstOrigin 
		     , ds_name    :: Name
		     , ds_tvs     :: [TyVar] 
		     , ds_theta   :: ThetaType
		     , ds_cls     :: Class
		     , ds_tys     :: [Type]
		     , ds_tc      :: TyCon
		     , ds_tc_args :: [Type]
		     , ds_newtype :: Bool }
	-- This spec implies a dfun declaration of the form
	--	 df :: forall tvs. theta => C tys
	-- The Name is the name for the DFun we'll build
	-- The tyvars bind all the variables in the theta
	-- For family indexes, the tycon in 
	--	 in ds_tys is the *family* tycon
	--	 in ds_tc, ds_tc_args is the *representation* tycon
	-- For non-family tycons, both are the same

	-- ds_newtype = True  <=> Newtype deriving
	--		False <=> Vanilla deriving

type DerivContext = Maybe ThetaType
   -- Nothing 	 <=> Vanilla deriving; infer the context of the instance decl
   -- Just theta <=> Standalone deriving: context supplied by programmer

type EarlyDerivSpec = Either DerivSpec DerivSpec
	-- Left  ds => the context for the instance should be inferred
	--	       In this case ds_theta is the list of all the 
	--		  constraints needed, such as (Eq [a], Eq a)
	--		  The inference process is to reduce this to a 
	--		  simpler form (e.g. Eq a)
	-- 
	-- Right ds => the exact context for the instance is supplied 
	--	       by the programmer; it is ds_theta

pprDerivSpec :: DerivSpec -> SDoc
pprDerivSpec (DS { ds_loc = l, ds_name = n, ds_tvs = tvs, 
		   ds_cls = c, ds_tys = tys, ds_theta = rhs })
  = parens (hsep [ppr l, ppr n, ppr tvs, ppr c, ppr tys]
	    <+> equals <+> ppr rhs)
\end{code}


Inferring missing contexts 
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	data T a b = C1 (Foo a) (Bar b)
		   | C2 Int (T b a)
		   | C3 (T a a)
		   deriving (Eq)

[NOTE: See end of these comments for what to do with 
	data (C a, D b) => T a b = ...
]

We want to come up with an instance declaration of the form

	instance (Ping a, Pong b, ...) => Eq (T a b) where
		x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely @Ping@, @Pong@ and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

	Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the @data@ decl:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

Foo and Bar may have explicit instances for @Eq@, in which case we can
just substitute for them.  Alternatively, either or both may have
their @Eq@ instances given by @deriving@ clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

	Eq (T a b) = {}		-- The empty set

Next iteration:
	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b u {} u {} u {}
		   = Eq a u Ping b

Next iteration:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b
		   u (Eq b u Ping a)
		   u (Eq a u Ping a)

		   = Eq a u Ping b u Eq b u Ping a

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

	- the classes constrain only tyvars
	- the list is sorted by tyvar (major key) and then class (minor key)
	- no duplicates, of course

So, here are the synonyms for the ``equation'' structures:


Note [Data decl contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	data (RealFloat a) => Complex a = !a :+ !a deriving( Read )

We will need an instance decl like:

	instance (Read a, RealFloat a) => Read (Complex a) where
	  ...

The RealFloat in the context is because the read method for Complex is bound
to construct a Complex, and doing that requires that the argument type is
in RealFloat. 

But this ain't true for Show, Eq, Ord, etc, since they don't construct
a Complex; they only take them apart.

Our approach: identify the offending classes, and add the data type
context to the instance decl.  The "offending classes" are

	Read, Enum?

FURTHER NOTE ADDED March 2002.  In fact, Haskell98 now requires that
pattern matching against a constructor from a data type with a context
gives rise to the constraints for that context -- or at least the thinned
version.  So now all classes are "offending".

Note [Newtype deriving]
~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
    class C a b
    instance C [a] Char
    newtype T = T Char deriving( C [a] )

Notice the free 'a' in the deriving.  We have to fill this out to 
    newtype T = T Char deriving( forall a. C [a] )

And then translate it to:
    instance C [a] Char => C [a] T where ...
    
	
Note [Newtype deriving superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See also Trac #1220 for an interesting exchange on newtype
deriving and superclasses.)

The 'tys' here come from the partial application in the deriving
clause. The last arg is the new instance type.

We must pass the superclasses; the newtype might be an instance
of them in a different way than the representation type
E.g.		newtype Foo a = Foo a deriving( Show, Num, Eq )
Then the Show instance is not done via isomorphism; it shows
	Foo 3 as "Foo 3"
The Num instance is derived via isomorphism, but the Show superclass
dictionary must the Show instance for Foo, *not* the Show dictionary
gotten from the Num dictionary. So we must build a whole new dictionary
not just use the Num one.  The instance we want is something like:
     instance (Num a, Show (Foo a), Eq (Foo a)) => Num (Foo a) where
     	(+) = ((+)@a)
     	...etc...
There may be a coercion needed which we get from the tycon for the newtype
when the dict is constructed in TcInstDcls.tcInstDecl2


Note [Unused constructors and deriving clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #3221.  Consider
   data T = T1 | T2 deriving( Show )
Are T1 and T2 unused?  Well, no: the deriving clause expands to mention
both of them.  So we gather defs/uses from deriving just like anything else.

%************************************************************************
%*									*
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*									*
%************************************************************************

\begin{code}
tcDeriving  :: [LTyClDecl Name]  -- All type constructors
            -> [LInstDecl Name]  -- All instance declarations
            -> [LDerivDecl Name] -- All stand-alone deriving declarations
	    -> TcM ([InstInfo Name],	-- The generated "instance decls"
		    HsValBinds Name,	-- Extra generated top-level bindings
                    DefUses)

tcDeriving tycl_decls inst_decls deriv_decls
  = recoverM (return ([], emptyValBindsOut, emptyDUs)) $
    do	{   	-- Fish the "deriving"-related information out of the TcEnv
		-- And make the necessary "equations".
	  is_boot <- tcIsHsBoot
	; traceTc (text "tcDeriving" <+> ppr is_boot)
	; early_specs <- makeDerivSpecs is_boot tycl_decls inst_decls deriv_decls

	; overlap_flag <- getOverlapFlag
	; let (infer_specs, given_specs) = splitEithers early_specs
	; insts1 <- mapM (genInst True overlap_flag) given_specs

	; final_specs <- extendLocalInstEnv (map (iSpec . fst) insts1) $
			 inferInstanceContexts overlap_flag infer_specs

	; insts2 <- mapM (genInst False overlap_flag) final_specs

		 -- Generate the generic to/from functions from each type declaration
	; gen_binds <- mkGenericBinds is_boot tycl_decls
	; (inst_info, rn_binds, rn_dus) <- renameDeriv is_boot gen_binds (insts1 ++ insts2)

	; dflags <- getDOpts
	; liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances"
	         (ddump_deriving inst_info rn_binds))

	; return (inst_info, rn_binds, rn_dus) }
  where
    ddump_deriving :: [InstInfo Name] -> HsValBinds Name -> SDoc
    ddump_deriving inst_infos extra_binds
      = vcat (map pprInstInfoDetails inst_infos) $$ ppr extra_binds

renameDeriv :: Bool -> LHsBinds RdrName
	    -> [(InstInfo RdrName, DerivAuxBinds)]
 	    -> TcM ([InstInfo Name], HsValBinds Name, DefUses)
renameDeriv is_boot gen_binds insts
  | is_boot	-- If we are compiling a hs-boot file, don't generate any derived bindings
		-- The inst-info bindings will all be empty, but it's easier to
		-- just use rn_inst_info to change the type appropriately
  = do	{ (rn_inst_infos, fvs) <- mapAndUnzipM rn_inst_info inst_infos	
	; return (rn_inst_infos, emptyValBindsOut, usesOnly (plusFVs fvs)) }

  | otherwise
  = discardWarnings $ 	 -- Discard warnings about unused bindings etc
    do	{ (rn_gen, dus_gen) <- setOptM Opt_ScopedTypeVariables $  -- Type signatures in patterns 
								  -- are used in the generic binds
			       rnTopBinds (ValBindsIn gen_binds [])
	; keepAliveSetTc (duDefs dus_gen)	-- Mark these guys to be kept alive

		-- Generate and rename any extra not-one-inst-decl-specific binds, 
		-- notably "con2tag" and/or "tag2con" functions.  
		-- Bring those names into scope before renaming the instances themselves
	; loc <- getSrcSpanM	-- Generic loc for shared bindings
	; let (aux_binds, aux_sigs) = unzip $ map (genAuxBind loc) $ 
	                              rm_dups [] $ concat deriv_aux_binds
              aux_val_binds = ValBindsIn (listToBag aux_binds) aux_sigs
	; rn_aux_lhs <- rnTopBindsLHS emptyFsEnv aux_val_binds
	; let aux_names = collectHsValBinders rn_aux_lhs

	; bindLocalNames aux_names $ 
    do	{ (rn_aux, dus_aux) <- rnTopBindsRHS (mkNameSet aux_names) rn_aux_lhs
	; (rn_inst_infos, fvs_insts) <- mapAndUnzipM rn_inst_info inst_infos
	; return (rn_inst_infos, rn_aux `plusHsValBinds` rn_gen,
                  dus_gen `plusDU` dus_aux `plusDU` usesOnly (plusFVs fvs_insts)) } }

  where
    (inst_infos, deriv_aux_binds) = unzip insts
    
	-- Remove duplicate requests for auxilliary bindings
    rm_dups acc [] = acc
    rm_dups acc (b:bs) | any (isDupAux b) acc = rm_dups acc bs
    		       | otherwise	      = rm_dups (b:acc) bs


    rn_inst_info :: InstInfo RdrName -> TcM (InstInfo Name, FreeVars)
    rn_inst_info info@(InstInfo { iBinds = NewTypeDerived coi tc })
	= return ( info { iBinds = NewTypeDerived coi tc }
                 , mkFVs (map dataConName (tyConDataCons tc)))
	  -- See Note [Newtype deriving and unused constructors]

    rn_inst_info (InstInfo { iSpec = inst, iBinds = VanillaInst binds sigs standalone_deriv })
	= 	-- Bring the right type variables into 
		-- scope (yuk), and rename the method binds
	   ASSERT( null sigs )
	   bindLocalNames (map Var.varName tyvars) $
 	   do { (rn_binds, fvs) <- rnMethodBinds clas_nm (\_ -> []) [] binds
	      ; let binds' = VanillaInst rn_binds [] standalone_deriv
	      ; return (InstInfo { iSpec = inst, iBinds = binds' }, fvs) }
	where
	  (tyvars,_, clas,_) = instanceHead inst
	  clas_nm            = className clas

-----------------------------------------
mkGenericBinds :: Bool -> [LTyClDecl Name] -> TcM (LHsBinds RdrName)
mkGenericBinds is_boot tycl_decls
  | is_boot 
  = return emptyBag
  | otherwise
  = do	{ tcs <- mapM tcLookupTyCon [ tcdName d 
    	      	      		    | L _ d <- tycl_decls, isDataDecl d ]
	; return (unionManyBags [ mkTyConGenericBinds tc
				| tc <- tcs, tyConHasGenerics tc ]) }
		-- We are only interested in the data type declarations,
		-- and then only in the ones whose 'has-generics' flag is on
		-- The predicate tyConHasGenerics finds both of these
\end{code}

Note [Newtype deriving and unused constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (see Trac #1954):

  module Bug(P) where
  newtype P a = MkP (IO a) deriving Monad

If you compile with -fwarn-unused-binds you do not expect the warning
"Defined but not used: data consructor MkP". Yet the newtype deriving
code does not explicitly mention MkP, but it should behave as if you
had written
  instance Monad P where
     return x = MkP (return x)
     ...etc...

So we want to signal a user of the data constructor 'MkP'.  That's
what we do in rn_inst_info, and it's the only reason we have the TyCon
stored in NewTypeDerived.


%************************************************************************
%*									*
		From HsSyn to DerivSpec
%*									*
%************************************************************************

@makeDerivSpecs@ fishes around to find the info about needed derived instances.

\begin{code}
makeDerivSpecs :: Bool 
	       -> [LTyClDecl Name] 
               -> [LInstDecl Name]
	       -> [LDerivDecl Name] 
	       -> TcM [EarlyDerivSpec]

makeDerivSpecs is_boot tycl_decls inst_decls deriv_decls
  | is_boot 	-- No 'deriving' at all in hs-boot files
  = do	{ mapM_ add_deriv_err deriv_locs 
	; return [] }
  | otherwise
  = do	{ eqns1 <- mapAndRecoverM deriveTyData all_tydata
	; eqns2 <- mapAndRecoverM deriveStandalone deriv_decls
	; return (eqns1 ++ eqns2) }
  where
    extractTyDataPreds decls
      = [(p, d) | d@(L _ (TyData {tcdDerivs = Just preds})) <- decls, p <- preds]

    all_tydata :: [(LHsType Name, LTyClDecl Name)]
	-- Derived predicate paired with its data type declaration
    all_tydata = extractTyDataPreds (instDeclATs inst_decls ++ tycl_decls)

    deriv_locs = map (getLoc . snd) all_tydata
		 ++ map getLoc deriv_decls

    add_deriv_err loc = setSrcSpan loc $
			addErr (hang (ptext (sLit "Deriving not permitted in hs-boot file"))
				   2 (ptext (sLit "Use an instance declaration instead")))

------------------------------------------------------------------
deriveStandalone :: LDerivDecl Name -> TcM EarlyDerivSpec
-- Standalone deriving declarations
--  e.g.   deriving instance Show a => Show (T a)
-- Rather like tcLocalInstDecl
deriveStandalone (L loc (DerivDecl deriv_ty))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do { traceTc (text "standalone deriving decl for" <+> ppr deriv_ty)
       ; (tvs, theta, tau) <- tcHsInstHead deriv_ty
       ; traceTc (text "standalone deriving;"
              <+> text "tvs:" <+> ppr tvs
              <+> text "theta:" <+> ppr theta
              <+> text "tau:" <+> ppr tau)
       ; (cls, inst_tys) <- checkValidInstance deriv_ty tvs theta tau
		-- C.f. TcInstDcls.tcLocalInstDecl1

       ; let cls_tys = take (length inst_tys - 1) inst_tys
             inst_ty = last inst_tys
       ; traceTc (text "standalone deriving;"
              <+> text "class:" <+> ppr cls
              <+> text "class types:" <+> ppr cls_tys
              <+> text "type:" <+> ppr inst_ty)
       ; mkEqnHelp StandAloneDerivOrigin tvs cls cls_tys inst_ty
                   (Just theta) }

------------------------------------------------------------------
deriveTyData :: (LHsType Name, LTyClDecl Name) -> TcM EarlyDerivSpec
deriveTyData (L loc deriv_pred, L _ decl@(TyData { tcdLName = L _ tycon_name, 
					           tcdTyVars = tv_names, 
				    	           tcdTyPats = ty_pats }))
  = setSrcSpan loc     $	-- Use the location of the 'deriving' item
    tcAddDeclCtxt decl $
    do	{ (tvs, tc, tc_args) <- get_lhs ty_pats
	; tcExtendTyVarEnv tvs $	-- Deriving preds may (now) mention
					-- the type variables for the type constructor

    do	{ (deriv_tvs, cls, cls_tys) <- tcHsDeriv deriv_pred
		-- The "deriv_pred" is a LHsType to take account of the fact that for
		-- newtype deriving we allow deriving (forall a. C [a]).

	-- Given data T a b c = ... deriving( C d ),
	-- we want to drop type variables from T so that (C d (T a)) is well-kinded
	; let cls_tyvars = classTyVars cls
	      kind = tyVarKind (last cls_tyvars)
	      (arg_kinds, _) = splitKindFunTys kind
	      n_args_to_drop = length arg_kinds	
	      n_args_to_keep = tyConArity tc - n_args_to_drop
	      args_to_drop   = drop n_args_to_keep tc_args
	      inst_ty        = mkTyConApp tc (take n_args_to_keep tc_args)
	      inst_ty_kind   = typeKind inst_ty
	      dropped_tvs    = mkVarSet (mapCatMaybes getTyVar_maybe args_to_drop)
	      univ_tvs       = (mkVarSet tvs `extendVarSetList` deriv_tvs)
					`minusVarSet` dropped_tvs
 
	-- Check that the result really is well-kinded
	; checkTc (n_args_to_keep >= 0 && (inst_ty_kind `eqKind` kind))
		  (derivingKindErr tc cls cls_tys kind)

	; checkTc (sizeVarSet dropped_tvs == n_args_to_drop && 		 -- (a)
	           tyVarsOfTypes (inst_ty:cls_tys) `subVarSet` univ_tvs) -- (b)
		  (derivingEtaErr cls cls_tys inst_ty)
		-- Check that 
		--  (a) The data type can be eta-reduced; eg reject:
		--		data instance T a a = ... deriving( Monad )
		--  (b) The type class args do not mention any of the dropped type
		--      variables 
		--		newtype T a s = ... deriving( ST s )

	-- Type families can't be partially applied
	-- e.g.   newtype instance T Int a = MkT [a] deriving( Monad )
	-- Note [Deriving, type families, and partial applications]
	; checkTc (not (isOpenTyCon tc) || n_args_to_drop == 0)
		  (typeFamilyPapErr tc cls cls_tys inst_ty)

	; mkEqnHelp DerivOrigin (varSetElems univ_tvs) cls cls_tys inst_ty Nothing } }
  where
	-- Tiresomely we must figure out the "lhs", which is awkward for type families
	-- E.g.   data T a b = .. deriving( Eq )
	-- 	    Here, the lhs is (T a b)
	--	  data instance TF Int b = ... deriving( Eq )
	--	    Here, the lhs is (TF Int b)
	-- But if we just look up the tycon_name, we get is the *family*
	-- tycon, but not pattern types -- they are in the *rep* tycon.
    get_lhs Nothing     = do { tc <- tcLookupTyCon tycon_name
			     ; let tvs = tyConTyVars tc
			     ; return (tvs, tc, mkTyVarTys tvs) }
    get_lhs (Just pats) = do { let hs_app = nlHsTyConApp tycon_name pats
			     ; (tvs, tc_app) <- tcHsQuantifiedType tv_names hs_app
			     ; let (tc, tc_args) = tcSplitTyConApp tc_app
			     ; return (tvs, tc, tc_args) }

deriveTyData _other
  = panic "derivTyData"	-- Caller ensures that only TyData can happen
\end{code}

Note [Deriving, type families, and partial applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When there are no type families, it's quite easy:

    newtype S a = MkS [a]
    -- :CoS :: S  ~ []	-- Eta-reduced

    instance Eq [a] => Eq (S a) 	-- by coercion sym (Eq (:CoS a)) : Eq [a] ~ Eq (S a)
    instance Monad [] => Monad S	-- by coercion sym (Monad :CoS)  : Monad [] ~ Monad S 

When type familes are involved it's trickier:

    data family T a b
    newtype instance T Int a = MkT [a] deriving( Eq, Monad )
    -- :RT is the representation type for (T Int a)
    --  :CoF:R1T a :: T Int a ~ :RT a	-- Not eta reduced
    --  :Co:R1T    :: :RT ~ []		-- Eta-reduced

    instance Eq [a] => Eq (T Int a) 	-- easy by coercion
    instance Monad [] => Monad (T Int)	-- only if we can eta reduce???

The "???" bit is that we don't build the :CoF thing in eta-reduced form
Henc the current typeFamilyPapErr, even though the instance makes sense.
After all, we can write it out
    instance Monad [] => Monad (T Int)	-- only if we can eta reduce???
      return x = MkT [x]
      ... etc ...	

\begin{code}
mkEqnHelp :: InstOrigin -> [TyVar] -> Class -> [Type] -> Type
          -> DerivContext	-- Just    => context supplied (standalone deriving)
				-- Nothing => context inferred (deriving on data decl)
          -> TcRn EarlyDerivSpec
-- Make the EarlyDerivSpec for an instance
--	forall tvs. theta => cls (tys ++ [ty])
-- where the 'theta' is optional (that's the Maybe part)
-- Assumes that this declaration is well-kinded

mkEqnHelp orig tvs cls cls_tys tc_app mtheta
  | Just (tycon, tc_args) <- tcSplitTyConApp_maybe tc_app
  , isAlgTyCon tycon	-- Check for functions, primitive types etc
  = do	{ (rep_tc, rep_tc_args) <- tcLookupFamInstExact tycon tc_args
	          -- Be careful to test rep_tc here: in the case of families, 
	          -- we want to check the instance tycon, not the family tycon

	-- For standalone deriving (mtheta /= Nothing), 
	-- check that all the data constructors are in scope.
	-- No need for this when deriving Typeable, becuase we don't need
	-- the constructors for that.
	; rdr_env <- getGlobalRdrEnv
	; let hidden_data_cons = isAbstractTyCon rep_tc || any not_in_scope (tyConDataCons rep_tc)
	      not_in_scope dc  = null (lookupGRE_Name rdr_env (dataConName dc))
	; checkTc (isNothing mtheta || 
	  	   not hidden_data_cons ||
		   className cls `elem` typeableClassNames) 
		  (derivingHiddenErr tycon)

	; dflags <- getDOpts
	; if isDataTyCon rep_tc then
		mkDataTypeEqn orig dflags tvs cls cls_tys
			      tycon tc_args rep_tc rep_tc_args mtheta
	  else
		mkNewTypeEqn orig dflags tvs cls cls_tys 
			     tycon tc_args rep_tc rep_tc_args mtheta }
  | otherwise
  = failWithTc (derivingThingErr False cls cls_tys tc_app
	       (ptext (sLit "The last argument of the instance must be a data or newtype application")))
\end{code}

Note [Looking up family instances for deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcLookupFamInstExact is an auxiliary lookup wrapper which requires
that looked-up family instances exist.  If called with a vanilla
tycon, the old type application is simply returned.

If we have
  data instance F () = ... deriving Eq
  data instance F () = ... deriving Eq
then tcLookupFamInstExact will be confused by the two matches;
but that can't happen because tcInstDecls1 doesn't call tcDeriving
if there are any overlaps.

There are two other things that might go wrong with the lookup.
First, we might see a standalone deriving clause
	deriving Eq (F ())
when there is no data instance F () in scope. 

Note that it's OK to have
  data instance F [a] = ...
  deriving Eq (F [(a,b)])
where the match is not exact; the same holds for ordinary data types
with standalone deriving declrations.

\begin{code}
tcLookupFamInstExact :: TyCon -> [Type] -> TcM (TyCon, [Type])
tcLookupFamInstExact tycon tys
  | not (isOpenTyCon tycon)
  = return (tycon, tys)
  | otherwise
  = do { maybeFamInst <- tcLookupFamInst tycon tys
       ; case maybeFamInst of
           Nothing      -> famInstNotFound tycon tys
           Just famInst -> return famInst
       }

famInstNotFound :: TyCon -> [Type] -> TcM a
famInstNotFound tycon tys 
  = failWithTc (ptext (sLit "No family instance for")
			<+> quotes (pprTypeApp tycon tys))
\end{code}


%************************************************************************
%*									*
		Deriving data types
%*									*
%************************************************************************

\begin{code}
mkDataTypeEqn :: InstOrigin
              -> DynFlags
              -> [Var]                  -- Universally quantified type variables in the instance
              -> Class                  -- Class for which we need to derive an instance
              -> [Type]                 -- Other parameters to the class except the last
              -> TyCon                  -- Type constructor for which the instance is requested 
					--    (last parameter to the type class)
              -> [Type]                 -- Parameters to the type constructor
              -> TyCon                  -- rep of the above (for type families)
              -> [Type]                 -- rep of the above
              -> DerivContext        -- Context of the instance, for standalone deriving
              -> TcRn EarlyDerivSpec    -- Return 'Nothing' if error

mkDataTypeEqn orig dflags tvs cls cls_tys
              tycon tc_args rep_tc rep_tc_args mtheta
  = case checkSideConditions dflags mtheta cls cls_tys rep_tc of
	-- NB: pass the *representation* tycon to checkSideConditions
	CanDerive               -> go_for_it
	NonDerivableClass	-> bale_out (nonStdErr cls)
	DerivableClassError msg -> bale_out msg
  where
    go_for_it    = mk_data_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta
    bale_out msg = failWithTc (derivingThingErr False cls cls_tys (mkTyConApp tycon tc_args) msg)

mk_data_eqn, mk_typeable_eqn
   :: InstOrigin -> [TyVar] -> Class 
   -> TyCon -> [TcType] -> TyCon -> [TcType] -> DerivContext
   -> TcM EarlyDerivSpec
mk_data_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta
  | getName cls `elem` typeableClassNames
  = mk_typeable_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta

  | otherwise
  = do	{ dfun_name <- new_dfun_name cls tycon
  	; loc <- getSrcSpanM
	; let inst_tys = [mkTyConApp tycon tc_args]
	      inferred_constraints = inferConstraints tvs cls inst_tys rep_tc rep_tc_args
	      spec = DS { ds_loc = loc, ds_orig = orig
			, ds_name = dfun_name, ds_tvs = tvs 
			, ds_cls = cls, ds_tys = inst_tys
			, ds_tc = rep_tc, ds_tc_args = rep_tc_args
			, ds_theta =  mtheta `orElse` inferred_constraints
			, ds_newtype = False }

  	; return (if isJust mtheta then Right spec	-- Specified context
				   else Left spec) }	-- Infer context

mk_typeable_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta
	-- The Typeable class is special in several ways
	-- 	  data T a b = ... deriving( Typeable )
	-- gives
	--	  instance Typeable2 T where ...
	-- Notice that:
	-- 1. There are no constraints in the instance
	-- 2. There are no type variables either
	-- 3. The actual class we want to generate isn't necessarily
	--	Typeable; it depends on the arity of the type
  | isNothing mtheta	-- deriving on a data type decl
  = do	{ checkTc (cls `hasKey` typeableClassKey)
		  (ptext (sLit "Use deriving( Typeable ) on a data type declaration"))
	; real_cls <- tcLookupClass (typeableClassNames !! tyConArity tycon)
	; mk_typeable_eqn orig tvs real_cls tycon [] rep_tc [] (Just []) }

  | otherwise		-- standaone deriving
  = do	{ checkTc (null tc_args)
		  (ptext (sLit "Derived typeable instance must be of form (Typeable") 
			<> int (tyConArity tycon) <+> ppr tycon <> rparen)
	; dfun_name <- new_dfun_name cls tycon
  	; loc <- getSrcSpanM
	; return (Right $
		  DS { ds_loc = loc, ds_orig = orig, ds_name = dfun_name, ds_tvs = []
		     , ds_cls = cls, ds_tys = [mkTyConApp tycon []]
		     , ds_tc = rep_tc, ds_tc_args = rep_tc_args
		     , ds_theta = mtheta `orElse` [], ds_newtype = False })  }


inferConstraints :: [TyVar] -> Class -> [TcType] -> TyCon -> [TcType] -> ThetaType
-- Generate a sufficiently large set of constraints that typechecking the
-- generated method definitions should succeed.   This set will be simplified
-- before being used in the instance declaration
inferConstraints _ cls inst_tys rep_tc rep_tc_args
  = ASSERT2( equalLength rep_tc_tvs all_rep_tc_args, ppr cls <+> ppr rep_tc )
    pprTrace "ic" (ppr rep_tc $$ ppr rep_tc_tvs $$ ppr (tyConStupidTheta rep_tc) $$ ppr stupid_constraints) $
    stupid_constraints ++ extra_constraints
    ++ sc_constraints ++ con_arg_constraints
  where
       -- Constraints arising from the arguments of each constructor
    con_arg_constraints
      = [ mkClassPred cls [arg_ty] 
        | data_con <- tyConDataCons rep_tc,
          arg_ty   <- ASSERT( isVanillaDataCon data_con )
    			get_constrained_tys $
    		 	dataConInstOrigArgTys data_con all_rep_tc_args,
          not (isUnLiftedType arg_ty) ]
    		-- No constraints for unlifted types
    		-- Where they are legal we generate specilised function calls

    		-- For functor-like classes, two things are different
    		-- (a) We recurse over argument types to generate constraints
    		--     See Functor examples in TcGenDeriv
    		-- (b) The rep_tc_args will be one short
    is_functor_like = getUnique cls `elem` functorLikeClassKeys

    get_constrained_tys :: [Type] -> [Type]
    get_constrained_tys tys 
    	| is_functor_like = concatMap (deepSubtypesContaining last_tv) tys
    	| otherwise	  = tys

    rep_tc_tvs = tyConTyVars rep_tc
    last_tv = last rep_tc_tvs
    all_rep_tc_args | is_functor_like = rep_tc_args ++ [mkTyVarTy last_tv]
    		    | otherwise       = rep_tc_args

    	-- Constraints arising from superclasses
    	-- See Note [Superclasses of derived instance]
    sc_constraints = substTheta (zipOpenTvSubst (classTyVars cls) inst_tys)
    				(classSCTheta cls)

    	-- Stupid constraints
    stupid_constraints = substTheta subst (tyConStupidTheta rep_tc)
    subst = zipTopTvSubst rep_tc_tvs all_rep_tc_args
	      
	-- Extra Data constraints
	-- The Data class (only) requires that for 
	--    instance (...) => Data (T t1 t2) 
	-- IF   t1:*, t2:*
	-- THEN (Data t1, Data t2) are among the (...) constraints
	-- Reason: when the IF holds, we generate a method
	-- 	       dataCast2 f = gcast2 f
	--         and we need the Data constraints to typecheck the method
    extra_constraints 
      | cls `hasKey` dataClassKey
      , all (isLiftedTypeKind . typeKind) rep_tc_args 
      = [mkClassPred cls [ty] | ty <- rep_tc_args]
      | otherwise 
      = []

------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

data DerivStatus = CanDerive
		 | DerivableClassError SDoc	-- Standard class, but can't do it
     		 | NonDerivableClass		-- Non-standard class

checkSideConditions :: DynFlags -> DerivContext -> Class -> [TcType] -> TyCon -> DerivStatus
checkSideConditions dflags mtheta cls cls_tys rep_tc
  | Just cond <- sideConditions mtheta cls
  = case (cond (dflags, rep_tc)) of
	Just err -> DerivableClassError err	-- Class-specific error
	Nothing  | null cls_tys -> CanDerive	-- All derivable classes are unary, so
						-- cls_tys (the type args other than last) 
						-- should be null
		 | otherwise    -> DerivableClassError ty_args_why	-- e.g. deriving( Eq s )
  | otherwise = NonDerivableClass	-- Not a standard class
  where
    ty_args_why	= quotes (ppr (mkClassPred cls cls_tys)) <+> ptext (sLit "is not a class")

nonStdErr :: Class -> SDoc
nonStdErr cls = quotes (ppr cls) <+> ptext (sLit "is not a derivable class")

sideConditions :: DerivContext -> Class -> Maybe Condition
sideConditions mtheta cls
  | cls_key == eqClassKey      	   = Just cond_std
  | cls_key == ordClassKey     	   = Just cond_std
  | cls_key == showClassKey    	   = Just cond_std
  | cls_key == readClassKey    	   = Just (cond_std `andCond` cond_noUnliftedArgs)
  | cls_key == enumClassKey    	   = Just (cond_std `andCond` cond_isEnumeration)
  | cls_key == ixClassKey      	   = Just (cond_std `andCond` cond_enumOrProduct)
  | cls_key == boundedClassKey 	   = Just (cond_std `andCond` cond_enumOrProduct)
  | cls_key == dataClassKey    	   = Just (checkFlag Opt_DeriveDataTypeable `andCond` 
                                           cond_std `andCond` cond_noUnliftedArgs)
  | cls_key == functorClassKey 	   = Just (checkFlag Opt_DeriveFunctor `andCond`
    	                                   cond_functorOK True)	 -- NB: no cond_std!
  | cls_key == foldableClassKey	   = Just (checkFlag Opt_DeriveFoldable `andCond`
    	                                   cond_functorOK False) -- Functor/Fold/Trav works ok for rank-n types
  | cls_key == traversableClassKey = Just (checkFlag Opt_DeriveTraversable `andCond`
    	                                   cond_functorOK False)
  | getName cls `elem` typeableClassNames = Just (checkFlag Opt_DeriveDataTypeable `andCond` cond_typeableOK)
  | otherwise = Nothing
  where
    cls_key = getUnique cls
    cond_std = cond_stdOK mtheta

type Condition = (DynFlags, TyCon) -> Maybe SDoc
	-- first Bool is whether or not we are allowed to derive Data and Typeable
	-- second Bool is whether or not we are allowed to derive Functor
	-- TyCon is the *representation* tycon if the 
	--	data type is an indexed one
	-- Nothing => OK

orCond :: Condition -> Condition -> Condition
orCond c1 c2 tc 
  = case c1 tc of
	Nothing -> Nothing		-- c1 succeeds
	Just x  -> case c2 tc of	-- c1 fails
		     Nothing -> Nothing
		     Just y  -> Just (x $$ ptext (sLit "  and") $$ y)
					-- Both fail

andCond :: Condition -> Condition -> Condition
andCond c1 c2 tc = case c1 tc of
		     Nothing -> c2 tc	-- c1 succeeds
		     Just x  -> Just x	-- c1 fails

cond_stdOK :: DerivContext -> Condition
cond_stdOK (Just _) _
  = Nothing	-- Don't check these conservative conditions for
		-- standalone deriving; just generate the code
cond_stdOK Nothing (_, rep_tc)
  | null data_cons      = Just (no_cons_why $$ suggestion)
  | not (null con_whys) = Just (vcat con_whys $$ suggestion)
  | otherwise      	= Nothing
  where
    suggestion  = ptext (sLit "Possible fix: use a standalone deriving declaration instead")
    data_cons   = tyConDataCons rep_tc
    no_cons_why	= quotes (pprSourceTyCon rep_tc) <+> 
		  ptext (sLit "has no data constructors")

    con_whys = mapCatMaybes check_con data_cons

    check_con :: DataCon -> Maybe SDoc
    check_con con 
      | isVanillaDataCon con
      , all isTauTy (dataConOrigArgTys con) = Nothing
      | otherwise = Just (badCon con (ptext (sLit "does not have a Haskell-98 type")))
  
cond_enumOrProduct :: Condition
cond_enumOrProduct = cond_isEnumeration `orCond` 
		       (cond_isProduct `andCond` cond_noUnliftedArgs)

cond_noUnliftedArgs :: Condition
-- For some classes (eg Eq, Ord) we allow unlifted arg types
-- by generating specilaised code.  For others (eg Data) we don't.
cond_noUnliftedArgs (_, tc)
  | null bad_cons = Nothing
  | otherwise     = Just why
  where
    bad_cons = [ con | con <- tyConDataCons tc
		     , any isUnLiftedType (dataConOrigArgTys con) ]
    why = badCon (head bad_cons) (ptext (sLit "has arguments of unlifted type"))

cond_isEnumeration :: Condition
cond_isEnumeration (_, rep_tc)
  | isEnumerationTyCon rep_tc = Nothing
  | otherwise		      = Just why
  where
    why = quotes (pprSourceTyCon rep_tc) <+> 
	  ptext (sLit "has non-nullary constructors")

cond_isProduct :: Condition
cond_isProduct (_, rep_tc)
  | isProductTyCon rep_tc = Nothing
  | otherwise	          = Just why
  where
    why = quotes (pprSourceTyCon rep_tc) <+> 
	  ptext (sLit "has more than one constructor")

cond_typeableOK :: Condition
-- OK for Typeable class
-- Currently: (a) args all of kind *
--	      (b) 7 or fewer args
cond_typeableOK (_, rep_tc)
  | tyConArity rep_tc > 7	= Just too_many
  | not (all (isSubArgTypeKind . tyVarKind) (tyConTyVars rep_tc)) 
                                = Just bad_kind
  | isFamInstTyCon rep_tc	= Just fam_inst  -- no Typable for family insts
  | otherwise	  		= Nothing
  where
    too_many = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext (sLit "has too many arguments")
    bad_kind = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext (sLit "has arguments of kind other than `*'")
    fam_inst = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext (sLit "is a type family")


functorLikeClassKeys :: [Unique]
functorLikeClassKeys = [functorClassKey, foldableClassKey, traversableClassKey]

cond_functorOK :: Bool -> Condition
-- OK for Functor class
-- Currently: (a) at least one argument
--            (b) don't use argument contravariantly
--            (c) don't use argument in the wrong place, e.g. data T a = T (X a a)
--            (d) optionally: don't use function types
cond_functorOK allowFunctions (dflags, rep_tc) 
  | not (dopt Opt_DeriveFunctor dflags)
  = Just (ptext (sLit "You need -XDeriveFunctor to derive an instance for this class"))
  | otherwise
  = msum (map check_con data_cons)	-- msum picks the first 'Just', if any
  where
    data_cons = tyConDataCons rep_tc
    check_con con = msum (check_vanilla con : foldDataConArgs (ft_check con) con)

    check_vanilla :: DataCon -> Maybe SDoc
    check_vanilla con | isVanillaDataCon con = Nothing
    		      | otherwise	     = Just (badCon con existential)

    ft_check :: DataCon -> FFoldType (Maybe SDoc)
    ft_check con = FT { ft_triv = Nothing, ft_var = Nothing
                      , ft_co_var = Just (badCon con covariant)
	      	      , ft_fun = \x y -> if allowFunctions then x `mplus` y 
                                                           else Just (badCon con functions)
                      , ft_tup = \_ xs  -> msum xs
                      , ft_ty_app = \_ x   -> x
                      , ft_bad_app = Just (badCon con wrong_arg)
                      , ft_forall = \_ x   -> x }
                    
    existential = ptext (sLit "has existential arguments")
    covariant 	= ptext (sLit "uses the type variable in a function argument")
    functions 	= ptext (sLit "contains function types")
    wrong_arg 	= ptext (sLit "uses the type variable in an argument other than the last")

checkFlag :: ExtensionFlag -> Condition
checkFlag flag (dflags, _)
  | dopt flag dflags = Nothing
  | otherwise        = Just why
  where
    why = ptext (sLit "You need -X") <> text flag_str 
          <+> ptext (sLit "to derive an instance for this class")
    flag_str = case [ s | (s, f, _) <- xFlags, f==flag ] of
                 [s]   -> s
                 other -> pprPanic "checkFlag" (ppr other)

std_class_via_iso :: Class -> Bool
-- These standard classes can be derived for a newtype
-- using the isomorphism trick *even if no -XGeneralizedNewtypeDeriving
-- because giving so gives the same results as generating the boilerplate
std_class_via_iso clas	
  = classKey clas `elem` [eqClassKey, ordClassKey, ixClassKey, boundedClassKey]
	-- Not Read/Show because they respect the type
	-- Not Enum, because newtypes are never in Enum


non_iso_class :: Class -> Bool
-- *Never* derive Read,Show,Typeable,Data by isomorphism,
-- even with -XGeneralizedNewtypeDeriving
non_iso_class cls 
  = classKey cls `elem` ([readClassKey, showClassKey, dataClassKey] ++
			 typeableClassKeys)

typeableClassKeys :: [Unique]
typeableClassKeys = map getUnique typeableClassNames

new_dfun_name :: Class -> TyCon -> TcM Name
new_dfun_name clas tycon 	-- Just a simple wrapper
  = do { loc <- getSrcSpanM	-- The location of the instance decl, not of the tycon
	; newDFunName clas [mkTyConApp tycon []] loc }
	-- The type passed to newDFunName is only used to generate
	-- a suitable string; hence the empty type arg list

badCon :: DataCon -> SDoc -> SDoc
badCon con msg = ptext (sLit "Constructor") <+> quotes (ppr con) <+> msg
\end{code}

Note [Superclasses of derived instance] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, a derived instance decl needs the superclasses of the derived
class too.  So if we have
	data T a = ...deriving( Ord )
then the initial context for Ord (T a) should include Eq (T a).  Often this is 
redundant; we'll also generate an Ord constraint for each constructor argument,
and that will probably generate enough constraints to make the Eq (T a) constraint 
be satisfied too.  But not always; consider:

 data S a = S
 instance Eq (S a)
 instance Ord (S a)

 data T a = MkT (S a) deriving( Ord )
 instance Num a => Eq (T a)

The derived instance for (Ord (T a)) must have a (Num a) constraint!
Similarly consider:
	data T a = MkT deriving( Data, Typeable )
Here there *is* no argument field, but we must nevertheless generate
a context for the Data instances:
	instance Typable a => Data (T a) where ...


%************************************************************************
%*									*
		Deriving newtypes
%*									*
%************************************************************************

\begin{code}
mkNewTypeEqn :: InstOrigin -> DynFlags -> [Var] -> Class
             -> [Type] -> TyCon -> [Type] -> TyCon -> [Type]
             -> DerivContext
             -> TcRn EarlyDerivSpec
mkNewTypeEqn orig dflags tvs
             cls cls_tys tycon tc_args rep_tycon rep_tc_args mtheta
-- Want: instance (...) => cls (cls_tys ++ [tycon tc_args]) where ...
  | can_derive_via_isomorphism && (newtype_deriving || std_class_via_iso cls)
  = do	{ traceTc (text "newtype deriving:" <+> ppr tycon <+> ppr rep_tys)
	; dfun_name <- new_dfun_name cls tycon
  	; loc <- getSrcSpanM
	; let spec = DS { ds_loc = loc, ds_orig = orig
			, ds_name = dfun_name, ds_tvs = varSetElems dfun_tvs 
			, ds_cls = cls, ds_tys = inst_tys
			, ds_tc = rep_tycon, ds_tc_args = rep_tc_args
			, ds_theta =  mtheta `orElse` all_preds
			, ds_newtype = True }
	; return (if isJust mtheta then Right spec
				   else Left spec) }

  | otherwise
  = case checkSideConditions dflags mtheta cls cls_tys rep_tycon of
      CanDerive -> go_for_it 	-- Use the standard H98 method
      DerivableClassError msg 	-- Error with standard class
        | can_derive_via_isomorphism -> bale_out (msg $$ suggest_nd)
        | otherwise                  -> bale_out msg
      NonDerivableClass 	-- Must use newtype deriving
      	| newtype_deriving           -> bale_out cant_derive_err  -- Too hard, even with newtype deriving
        | can_derive_via_isomorphism -> bale_out (non_std $$ suggest_nd) -- Try newtype deriving!
      	| otherwise                  -> bale_out non_std
  where
        newtype_deriving = dopt Opt_GeneralizedNewtypeDeriving dflags
        go_for_it        = mk_data_eqn orig tvs cls tycon tc_args rep_tycon rep_tc_args mtheta
	bale_out msg     = failWithTc (derivingThingErr newtype_deriving cls cls_tys inst_ty msg)

	non_std    = nonStdErr cls
        suggest_nd = ptext (sLit "Try -XGeneralizedNewtypeDeriving for GHC's newtype-deriving extension")

	-- Here is the plan for newtype derivings.  We see
	--	  newtype T a1...an = MkT (t ak+1...an) deriving (.., C s1 .. sm, ...)
	-- where t is a type,
	-- 	 ak+1...an is a suffix of a1..an, and are all tyars
	--	 ak+1...an do not occur free in t, nor in the s1..sm
	-- 	 (C s1 ... sm) is a  *partial applications* of class C 
	--			with the last parameter missing
	--	 (T a1 .. ak) matches the kind of C's last argument
	--		(and hence so does t)
	-- The latter kind-check has been done by deriveTyData already,
	-- and tc_args are already trimmed
	--
	-- We generate the instance
	--	 instance forall ({a1..ak} u fvs(s1..sm)).
	--		  C s1 .. sm t => C s1 .. sm (T a1...ak)
	-- where T a1...ap is the partial application of 
	-- 	 the LHS of the correct kind and p >= k
	--
	--	NB: the variables below are:
	--		tc_tvs = [a1, ..., an]
	--		tyvars_to_keep = [a1, ..., ak]
	--		rep_ty = t ak .. an
	--		deriv_tvs = fvs(s1..sm) \ tc_tvs
	--		tys = [s1, ..., sm]
	--		rep_fn' = t
	--
	-- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
	-- We generate the instance
	--	instance Monad (ST s) => Monad (T s) where 

	nt_eta_arity = length (fst (newTyConEtadRhs rep_tycon))
		-- For newtype T a b = MkT (S a a b), the TyCon machinery already
		-- eta-reduces the representation type, so we know that
		-- 	T a ~ S a a
		-- That's convenient here, because we may have to apply
		-- it to fewer than its original complement of arguments

	-- Note [Newtype representation]
	-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	-- Need newTyConRhs (*not* a recursive representation finder) 
	-- to get the representation type. For example
	--	newtype B = MkB Int
	--	newtype A = MkA B deriving( Num )
	-- We want the Num instance of B, *not* the Num instance of Int,
	-- when making the Num instance of A!
	rep_inst_ty = newTyConInstRhs rep_tycon rep_tc_args
	rep_tys     = cls_tys ++ [rep_inst_ty]
	rep_pred    = mkClassPred cls rep_tys
		-- rep_pred is the representation dictionary, from where
		-- we are gong to get all the methods for the newtype
		-- dictionary 


    -- Next we figure out what superclass dictionaries to use
    -- See Note [Newtype deriving superclasses] above

	cls_tyvars = classTyVars cls
	dfun_tvs = tyVarsOfTypes inst_tys
	inst_ty = mkTyConApp tycon tc_args
	inst_tys = cls_tys ++ [inst_ty]
	sc_theta = substTheta (zipOpenTvSubst cls_tyvars inst_tys)
			      (classSCTheta cls)

		-- If there are no tyvars, there's no need
		-- to abstract over the dictionaries we need
		-- Example: 	newtype T = MkT Int deriving( C )
		-- We get the derived instance
		--		instance C T
		-- rather than
		--		instance C Int => C T
	all_preds = rep_pred : sc_theta		-- NB: rep_pred comes first

	-------------------------------------------------------------------
	--  Figuring out whether we can only do this newtype-deriving thing

	can_derive_via_isomorphism
	   =  not (non_iso_class cls)
	   && arity_ok
	   && eta_ok
	   && ats_ok
--	   && not (isRecursiveTyCon tycon)	-- Note [Recursive newtypes]

	arity_ok = length cls_tys + 1 == classArity cls
 		-- Well kinded; eg not: newtype T ... deriving( ST )
		--			because ST needs *2* type params

	-- Check that eta reduction is OK
	eta_ok = nt_eta_arity <= length rep_tc_args
		-- The newtype can be eta-reduced to match the number
		--     of type argument actually supplied
		--	  newtype T a b = MkT (S [a] b) deriving( Monad )
		--     Here the 'b' must be the same in the rep type (S [a] b)
		--     And the [a] must not mention 'b'.  That's all handled
		--     by nt_eta_rity.

	ats_ok = null (classATs cls)	
	       -- No associated types for the class, because we don't 
	       -- currently generate type 'instance' decls; and cannot do
	       -- so for 'data' instance decls
					 
	cant_derive_err
	   = vcat [ ppUnless arity_ok arity_msg
		  , ppUnless eta_ok eta_msg
		  , ppUnless ats_ok ats_msg ]
        arity_msg = quotes (ppr (mkClassPred cls cls_tys)) <+> ptext (sLit "does not have arity 1")
	eta_msg   = ptext (sLit "cannot eta-reduce the representation type enough")
	ats_msg   = ptext (sLit "the class has associated types")
\end{code}

Note [Recursive newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Newtype deriving works fine, even if the newtype is recursive.
e.g. 	newtype S1 = S1 [T1 ()]
	newtype T1 a = T1 (StateT S1 IO a ) deriving( Monad )
Remember, too, that type families are curretly (conservatively) given
a recursive flag, so this also allows newtype deriving to work
for type famillies.

We used to exclude recursive types, because we had a rather simple
minded way of generating the instance decl:
   newtype A = MkA [A]
   instance Eq [A] => Eq A	-- Makes typechecker loop!
But now we require a simple context, so it's ok.


%************************************************************************
%*									*
\subsection[TcDeriv-fixpoint]{Finding the fixed point of \tr{deriving} equations}
%*									*
%************************************************************************

A ``solution'' (to one of the equations) is a list of (k,TyVarTy tv)
terms, which is the final correct RHS for the corresponding original
equation.
\begin{itemize}
\item
Each (k,TyVarTy tv) in a solution constrains only a type
variable, tv.

\item
The (k,TyVarTy tv) pairs in a solution are canonically
ordered by sorting on type varible, tv, (major key) and then class, k,
(minor key)
\end{itemize}

\begin{code}
inferInstanceContexts :: OverlapFlag -> [DerivSpec] -> TcM [DerivSpec]

inferInstanceContexts _ [] = return []

inferInstanceContexts oflag infer_specs
  = do	{ traceTc (text "inferInstanceContexts" <+> vcat (map pprDerivSpec infer_specs))
	; iterate_deriv 1 initial_solutions }
  where
    ------------------------------------------------------------------
	-- The initial solutions for the equations claim that each
	-- instance has an empty context; this solution is certainly
	-- in canonical form.
    initial_solutions :: [ThetaType]
    initial_solutions = [ [] | _ <- infer_specs ]

    ------------------------------------------------------------------
	-- iterate_deriv calculates the next batch of solutions,
	-- compares it with the current one; finishes if they are the
	-- same, otherwise recurses with the new solutions.
	-- It fails if any iteration fails
    iterate_deriv :: Int -> [ThetaType] -> TcM [DerivSpec]
    iterate_deriv n current_solns
      | n > 20 	-- Looks as if we are in an infinite loop
		-- This can happen if we have -XUndecidableInstances
		-- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop" 
		 (vcat (map pprDerivSpec infer_specs) $$ ppr current_solns)
      | otherwise
      =	do { 	  -- Extend the inst info from the explicit instance decls
		  -- with the current set of solutions, and simplify each RHS
	     let inst_specs = zipWithEqual "add_solns" (mkInstance oflag)
					   current_solns infer_specs
	   ; new_solns <- checkNoErrs $
	     		  extendLocalInstEnv inst_specs $
	     		  mapM gen_soln infer_specs

	   ; if (current_solns == new_solns) then
		return [ spec { ds_theta = soln } 
                       | (spec, soln) <- zip infer_specs current_solns ]
	     else
		iterate_deriv (n+1) new_solns }

    ------------------------------------------------------------------
    gen_soln :: DerivSpec  -> TcM [PredType]
    gen_soln (DS { ds_loc = loc, ds_orig = orig, ds_tvs = tyvars 
		 , ds_cls = clas, ds_tys = inst_tys, ds_theta = deriv_rhs })
      = setSrcSpan loc	$
	addErrCtxt (derivInstCtxt clas inst_tys) $ 
	do { theta <- tcSimplifyDeriv orig tyvars deriv_rhs
	   	-- checkValidInstance tyvars theta clas inst_tys
		-- Not necessary; see Note [Exotic derived instance contexts]
		-- 		  in TcSimplify

		  -- Check for a bizarre corner case, when the derived instance decl should
		  -- have form 	instance C a b => D (T a) where ...
		  -- Note that 'b' isn't a parameter of T.  This gives rise to all sorts
		  -- of problems; in particular, it's hard to compare solutions for
		  -- equality when finding the fixpoint.  So I just rule it out for now.
 	   ; let tv_set = mkVarSet tyvars
	         weird_preds = [pred | pred <- theta, not (tyVarsOfPred pred `subVarSet` tv_set)]  
	   ; mapM_ (addErrTc . badDerivedPred) weird_preds	

           ; traceTc (text "TcDeriv" <+> (ppr deriv_rhs $$ ppr theta))
		-- Claim: the result instance declaration is guaranteed valid
		-- Hence no need to call:
		--   checkValidInstance tyvars theta clas inst_tys
	   ; return (sortLe (<=) theta) }	-- Canonicalise before returning the solution

------------------------------------------------------------------
mkInstance :: OverlapFlag -> ThetaType -> DerivSpec -> Instance
mkInstance overlap_flag theta
	    (DS { ds_name = dfun_name
		, ds_tvs = tyvars, ds_cls = clas, ds_tys = tys })
  = mkLocalInstance dfun overlap_flag
  where
    dfun = mkDictFunId dfun_name tyvars theta clas tys


extendLocalInstEnv :: [Instance] -> TcM a -> TcM a
-- Add new locally-defined instances; don't bother to check
-- for functional dependency errors -- that'll happen in TcInstDcls
extendLocalInstEnv dfuns thing_inside
 = do { env <- getGblEnv
      ; let  inst_env' = extendInstEnvList (tcg_inst_env env) dfuns 
	     env'      = env { tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-normal-binds]{Bindings for the various classes}
%*									*
%************************************************************************

After all the trouble to figure out the required context for the
derived instance declarations, all that's left is to chug along to
produce them.  They will then be shoved into @tcInstDecls2@, which
will do all its usual business.

There are lots of possibilities for code to generate.  Here are
various general remarks.

PRINCIPLES:
\begin{itemize}
\item
We want derived instances of @Eq@ and @Ord@ (both v common) to be
``you-couldn't-do-better-by-hand'' efficient.

\item
Deriving @Show@---also pretty common--- should also be reasonable good code.

\item
Deriving for the other classes isn't that common or that big a deal.
\end{itemize}

PRAGMATICS:

\begin{itemize}
\item
Deriving @Ord@ is done mostly with the 1.3 @compare@ method.

\item
Deriving @Eq@ also uses @compare@, if we're deriving @Ord@, too.

\item
We {\em normally} generate code only for the non-defaulted methods;
there are some exceptions for @Eq@ and (especially) @Ord@...

\item
Sometimes we use a @_con2tag_<tycon>@ function, which returns a data
constructor's numeric (@Int#@) tag.  These are generated by
@gen_tag_n_con_binds@, and the heuristic for deciding if one of
these is around is given by @hasCon2TagFun@.

The examples under the different sections below will make this
clearer.

\item
Much less often (really just for deriving @Ix@), we use a
@_tag2con_<tycon>@ function.  See the examples.

\item
We use the renamer!!!  Reason: we're supposed to be
producing @LHsBinds Name@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @MonoBinds RdrName@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}

\begin{code}
-- Generate the InstInfo for the required instance paired with the
--   *representation* tycon for that instance,
-- plus any auxiliary bindings required
--
-- Representation tycons differ from the tycon in the instance signature in
-- case of instances for indexed families.
--
genInst :: Bool 	-- True <=> standalone deriving
	-> OverlapFlag
        -> DerivSpec -> TcM (InstInfo RdrName, DerivAuxBinds)
genInst standalone_deriv oflag spec
  | ds_newtype spec
  = return (InstInfo { iSpec  = mkInstance oflag (ds_theta spec) spec
		     , iBinds = NewTypeDerived co rep_tycon }, [])

  | otherwise
  = do	{ let loc  = getSrcSpan (ds_name spec)
	      inst = mkInstance oflag (ds_theta spec) spec
   	      clas = ds_cls spec

          -- In case of a family instance, we need to use the representation
          -- tycon (after all, it has the data constructors)
	; fix_env <- getFixityEnv
	; let (meth_binds, aux_binds) = genDerivBinds loc fix_env clas rep_tycon
	      binds = VanillaInst meth_binds [] standalone_deriv
	; return (InstInfo { iSpec = inst, iBinds = binds }, aux_binds)
        }
  where
    rep_tycon   = ds_tc spec
    rep_tc_args = ds_tc_args spec
    co1 = case tyConFamilyCoercion_maybe rep_tycon of
    	      Nothing     -> IdCo
	      Just co_con -> ACo (mkTyConApp co_con rep_tc_args)
    co2 = case newTyConCo_maybe rep_tycon of
              Nothing     -> IdCo	-- The newtype is transparent; no need for a cast
	      Just co_con -> ACo (mkTyConApp co_con rep_tc_args)
    co = co1 `mkTransCoI` co2

-- Example: newtype instance N [a] = N1 (Tree a) 
--          deriving instance Eq b => Eq (N [(b,b)])
-- From the instance, we get an implicit newtype R1:N a = N1 (Tree a)
-- When dealing with the deriving clause
--    co1 : N [(b,b)] ~ R1:N (b,b)
--    co2 : R1:N (b,b) ~ Tree (b,b)
--    co  : N [(b,b)] ~ Tree (b,b)

genDerivBinds :: SrcSpan -> FixityEnv -> Class -> TyCon -> (LHsBinds RdrName, DerivAuxBinds)
genDerivBinds loc fix_env clas tycon
  | className clas `elem` typeableClassNames
  = (gen_Typeable_binds loc tycon, [])

  | otherwise
  = case assocMaybe gen_list (getUnique clas) of
	Just gen_fn -> gen_fn loc tycon
	Nothing	    -> pprPanic "genDerivBinds: bad derived class" (ppr clas)
  where
    gen_list :: [(Unique, SrcSpan -> TyCon -> (LHsBinds RdrName, DerivAuxBinds))]
    gen_list = [(eqClassKey,       gen_Eq_binds)
 	       ,(ordClassKey,      gen_Ord_binds)
 	       ,(enumClassKey,     gen_Enum_binds)
 	       ,(boundedClassKey,  gen_Bounded_binds)
 	       ,(ixClassKey,       gen_Ix_binds)
 	       ,(showClassKey,     gen_Show_binds fix_env)
 	       ,(readClassKey,     gen_Read_binds fix_env)
	       ,(dataClassKey,     gen_Data_binds)
	       ,(functorClassKey,  gen_Functor_binds)
	       ,(foldableClassKey, gen_Foldable_binds)
	       ,(traversableClassKey, gen_Traversable_binds)
 	       ]
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
%*									*
%************************************************************************

\begin{code}
derivingKindErr :: TyCon -> Class -> [Type] -> Kind -> Message
derivingKindErr tc cls cls_tys cls_kind
  = hang (ptext (sLit "Cannot derive well-kinded instance of form")
		<+> quotes (pprClassPred cls cls_tys <+> parens (ppr tc <+> ptext (sLit "..."))))
       2 (ptext (sLit "Class") <+> quotes (ppr cls)
	    <+> ptext (sLit "expects an argument of kind") <+> quotes (pprKind cls_kind))

derivingEtaErr :: Class -> [Type] -> Type -> Message
derivingEtaErr cls cls_tys inst_ty
  = sep [ptext (sLit "Cannot eta-reduce to an instance of form"),
	 nest 2 (ptext (sLit "instance (...) =>")
		<+> pprClassPred cls (cls_tys ++ [inst_ty]))]

typeFamilyPapErr :: TyCon -> Class -> [Type] -> Type -> Message
typeFamilyPapErr tc cls cls_tys inst_ty
  = hang (ptext (sLit "Derived instance") <+> quotes (pprClassPred cls (cls_tys ++ [inst_ty])))
       2 (ptext (sLit "requires illegal partial application of data type family") <+> ppr tc) 

derivingThingErr :: Bool -> Class -> [Type] -> Type -> Message -> Message
derivingThingErr newtype_deriving clas tys ty why
  = sep [(hang (ptext (sLit "Can't make a derived instance of"))
	     2 (quotes (ppr pred)) 
          $$ nest 2 extra) <> colon,
	 nest 2 why]
  where
    extra | newtype_deriving = ptext (sLit "(even with cunning newtype deriving)")
          | otherwise        = empty
    pred = mkClassPred clas (tys ++ [ty])

derivingHiddenErr :: TyCon -> SDoc
derivingHiddenErr tc
  = hang (ptext (sLit "The data constructors of") <+> quotes (ppr tc) <+> ptext (sLit "are not all in scope"))
       2 (ptext (sLit "so you cannot derive an instance for it"))

standaloneCtxt :: LHsType Name -> SDoc
standaloneCtxt ty = hang (ptext (sLit "In the stand-alone deriving instance for")) 
		       2 (quotes (ppr ty))

derivInstCtxt :: Class -> [Type] -> Message
derivInstCtxt clas inst_tys
  = ptext (sLit "When deriving the instance for") <+> parens (pprClassPred clas inst_tys)

badDerivedPred :: PredType -> Message
badDerivedPred pred
  = vcat [ptext (sLit "Can't derive instances where the instance context mentions"),
	  ptext (sLit "type variables that are not data type parameters"),
	  nest 2 (ptext (sLit "Offending constraint:") <+> ppr pred)]
\end{code}
