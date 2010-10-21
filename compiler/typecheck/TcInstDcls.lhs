%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcInstDecls: Typechecking instance declarations

\begin{code}
module TcInstDcls ( tcInstDecls1, tcInstDecls2 ) where

import HsSyn
import TcBinds
import TcTyClsDecls
import TcClassDcl
import TcPat( addInlinePrags )
import TcRnMonad
import TcMType
import TcType
import Inst
import InstEnv
import FamInst
import FamInstEnv
import MkCore	( nO_METHOD_BINDING_ERROR_ID )
import TcDeriv
import TcEnv
import RnSource ( addTcgDUs )
import TcSimplify( simplifySuperClass )
import TcHsType
import TcUnify
import Type
import Coercion
import TyCon
import DataCon
import Class
import Var
import CoreUtils  ( mkPiTypes )
import CoreUnfold ( mkDFunUnfolding )
import CoreSyn	  ( Expr(Var) )
import Id
import MkId
import Name
import NameSet
import DynFlags
import SrcLoc
import Util
import Outputable
import Bag
import BasicTypes
import HscTypes
import FastString
import Maybes	( orElse )
import Data.Maybe
import Control.Monad
import Data.List

#include "HsVersions.h"
\end{code}

Typechecking instance declarations is done in two passes. The first
pass, made by @tcInstDecls1@, collects information to be used in the
second pass.

This pre-processed info includes the as-yet-unprocessed bindings
inside the instance declaration.  These are type-checked in the second
pass, when the class-instance envs and GVE contain all the info from
all the instance and value decls.  Indeed that's the reason we need
two passes over the instance decls.


Note [How instance declarations are translated]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is how we translation instance declarations into Core

Running example:
	class C a where
	   op1, op2 :: Ix b => a -> b -> b
	   op2 = <dm-rhs>

	instance C a => C [a]
	   {-# INLINE [2] op1 #-}
	   op1 = <rhs>
===>
	-- Method selectors
	op1,op2 :: forall a. C a => forall b. Ix b => a -> b -> b
	op1 = ...
	op2 = ...

	-- Default methods get the 'self' dictionary as argument
	-- so they can call other methods at the same type
	-- Default methods get the same type as their method selector
	$dmop2 :: forall a. C a => forall b. Ix b => a -> b -> b
	$dmop2 = /\a. \(d:C a). /\b. \(d2: Ix b). <dm-rhs>
	       -- NB: type variables 'a' and 'b' are *both* in scope in <dm-rhs>
	       -- Note [Tricky type variable scoping]

	-- A top-level definition for each instance method
	-- Here op1_i, op2_i are the "instance method Ids"
	-- The INLINE pragma comes from the user pragma
	{-# INLINE [2] op1_i #-}  -- From the instance decl bindings
	op1_i, op2_i :: forall a. C a => forall b. Ix b => [a] -> b -> b
	op1_i = /\a. \(d:C a). 
	       let this :: C [a]
		   this = df_i a d
	             -- Note [Subtle interaction of recursion and overlap]

		   local_op1 :: forall b. Ix b => [a] -> b -> b
	           local_op1 = <rhs>
	       	     -- Source code; run the type checker on this
		     -- NB: Type variable 'a' (but not 'b') is in scope in <rhs>
		     -- Note [Tricky type variable scoping]

	       in local_op1 a d

	op2_i = /\a \d:C a. $dmop2 [a] (df_i a d) 

	-- The dictionary function itself
	{-# NOINLINE CONLIKE df_i #-}	-- Never inline dictionary functions
	df_i :: forall a. C a -> C [a]
	df_i = /\a. \d:C a. MkC (op1_i a d) (op2_i a d)
		-- But see Note [Default methods in instances]
		-- We can't apply the type checker to the default-method call

        -- Use a RULE to short-circuit applications of the class ops
	{-# RULE "op1@C[a]" forall a, d:C a. 
                            op1 [a] (df_i d) = op1_i a d #-}

Note [Instances and loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Note that df_i may be mutually recursive with both op1_i and op2_i.
  It's crucial that df_i is not chosen as the loop breaker, even 
  though op1_i has a (user-specified) INLINE pragma.

* Instead the idea is to inline df_i into op1_i, which may then select
  methods from the MkC record, and thereby break the recursion with
  df_i, leaving a *self*-recurisve op1_i.  (If op1_i doesn't call op at
  the same type, it won't mention df_i, so there won't be recursion in
  the first place.)  

* If op1_i is marked INLINE by the user there's a danger that we won't
  inline df_i in it, and that in turn means that (since it'll be a
  loop-breaker because df_i isn't), op1_i will ironically never be 
  inlined.  But this is OK: the recursion breaking happens by way of
  a RULE (the magic ClassOp rule above), and RULES work inside InlineRule
  unfoldings. See Note [RULEs enabled in SimplGently] in SimplUtils

Note [ClassOp/DFun selection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One thing we see a lot is stuff like
    op2 (df d1 d2)
where 'op2' is a ClassOp and 'df' is DFun.  Now, we could inline *both*
'op2' and 'df' to get
     case (MkD ($cop1 d1 d2) ($cop2 d1 d2) ... of
       MkD _ op2 _ _ _ -> op2
And that will reduce to ($cop2 d1 d2) which is what we wanted.

But it's tricky to make this work in practice, because it requires us to 
inline both 'op2' and 'df'.  But neither is keen to inline without having
seen the other's result; and it's very easy to get code bloat (from the 
big intermediate) if you inline a bit too much.

Instead we use a cunning trick.
 * We arrange that 'df' and 'op2' NEVER inline.  

 * We arrange that 'df' is ALWAYS defined in the sylised form
      df d1 d2 = MkD ($cop1 d1 d2) ($cop2 d1 d2) ...

 * We give 'df' a magical unfolding (DFunUnfolding [$cop1, $cop2, ..])
   that lists its methods.

 * We make CoreUnfold.exprIsConApp_maybe spot a DFunUnfolding and return
   a suitable constructor application -- inlining df "on the fly" as it 
   were.

 * We give the ClassOp 'op2' a BuiltinRule that extracts the right piece
   iff its argument satisfies exprIsConApp_maybe.  This is done in
   MkId mkDictSelId

 * We make 'df' CONLIKE, so that shared uses stil match; eg
      let d = df d1 d2
      in ...(op2 d)...(op1 d)...

Note [Single-method classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the class has just one method (or, more accurately, just one element
of {superclasses + methods}), then we still use the *same* strategy

   class C a where op :: a -> a
   instance C a => C [a] where op = <blah>

We translate the class decl into a newtype, which just gives
a top-level axiom:

   axiom Co:C a :: C a ~ (a->a)

   op :: forall a. C a -> (a -> a)
   op a d = d |> (Co:C a)

   MkC :: forall a. (a->a) -> C a
   MkC = /\a.\op. op |> (sym Co:C a)

   df :: forall a. C a => C [a]
   {-# NOINLINE df   DFun[ $cop_list ] #-}
   df = /\a. \d. MkC ($cop_list a d)

   $cop_list :: forall a. C a => [a] -> [a]
   $cop_list = <blah>

The "constructor" MkC expands to a cast, as does the class-op selector.
The RULE works just like for multi-field dictionaries:

  * (df a d) returns (Just (MkC,..,[$cop_list a d])) 
    to exprIsConApp_Maybe

  * The RULE for op picks the right result

This is a bit of a hack, because (df a d) isn't *really* a constructor
application.  But it works just fine in this case, exprIsConApp_maybe
is otherwise used only when we hit a case expression which will have
a real data constructor in it.

The biggest reason for doing it this way, apart from uniformity, is
that we want to be very careful when we have
    instance C a => C [a] where
      {-# INLINE op #-}
      op = ...
then we'll get an INLINE pragma on $cop_list but it's important that
$cop_list only inlines when it's applied to *two* arguments (the
dictionary and the list argument

The danger is that we'll get something like
      op_list :: C a => [a] -> [a]
      op_list = /\a.\d. $cop_list a d
and then we'll eta expand, and then we'll inline TOO EARLY. This happened in 
Trac #3772 and I spent far too long fiddling around trying to fix it.
Look at the test for Trac #3772.

     (Note: re-reading the above, I can't see how using the
            uniform story solves the problem.)

Note [Subtle interaction of recursion and overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
  class C a where { op1,op2 :: a -> a }
  instance C a => C [a] where
    op1 x = op2 x ++ op2 x
    op2 x = ...
  instance C [Int] where
    ...

When type-checking the C [a] instance, we need a C [a] dictionary (for
the call of op2).  If we look up in the instance environment, we find
an overlap.  And in *general* the right thing is to complain (see Note
[Overlapping instances] in InstEnv).  But in *this* case it's wrong to
complain, because we just want to delegate to the op2 of this same
instance.  

Why is this justified?  Because we generate a (C [a]) constraint in 
a context in which 'a' cannot be instantiated to anything that matches
other overlapping instances, or else we would not be excecuting this
version of op1 in the first place.

It might even be a bit disguised:

  nullFail :: C [a] => [a] -> [a]
  nullFail x = op2 x ++ op2 x

  instance C a => C [a] where
    op1 x = nullFail x

Precisely this is used in package 'regex-base', module Context.hs.
See the overlapping instances for RegexContext, and the fact that they
call 'nullFail' just like the example above.  The DoCon package also
does the same thing; it shows up in module Fraction.hs

Conclusion: when typechecking the methods in a C [a] instance, we want
to have C [a] available.  That is why we have the strange local
definition for 'this' in the definition of op1_i in the example above.
We can typecheck the defintion of local_op1, and when doing tcSimplifyCheck
we supply 'this' as a given dictionary.  Only needed, though, if there
are some type variables involved; otherwise there can be no overlap and
none of this arises.

Note [Tricky type variable scoping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In our example
	class C a where
	   op1, op2 :: Ix b => a -> b -> b
	   op2 = <dm-rhs>

	instance C a => C [a]
	   {-# INLINE [2] op1 #-}
	   op1 = <rhs>

note that 'a' and 'b' are *both* in scope in <dm-rhs>, but only 'a' is
in scope in <rhs>.  In particular, we must make sure that 'b' is in
scope when typechecking <dm-rhs>.  This is achieved by subFunTys,
which brings appropriate tyvars into scope. This happens for both
<dm-rhs> and for <rhs>, but that doesn't matter: the *renamer* will have
complained if 'b' is mentioned in <rhs>.



%************************************************************************
%*                                                                      *
\subsection{Extracting instance decls}
%*                                                                      *
%************************************************************************

Gather up the instance declarations from their various sources

\begin{code}
tcInstDecls1    -- Deal with both source-code and imported instance decls
   :: [LTyClDecl Name]          -- For deriving stuff
   -> [LInstDecl Name]          -- Source code instance decls
   -> [LDerivDecl Name]         -- Source code stand-alone deriving decls
   -> TcM (TcGblEnv,            -- The full inst env
           [InstInfo Name],     -- Source-code instance decls to process;
                                -- contains all dfuns for this module
           HsValBinds Name)     -- Supporting bindings for derived instances

tcInstDecls1 tycl_decls inst_decls deriv_decls
  = checkNoErrs $
    do {        -- Stop if addInstInfos etc discovers any errors
                -- (they recover, so that we get more than one error each
                -- round)

                -- (1) Do class and family instance declarations
       ; idx_tycons        <- mapAndRecoverM (tcFamInstDecl TopLevel) $
       	 		      filter (isFamInstDecl . unLoc) tycl_decls 
       ; local_info_tycons <- mapAndRecoverM tcLocalInstDecl1  inst_decls

       ; let { (local_info,
                at_tycons_s)   = unzip local_info_tycons
             ; at_idx_tycons   = concat at_tycons_s ++ idx_tycons
             ; clas_decls      = filter (isClassDecl.unLoc) tycl_decls
             ; implicit_things = concatMap implicitTyThings at_idx_tycons
	     ; aux_binds       = mkRecSelBinds at_idx_tycons
             }

                -- (2) Add the tycons of indexed types and their implicit
                --     tythings to the global environment
       ; tcExtendGlobalEnv (at_idx_tycons ++ implicit_things) $ do {

                -- (3) Instances from generic class declarations
       ; generic_inst_info <- getGenericInstances clas_decls

                -- Next, construct the instance environment so far, consisting
                -- of
                --   (a) local instance decls
                --   (b) generic instances
                --   (c) local family instance decls
       ; addInsts local_info         $
         addInsts generic_inst_info  $
         addFamInsts at_idx_tycons   $ do {

                -- (4) Compute instances from "deriving" clauses;
                -- This stuff computes a context for the derived instance
                -- decl, so it needs to know about all the instances possible
                -- NB: class instance declarations can contain derivings as
                --     part of associated data type declarations
	 failIfErrsM		-- If the addInsts stuff gave any errors, don't
				-- try the deriving stuff, becuase that may give
				-- more errors still
       ; (deriv_inst_info, deriv_binds, deriv_dus) 
              <- tcDeriving tycl_decls inst_decls deriv_decls
       ; gbl_env <- addInsts deriv_inst_info getGblEnv
       ; return ( addTcgDUs gbl_env deriv_dus,
                  generic_inst_info ++ deriv_inst_info ++ local_info,
                  aux_binds `plusHsValBinds` deriv_binds)
    }}}

addInsts :: [InstInfo Name] -> TcM a -> TcM a
addInsts infos thing_inside
  = tcExtendLocalInstEnv (map iSpec infos) thing_inside

addFamInsts :: [TyThing] -> TcM a -> TcM a
addFamInsts tycons thing_inside
  = tcExtendLocalFamInstEnv (map mkLocalFamInstTyThing tycons) thing_inside
  where
    mkLocalFamInstTyThing (ATyCon tycon) = mkLocalFamInst tycon
    mkLocalFamInstTyThing tything        = pprPanic "TcInstDcls.addFamInsts"
                                                    (ppr tything)
\end{code}

\begin{code}
tcLocalInstDecl1 :: LInstDecl Name
                 -> TcM (InstInfo Name, [TyThing])
        -- A source-file instance declaration
        -- Type-check all the stuff before the "where"
        --
        -- We check for respectable instance type, and context
tcLocalInstDecl1 (L loc (InstDecl poly_ty binds uprags ats))
  = setSrcSpan loc		        $
    addErrCtxt (instDeclCtxt1 poly_ty)  $

    do  { is_boot <- tcIsHsBoot
        ; checkTc (not is_boot || (isEmptyLHsBinds binds && null uprags))
                  badBootDeclErr

        ; (tyvars, theta, tau) <- tcHsInstHead poly_ty

        -- Now, check the validity of the instance.
        ; (clas, inst_tys) <- checkValidInstance poly_ty tyvars theta tau

        -- Next, process any associated types.
        ; idx_tycons <- recoverM (return []) $
	  	     do { idx_tycons <- checkNoErrs $ 
                                        mapAndRecoverM (tcFamInstDecl NotTopLevel) ats
		     	; checkValidAndMissingATs clas (tyvars, inst_tys)
                          			  (zip ats idx_tycons)
			; return idx_tycons }

        -- Finally, construct the Core representation of the instance.
        -- (This no longer includes the associated types.)
        ; dfun_name <- newDFunName clas inst_tys (getLoc poly_ty)
		-- Dfun location is that of instance *header*
        ; overlap_flag <- getOverlapFlag
        ; let (eq_theta,dict_theta) = partition isEqPred theta
              theta'         = eq_theta ++ dict_theta
              dfun           = mkDictFunId dfun_name tyvars theta' clas inst_tys
              ispec          = mkLocalInstance dfun overlap_flag

        ; return (InstInfo { iSpec  = ispec,
                             iBinds = VanillaInst binds uprags False },
                  idx_tycons)
        }
  where
    -- We pass in the source form and the type checked form of the ATs.  We
    -- really need the source form only to be able to produce more informative
    -- error messages.
    checkValidAndMissingATs :: Class
                            -> ([TyVar], [TcType])     -- instance types
                            -> [(LTyClDecl Name,       -- source form of AT
                                 TyThing)]    	       -- Core form of AT
                            -> TcM ()
    checkValidAndMissingATs clas inst_tys ats
      = do { -- Issue a warning for each class AT that is not defined in this
             -- instance.
           ; let class_ats   = map tyConName (classATs clas)
                 defined_ats = listToNameSet . map (tcdName.unLoc.fst)  $ ats
                 omitted     = filterOut (`elemNameSet` defined_ats) class_ats
           ; warn <- doptM Opt_WarnMissingMethods
           ; mapM_ (warnTc warn . omittedATWarn) omitted

             -- Ensure that all AT indexes that correspond to class parameters
             -- coincide with the types in the instance head.  All remaining
             -- AT arguments must be variables.  Also raise an error for any
             -- type instances that are not associated with this class.
           ; mapM_ (checkIndexes clas inst_tys) ats
           }

    checkIndexes clas inst_tys (hsAT, ATyCon tycon)
-- !!!TODO: check that this does the Right Thing for indexed synonyms, too!
      = checkIndexes' clas inst_tys hsAT
                      (tyConTyVars tycon,
                       snd . fromJust . tyConFamInst_maybe $ tycon)
    checkIndexes _ _ _ = panic "checkIndexes"

    checkIndexes' clas (instTvs, instTys) hsAT (atTvs, atTys)
      = let atName = tcdName . unLoc $ hsAT
        in
        setSrcSpan (getLoc hsAT)       $
        addErrCtxt (atInstCtxt atName) $
        case find ((atName ==) . tyConName) (classATs clas) of
          Nothing     -> addErrTc $ badATErr clas atName  -- not in this class
          Just atycon ->
                -- The following is tricky!  We need to deal with three
                -- complications: (1) The AT possibly only uses a subset of
                -- the class parameters as indexes and those it uses may be in
                -- a different order; (2) the AT may have extra arguments,
                -- which must be type variables; and (3) variables in AT and
                -- instance head will be different `Name's even if their
                -- source lexemes are identical.
		--
		-- e.g.    class C a b c where 
		-- 	     data D b a :: * -> *           -- NB (1) b a, omits c
		-- 	   instance C [x] Bool Char where 
		--	     data D Bool [x] v = MkD x [v]  -- NB (2) v
		--	     	  -- NB (3) the x in 'instance C...' have differnt
		--		  --        Names to x's in 'data D...'
                --
                -- Re (1), `poss' contains a permutation vector to extract the
                -- class parameters in the right order.
                --
                -- Re (2), we wrap the (permuted) class parameters in a Maybe
                -- type and use Nothing for any extra AT arguments.  (First
                -- equation of `checkIndex' below.)
                --
                -- Re (3), we replace any type variable in the AT parameters
                -- that has the same source lexeme as some variable in the
                -- instance types with the instance type variable sharing its
                -- source lexeme.
                --
                let poss :: [Int]
                    -- For *associated* type families, gives the position
                    -- of that 'TyVar' in the class argument list (0-indexed)
	   	    -- e.g.  class C a b c where { type F c a :: *->* }
           	    --       Then we get Just [2,0]
	            poss = catMaybes [ tv `elemIndex` classTyVars clas 
                                     | tv <- tyConTyVars atycon]
                       -- We will get Nothings for the "extra" type 
                       -- variables in an associated data type
                       -- e.g. class C a where { data D a :: *->* }
                       -- here D gets arity 2 and has two tyvars

                    relevantInstTys = map (instTys !!) poss
                    instArgs        = map Just relevantInstTys ++
                                      repeat Nothing  -- extra arguments
                    renaming        = substSameTyVar atTvs instTvs
                in
                zipWithM_ checkIndex (substTys renaming atTys) instArgs

    checkIndex ty Nothing
      | isTyVarTy ty         = return ()
      | otherwise            = addErrTc $ mustBeVarArgErr ty
    checkIndex ty (Just instTy)
      | ty `tcEqType` instTy = return ()
      | otherwise            = addErrTc $ wrongATArgErr ty instTy

    listToNameSet = addListToNameSet emptyNameSet

    substSameTyVar []       _            = emptyTvSubst
    substSameTyVar (tv:tvs) replacingTvs =
      let replacement = case find (tv `sameLexeme`) replacingTvs of
                        Nothing  -> mkTyVarTy tv
                        Just rtv -> mkTyVarTy rtv
          --
          tv1 `sameLexeme` tv2 =
            nameOccName (tyVarName tv1) == nameOccName (tyVarName tv2)
      in
      extendTvSubst (substSameTyVar tvs replacingTvs) tv replacement
\end{code}


%************************************************************************
%*                                                                      *
      Type-checking instance declarations, pass 2
%*                                                                      *
%************************************************************************

\begin{code}
tcInstDecls2 :: [LTyClDecl Name] -> [InstInfo Name]
             -> TcM (LHsBinds Id)
-- (a) From each class declaration,
--      generate any default-method bindings
-- (b) From each instance decl
--      generate the dfun binding

tcInstDecls2 tycl_decls inst_decls
  = do  { -- (a) Default methods from class decls
          let class_decls = filter (isClassDecl . unLoc) tycl_decls
        ; dm_binds_s <- mapM tcClassDecl2 class_decls
        ; let dm_binds = unionManyBags dm_binds_s
                                    
          -- (b) instance declarations
	; let dm_ids = collectHsBindsBinders dm_binds
	      -- Add the default method Ids (again)
	      -- See Note [Default methods and instances]
        ; inst_binds_s <- tcExtendIdEnv dm_ids $
                          mapM tcInstDecl2 inst_decls

          -- Done
        ; return (dm_binds `unionBags` unionManyBags inst_binds_s) }

tcInstDecl2 :: InstInfo Name -> TcM (LHsBinds Id)
tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = ibinds })
  = recoverM (return emptyLHsBinds)             $
    setSrcSpan loc                              $
    addErrCtxt (instDeclCtxt2 (idType dfun_id)) $ 
    tc_inst_decl2 dfun_id ibinds
 where
    dfun_id = instanceDFunId ispec
    loc     = getSrcSpan dfun_id
\end{code}

See Note [Default methods and instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The default method Ids are already in the type environment (see Note
[Default method Ids and Template Haskell] in TcTyClsDcls), BUT they
don't have their InlinePragmas yet.  Usually that would not matter,
because the simplifier propagates information from binding site to
use.  But, unusually, when compiling instance decls we *copy* the
INLINE pragma from the default method to the method for that
particular operation (see Note [INLINE and default methods] below).

So right here in tcInstDecl2 we must re-extend the type envt with
the default method Ids replete with their INLINE pragmas.  Urk.

\begin{code}
tc_inst_decl2 :: Id -> InstBindings Name -> TcM (LHsBinds Id)
-- Returns a binding for the dfun
tc_inst_decl2 dfun_id inst_binds
 = do { let rigid_info = InstSkol
            inst_ty    = idType dfun_id
            loc        = getSrcSpan dfun_id

        -- Instantiate the instance decl with skolem constants
       ; (inst_tyvars', dfun_theta', inst_head') <- tcSkolSigType rigid_info inst_ty
                -- These inst_tyvars' scope over the 'where' part
                -- Those tyvars are inside the dfun_id's type, which is a bit
                -- bizarre, but OK so long as you realise it!
       ; let
            (clas, inst_tys') = tcSplitDFunHead inst_head'
            (class_tyvars, sc_theta, sc_sels, op_items) = classBigSig clas

             -- Instantiate the super-class context with inst_tys
            sc_theta' = substTheta (zipOpenTvSubst class_tyvars inst_tys') sc_theta

         -- Create dictionary Ids from the specified instance contexts.
       ; dfun_ev_vars <- newEvVars dfun_theta'
       ; self_dict    <- newSelfDict clas inst_tys'
                -- Default-method Ids may be mentioned in synthesised RHSs,
                -- but they'll already be in the environment.

	-- Cook up a binding for "self = df d1 .. dn",
	-- to use in each method binding
 	-- Why?  See Note [Subtle interaction of recursion and overlap]
       ; let self_ev_bind = EvBind self_dict $ 
                            EvDFunApp dfun_id (mkTyVarTys inst_tyvars') dfun_ev_vars

       -- Deal with 'SPECIALISE instance' pragmas
       -- See Note [SPECIALISE instance pragmas]
       ; spec_info <- tcSpecInstPrags dfun_id inst_binds

        -- Typecheck the methods
       ; (meth_ids, meth_binds) 
           <- tcExtendTyVarEnv inst_tyvars' $
              tcInstanceMethods dfun_id clas inst_tyvars' dfun_ev_vars 
	     	     	 	inst_tys' self_ev_bind spec_info
                                op_items inst_binds

         -- Figure out bindings for the superclass context
       ; let tc_sc = tcSuperClass inst_tyvars' dfun_ev_vars self_ev_bind
             (sc_eqs, sc_dicts) = splitAt (classSCNEqs clas) sc_theta'
       ; (sc_dict_ids, sc_binds) <- ASSERT( equalLength sc_sels sc_dicts )
                                    ASSERT( all isEqPred sc_eqs )
                                    mapAndUnzipM tc_sc (sc_sels `zip` sc_dicts)

				    -- NOT FINISHED!
       ; (_eq_sc_binds, sc_eq_vars) <- checkConstraints InstSkol
                                           inst_tyvars' dfun_ev_vars $
                                      emitWanteds ScOrigin sc_eqs

       -- Create the result bindings
       ; let dict_constr       = classDataCon clas
	     dict_bind         = mkVarBind self_dict dict_rhs
             dict_rhs          = foldl mk_app inst_constr dict_and_meth_ids
             dict_and_meth_ids = sc_dict_ids ++ meth_ids
 	     inst_constr   = L loc $ wrapId (mkWpEvVarApps sc_eq_vars 
                                             <.> mkWpTyApps inst_tys')
	     			            (dataConWrapId dict_constr)
                     -- We don't produce a binding for the dict_constr; instead we
                     -- rely on the simplifier to unfold this saturated application
                     -- We do this rather than generate an HsCon directly, because
                     -- it means that the special cases (e.g. dictionary with only one
                     -- member) are dealt with by the common MkId.mkDataConWrapId code rather
                     -- than needing to be repeated here.

	     mk_app :: LHsExpr Id -> Id -> LHsExpr Id
 	     mk_app fun arg_id = L loc (HsApp fun (L loc (wrapId arg_wrapper arg_id)))
	     arg_wrapper = mkWpEvVarApps dfun_ev_vars <.> mkWpTyApps (mkTyVarTys inst_tyvars')

	        -- Do not inline the dfun; instead give it a magic DFunFunfolding
	        -- See Note [ClassOp/DFun selection]
		-- See also note [Single-method classes]
             dfun_id_w_fun = dfun_id  
                             `setIdUnfolding`  mkDFunUnfolding inst_ty (map Var dict_and_meth_ids)
			     		       		-- Not right for equality superclasses
                             `setInlinePragma` dfunInlinePragma

             (spec_inst_prags, _) = spec_info
             main_bind = AbsBinds { abs_tvs = inst_tyvars'
                                  , abs_ev_vars = dfun_ev_vars
                                  , abs_exports = [(inst_tyvars', dfun_id_w_fun, self_dict, 
                                                    SpecPrags spec_inst_prags)]
                                  , abs_ev_binds = emptyTcEvBinds
                                  , abs_binds = unitBag dict_bind }

       ; return (unitBag (L loc main_bind) `unionBags` 
		 listToBag meth_binds      `unionBags` 
                 listToBag sc_binds)
       }

------------------------------
tcSpecInstPrags :: DFunId -> InstBindings Name 
                -> TcM ([Located TcSpecPrag], PragFun)
tcSpecInstPrags _ (NewTypeDerived {})
  = return ([], \_ -> [])
tcSpecInstPrags dfun_id (VanillaInst binds uprags _)
  = do { spec_inst_prags <- mapM (wrapLocM (tcSpecInst dfun_id)) $
                            filter isSpecInstLSig uprags
	     -- The filter removes the pragmas for methods
       ; return (spec_inst_prags, mkPragFun uprags binds) }

------------------------------
tcSuperClass :: [TyVar] -> [EvVar]
	     -> EvBind
	     -> (Id, PredType) -> TcM (Id, LHsBind Id)
-- Build a top level decl like
--	sc_op = /\a \d. let this = ... in 
--		        let sc = ... in
--			sc
-- The "this" part is just-in-case (discarded if not used)
-- See Note [Recursive superclasses]
tcSuperClass tyvars dicts 
             self_ev_bind@(EvBind self_dict _)
	     (sc_sel, sc_pred)
  = do { (ev_binds, wanted, sc_dict)
             <- newImplication InstSkol tyvars dicts $
                emitWanted ScOrigin sc_pred

       ; simplifySuperClass self_dict wanted
         -- We include self_dict in the 'givens'; the simplifier
         -- is clever enough to stop sc_pred geting bound by just 
         -- selecting from self_dict!!

       ; uniq <- newUnique
       ; let sc_op_ty   = mkForAllTys tyvars $ mkPiTypes dicts (varType sc_dict)
	     sc_op_name = mkDerivedInternalName mkClassOpAuxOcc uniq
						(getName sc_sel)
	     sc_op_id   = mkLocalId sc_op_name sc_op_ty
	     sc_op_bind = VarBind { var_id = sc_op_id, var_inline = False
                                  , var_rhs = L noSrcSpan $ wrapId sc_wrapper sc_dict }
             sc_wrapper = mkWpTyLams tyvars
                          <.> mkWpLams dicts
                          <.> mkWpLet (EvBinds (unitBag self_ev_bind))
			  <.> mkWpLet ev_binds

       ; return (sc_op_id, noLoc sc_op_bind) }
\end{code}

Note [Recursive superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #1470 for why we would *like* to add "self_dict" to the 
available instances here.  But we can't do so because then the superclases
get satisfied by selection from self_dict, and that leads to an immediate
loop.  What we need is to add self_dict to Avails without adding its 
superclasses, and we currently have no way to do that.

Note [SPECIALISE instance pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   instance (Ix a, Ix b) => Ix (a,b) where
     {-# SPECIALISE instance Ix (Int,Int) #-}
     range (x,y) = ...

We do *not* want to make a specialised version of the dictionary
function.  Rather, we want specialised versions of each method.
Thus we should generate something like this:

  $dfIx :: (Ix a, Ix x) => Ix (a,b)
  {- DFUN [$crange, ...] -}
  $dfIx da db = Ix ($crange da db) (...other methods...)

  $dfIxPair :: (Ix a, Ix x) => Ix (a,b)
  {- DFUN [$crangePair, ...] -}
  $dfIxPair = Ix ($crangePair da db) (...other methods...)

  $crange :: (Ix a, Ix b) -> ((a,b),(a,b)) -> [(a,b)]
  {-# SPECIALISE $crange :: ((Int,Int),(Int,Int)) -> [(Int,Int)] #-}
  $crange da db = <blah>

  {-# RULE  range ($dfIx da db) = $crange da db #-}

Note that  

  * The RULE is unaffected by the specialisation.  We don't want to
    specialise $dfIx, because then it would need a specialised RULE
    which is a pain.  The single RULE works fine at all specialisations.
    See Note [How instance declarations are translated] above

  * Instead, we want to specialise the *method*, $crange

In practice, rather than faking up a SPECIALISE pragama for each
method (which is painful, since we'd have to figure out its
specialised type), we call tcSpecPrag *as if* were going to specialise
$dfIx -- you can see that in the call to tcSpecInst.  That generates a
SpecPrag which, as it turns out, can be used unchanged for each method.
The "it turns out" bit is delicate, but it works fine!

\begin{code}
tcSpecInst :: Id -> Sig Name -> TcM TcSpecPrag
tcSpecInst dfun_id prag@(SpecInstSig hs_ty) 
  = addErrCtxt (spec_ctxt prag) $
    do  { let name = idName dfun_id
        ; (tyvars, theta, tau) <- tcHsInstHead hs_ty	
        ; let spec_ty = mkSigmaTy tyvars theta tau
        ; co_fn <- tcSubType (SpecPragOrigin name) (SigSkol SpecInstCtxt) 
                             (idType dfun_id) spec_ty
        ; return (SpecPrag dfun_id co_fn defaultInlinePragma) }
  where
    spec_ctxt prag = hang (ptext (sLit "In the SPECIALISE pragma")) 2 (ppr prag)

tcSpecInst _  _ = panic "tcSpecInst"
\end{code}

%************************************************************************
%*                                                                      *
      Type-checking an instance method
%*                                                                      *
%************************************************************************

tcInstanceMethod
- Make the method bindings, as a [(NonRec, HsBinds)], one per method
- Remembering to use fresh Name (the instance method Name) as the binder
- Bring the instance method Ids into scope, for the benefit of tcInstSig
- Use sig_fn mapping instance method Name -> instance tyvars
- Ditto prag_fn
- Use tcValBinds to do the checking

\begin{code}
tcInstanceMethods :: DFunId -> Class -> [TcTyVar]
                  -> [EvVar]
	 	  -> [TcType]
		  -> EvBind               -- "This" and its binding
		  -> ([Located TcSpecPrag], PragFun)
	  	  -> [(Id, DefMeth)]
                  -> InstBindings Name 
          	  -> TcM ([Id], [LHsBind Id])
	-- The returned inst_meth_ids all have types starting
	--	forall tvs. theta => ...
tcInstanceMethods dfun_id clas tyvars dfun_ev_vars inst_tys 
		  self_dict_ev (spec_inst_prags, prag_fn)
                  op_items (VanillaInst binds _ standalone_deriv)
  = mapAndUnzipM tc_item op_items
  where
    ----------------------
    tc_item :: (Id, DefMeth) -> TcM (Id, LHsBind Id)
    tc_item (sel_id, dm_info)
      = case findMethodBind (idName sel_id) binds of
  	    Just user_bind -> tc_body sel_id standalone_deriv user_bind
  	    Nothing	   -> tc_default sel_id dm_info

    ----------------------
    tc_body :: Id -> Bool -> LHsBind Name -> TcM (TcId, LHsBind Id)
    tc_body sel_id generated_code rn_bind 
      = add_meth_ctxt sel_id generated_code rn_bind $
        do { (meth_id, local_meth_id) <- mkMethIds clas tyvars dfun_ev_vars 
                                                   inst_tys sel_id
           ; let prags = prag_fn (idName sel_id)
           ; meth_id1 <- addInlinePrags meth_id prags
           ; spec_prags <- tcSpecPrags meth_id1 prags
           ; bind <- tcInstanceMethodBody InstSkol
                          tyvars dfun_ev_vars mb_dict_ev
                          meth_id1 local_meth_id meth_sig_fn 
                          (mk_meth_spec_prags meth_id1 spec_prags)
                          rn_bind 
           ; return (meth_id1, bind) }

    ----------------------
    tc_default :: Id -> DefMeth -> TcM (TcId, LHsBind Id)
    tc_default sel_id GenDefMeth    -- Derivable type classes stuff
      = do { meth_bind <- mkGenericDefMethBind clas inst_tys sel_id
           ; tc_body sel_id False {- Not generated code? -} meth_bind }
    	  
    tc_default sel_id NoDefMeth	    -- No default method at all
      = do { warnMissingMethod sel_id
    	   ; (meth_id, _) <- mkMethIds clas tyvars dfun_ev_vars 
                                         inst_tys sel_id
           ; return (meth_id, mkVarBind meth_id $ 
                              mkLHsWrap lam_wrapper error_rhs) }
      where
    	error_rhs    = L loc $ HsApp error_fun error_msg
    	error_fun    = L loc $ wrapId (WpTyApp meth_tau) nO_METHOD_BINDING_ERROR_ID
    	error_msg    = L loc (HsLit (HsStringPrim (mkFastString error_string)))
    	meth_tau     = funResultTy (applyTys (idType sel_id) inst_tys)
    	error_string = showSDoc (hcat [ppr loc, text "|", ppr sel_id ])
        lam_wrapper  = mkWpTyLams tyvars <.> mkWpLams dfun_ev_vars

    tc_default sel_id (DefMeth dm_name)	-- A polymorphic default method
      = do {   -- Build the typechecked version directly, 
    		 -- without calling typecheck_method; 
    		 -- see Note [Default methods in instances]
    		 -- Generate   /\as.\ds. let this = df as ds 
               --                      in $dm inst_tys this
    		 -- The 'let' is necessary only because HsSyn doesn't allow
    		 -- you to apply a function to a dictionary *expression*.

           ; (meth_id, local_meth_id) <- mkMethIds clas tyvars dfun_ev_vars 
                                                   inst_tys sel_id
           ; dm_id <- tcLookupId dm_name
           ; let dm_inline_prag = idInlinePragma dm_id
                 EvBind self_dict _ = self_dict_ev
                 rhs = HsWrap (mkWpEvVarApps [self_dict] <.> mkWpTyApps inst_tys) $
    		         HsVar dm_id 

    	         meth_bind = L loc $ VarBind { var_id = local_meth_id
                                             , var_rhs = L loc rhs 
    					       , var_inline = False }
                 meth_id1 = meth_id `setInlinePragma` dm_inline_prag
    		   	    -- Copy the inline pragma (if any) from the default
    			    -- method to this version. Note [INLINE and default methods]
    			    
                 bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars =  dfun_ev_vars
                                 , abs_exports = [( tyvars, meth_id1, local_meth_id
                                                  , mk_meth_spec_prags meth_id1 [])]
                                 , abs_ev_binds = EvBinds (unitBag self_dict_ev)
                                 , abs_binds    = unitBag meth_bind }
    	     -- Default methods in an instance declaration can't have their own 
    	     -- INLINE or SPECIALISE pragmas. It'd be possible to allow them, but
    	     -- currently they are rejected with 
    	     --		  "INLINE pragma lacks an accompanying binding"

           ; return (meth_id1, L loc bind) } 

    ----------------------
    mk_meth_spec_prags :: Id -> [LTcSpecPrag] -> TcSpecPrags
	-- Adapt the SPECIALISE pragmas to work for this method Id
        -- There are two sources: 
        --   * spec_inst_prags: {-# SPECIALISE instance :: <blah> #-}
        --     These ones have the dfun inside, but [perhaps surprisingly] 
        --     the correct wrapper
        --   * spec_prags_for_me: {-# SPECIALISE op :: <blah> #-}
    mk_meth_spec_prags meth_id spec_prags_for_me
      = SpecPrags (spec_prags_for_me ++ 
                   [ L loc (SpecPrag meth_id wrap inl)
        	   | L loc (SpecPrag _ wrap inl) <- spec_inst_prags])
   
    loc = getSrcSpan dfun_id
    meth_sig_fn _ = Just ([],loc)	-- The 'Just' says "yes, there's a type sig"
	-- But there are no scoped type variables from local_method_id
	-- Only the ones from the instance decl itself, which are already
	-- in scope.  Example:
	--	class C a where { op :: forall b. Eq b => ... }
	-- 	instance C [c] where { op = <rhs> }
	-- In <rhs>, 'c' is scope but 'b' is not!

    mb_dict_ev = if null tyvars then Nothing else Just self_dict_ev
		-- Only need the self_dict stuff if there are type 
		-- variables involved; otherwise overlap is not possible
		-- See Note [Subtle interaction of recursion and overlap]
		-- in TcInstDcls

	-- For instance decls that come from standalone deriving clauses
	-- we want to print out the full source code if there's an error
	-- because otherwise the user won't see the code at all
    add_meth_ctxt sel_id generated_code rn_bind thing 
      | generated_code = addLandmarkErrCtxt (derivBindCtxt sel_id clas inst_tys rn_bind) thing
      | otherwise      = thing


tcInstanceMethods dfun_id clas tyvars dfun_ev_vars inst_tys 
		  _ _ op_items (NewTypeDerived coi _)

-- Running example:
--   class Show b => Foo a b where
--     op :: a -> b -> b
--   newtype N a = MkN (Tree [a]) 
--   deriving instance (Show p, Foo Int p) => Foo Int (N p)
--		 -- NB: standalone deriving clause means
--		 --     that the contex is user-specified
-- Hence op :: forall a b. Foo a b => a -> b -> b
--
-- We're going to make an instance like
--   instance (Show p, Foo Int p) => Foo Int (N p)
--      op = $copT
--
--   $copT :: forall p. (Show p, Foo Int p) => Int -> N p -> N p
--   $copT p (d1:Show p) (d2:Foo Int p) 
--     = op Int (Tree [p]) rep_d |> op_co
--     where 
--       rep_d :: Foo Int (Tree [p]) = ...d1...d2...
--       op_co :: (Int -> Tree [p] -> Tree [p]) ~ (Int -> T p -> T p)
-- We get op_co by substituting [Int/a] and [co/b] in type for op
-- where co : [p] ~ T p
--
-- Notice that the dictionary bindings "..d1..d2.." must be generated
-- by the constraint solver, since the <context> may be
-- user-specified.

  = do { rep_d_stuff <- checkConstraints InstSkol tyvars dfun_ev_vars $
                        emitWanted ScOrigin rep_pred
                         
       ; mapAndUnzipM (tc_item rep_d_stuff) op_items }
  where
     loc = getSrcSpan dfun_id

     inst_tvs = fst (tcSplitForAllTys (idType dfun_id))
     Just (init_inst_tys, _) = snocView inst_tys
     rep_ty   = fst (coercionKind co)  -- [p]
     rep_pred = mkClassPred clas (init_inst_tys ++ [rep_ty])

     -- co : [p] ~ T p
     co = substTyWith inst_tvs (mkTyVarTys tyvars) $
          case coi of { IdCo ty -> ty ;
                        ACo co  -> mkSymCoercion co }

     ----------------
     tc_item :: (TcEvBinds, EvVar) -> (Id, DefMeth) -> TcM (TcId, LHsBind TcId)
     tc_item (rep_ev_binds, rep_d) (sel_id, _)
       = do { (meth_id, local_meth_id) <- mkMethIds clas tyvars dfun_ev_vars 
                                                    inst_tys sel_id

            ; let meth_rhs  = wrapId (mk_op_wrapper sel_id rep_d) sel_id
                  meth_bind = VarBind { var_id = local_meth_id
                                      , var_rhs = L loc meth_rhs
    				      , var_inline = False }

	          bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars = dfun_ev_vars
                                   , abs_exports = [(tyvars, meth_id, 
                                                     local_meth_id, noSpecPrags)]
				   , abs_ev_binds = rep_ev_binds
                                   , abs_binds = unitBag $ L loc meth_bind }

            ; return (meth_id, L loc bind) }

     ----------------
     mk_op_wrapper :: Id -> EvVar -> HsWrapper
     mk_op_wrapper sel_id rep_d 
       = WpCast (substTyWith sel_tvs (init_inst_tys ++ [co]) local_meth_ty)
         <.> WpEvApp (EvId rep_d)
         <.> mkWpTyApps (init_inst_tys ++ [rep_ty]) 
       where
         (sel_tvs, sel_rho) = tcSplitForAllTys (idType sel_id)
         (_, local_meth_ty) = tcSplitPredFunTy_maybe sel_rho
                              `orElse` pprPanic "tcInstanceMethods" (ppr sel_id)

----------------------
mkMethIds :: Class -> [TcTyVar] -> [EvVar] -> [TcType] -> Id -> TcM (TcId, TcId)
mkMethIds clas tyvars dfun_ev_vars inst_tys sel_id
  = do  { uniq <- newUnique
  	; let meth_name = mkDerivedInternalName mkClassOpAuxOcc uniq sel_name
  	; local_meth_name <- newLocalName sel_name
  		  -- Base the local_meth_name on the selector name, becuase
  		  -- type errors from tcInstanceMethodBody come from here

  	; let meth_id       = mkLocalId meth_name meth_ty
  	      local_meth_id = mkLocalId local_meth_name local_meth_ty
        ; return (meth_id, local_meth_id) }
  where
    local_meth_ty = instantiateMethod clas sel_id inst_tys
    meth_ty = mkForAllTys tyvars $ mkPiTypes dfun_ev_vars local_meth_ty
    sel_name = idName sel_id

----------------------
wrapId :: HsWrapper -> id -> HsExpr id
wrapId wrapper id = mkHsWrap wrapper (HsVar id)

derivBindCtxt :: Id -> Class -> [Type ] -> LHsBind Name -> SDoc
derivBindCtxt sel_id clas tys _bind
   = vcat [ ptext (sLit "When typechecking the code for ") <+> quotes (ppr sel_id)
          , nest 2 (ptext (sLit "in a standalone derived instance for")
	  	    <+> quotes (pprClassPred clas tys) <> colon)
          , nest 2 $ ptext (sLit "To see the code I am typechecking, use -ddump-deriv") ]

-- Too voluminous
--	  , nest 2 $ pprSetDepth AllTheWay $ ppr bind ]

warnMissingMethod :: Id -> TcM ()
warnMissingMethod sel_id
  = do { warn <- doptM Opt_WarnMissingMethods		
       ; warnTc (warn  -- Warn only if -fwarn-missing-methods
                 && not (startsWithUnderscore (getOccName sel_id)))
					-- Don't warn about _foo methods
		(ptext (sLit "No explicit method nor default method for")
                 <+> quotes (ppr sel_id)) }
\end{code}

Note [Export helper functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange to export the "helper functions" of an instance declaration,
so that they are not subject to preInlineUnconditionally, even if their
RHS is trivial.  Reason: they are mentioned in the DFunUnfolding of
the dict fun as Ids, not as CoreExprs, so we can't substitute a 
non-variable for them.

We could change this by making DFunUnfoldings have CoreExprs, but it
seems a bit simpler this way.

Note [Default methods in instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this

   class Baz v x where
      foo :: x -> x
      foo y = <blah>

   instance Baz Int Int

From the class decl we get

   $dmfoo :: forall v x. Baz v x => x -> x
   $dmfoo y = <blah>

Notice that the type is ambiguous.  That's fine, though. The instance
decl generates

   $dBazIntInt = MkBaz fooIntInt
   fooIntInt = $dmfoo Int Int $dBazIntInt

BUT this does mean we must generate the dictionary translation of
fooIntInt directly, rather than generating source-code and
type-checking it.  That was the bug in Trac #1061. In any case it's
less work to generate the translated version!

Note [INLINE and default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Default methods need special case.  They are supposed to behave rather like
macros.  For exmample

  class Foo a where
    op1, op2 :: Bool -> a -> a

    {-# INLINE op1 #-}
    op1 b x = op2 (not b) x

  instance Foo Int where
    -- op1 via default method
    op2 b x = <blah>
   
The instance declaration should behave

   just as if 'op1' had been defined with the
   code, and INLINE pragma, from its original
   definition. 

That is, just as if you'd written

  instance Foo Int where
    op2 b x = <blah>

    {-# INLINE op1 #-}
    op1 b x = op2 (not b) x

So for the above example we generate:


  {-# INLINE $dmop1 #-}
  -- $dmop1 has an InlineCompulsory unfolding
  $dmop1 d b x = op2 d (not b) x

  $fFooInt = MkD $cop1 $cop2

  {-# INLINE $cop1 #-}
  $cop1 = $dmop1 $fFooInt

  $cop2 = <blah>

Note carefullly:

* We *copy* any INLINE pragma from the default method $dmop1 to the
  instance $cop1.  Otherwise we'll just inline the former in the
  latter and stop, which isn't what the user expected

* Regardless of its pragma, we give the default method an 
  unfolding with an InlineCompulsory source. That means
  that it'll be inlined at every use site, notably in
  each instance declaration, such as $cop1.  This inlining
  must happen even though 
    a) $dmop1 is not saturated in $cop1
    b) $cop1 itself has an INLINE pragma

  It's vital that $dmop1 *is* inlined in this way, to allow the mutual
  recursion between $fooInt and $cop1 to be broken

* To communicate the need for an InlineCompulsory to the desugarer
  (which makes the Unfoldings), we use the IsDefaultMethod constructor
  in TcSpecPrags.


%************************************************************************
%*                                                                      *
\subsection{Error messages}
%*                                                                      *
%************************************************************************

\begin{code}
instDeclCtxt1 :: LHsType Name -> SDoc
instDeclCtxt1 hs_inst_ty
  = inst_decl_ctxt (case unLoc hs_inst_ty of
                        HsForAllTy _ _ _ (L _ (HsPredTy pred)) -> ppr pred
                        HsPredTy pred                    -> ppr pred
                        _                                -> ppr hs_inst_ty)     -- Don't expect this
instDeclCtxt2 :: Type -> SDoc
instDeclCtxt2 dfun_ty
  = inst_decl_ctxt (ppr (mkClassPred cls tys))
  where
    (_,cls,tys) = tcSplitDFunTy dfun_ty

inst_decl_ctxt :: SDoc -> SDoc
inst_decl_ctxt doc = ptext (sLit "In the instance declaration for") <+> quotes doc

atInstCtxt :: Name -> SDoc
atInstCtxt name = ptext (sLit "In the associated type instance for") <+>
                  quotes (ppr name)

mustBeVarArgErr :: Type -> SDoc
mustBeVarArgErr ty =
  sep [ ptext (sLit "Arguments that do not correspond to a class parameter") <+>
        ptext (sLit "must be variables")
      , ptext (sLit "Instead of a variable, found") <+> ppr ty
      ]

wrongATArgErr :: Type -> Type -> SDoc
wrongATArgErr ty instTy =
  sep [ ptext (sLit "Type indexes must match class instance head")
      , ptext (sLit "Found") <+> quotes (ppr ty)
        <+> ptext (sLit "but expected") <+> quotes (ppr instTy)
      ]
\end{code}
