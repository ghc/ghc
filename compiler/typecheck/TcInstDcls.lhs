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
import TcRnMonad
import TcMType
import TcType
import Inst
import InstEnv
import FamInst
import FamInstEnv
import TcDeriv
import TcEnv
import RnEnv	( lookupImportedName )
import TcHsType
import TcUnify
import TcSimplify
import Type
import Coercion
import TyCon
import TypeRep
import DataCon
import Class
import Var
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
	{-# INLINE df_i #-}	-- Always inline dictionary functions
	df_i :: forall a. C a -> C [a]
	df_i = /\a. \d:C a. letrec d' = MkC (op1_i  a   d)
                                            ($dmop2 [a] d')
	       	    	    in d'
		-- But see Note [Default methods in instances]
		-- We can't apply the type checker to the default-method call

* The dictionary function itself is inlined as vigorously as we
  possibly can, so that we expose that dictionary constructor to
  selectors as much as poss.  That is why the op_i stuff is in 
  *separate* bindings, so that the df_i binding is small enough
  to inline.  See Note [Inline dfuns unconditionally].

* Note that df_i may be mutually recursive with both op1_i and op2_i.
  It's crucial that df_i is not chosen as the loop breaker, even 
  though op1_i has a (user-specified) INLINE pragma.
  Not even once!  Else op1_i, op2_i may be inlined into df_i.

* Instead the idea is to inline df_i into op1_i, which may then select
  methods from the MkC record, and thereby break the recursion with
  df_i, leaving a *self*-recurisve op1_i.  (If op1_i doesn't call op at
  the same type, it won't mention df_i, so there won't be recursion in
  the first place.)  

* If op1_i is marked INLINE by the user there's a danger that we won't
  inline df_i in it, and that in turn means that (since it'll be a
  loop-breaker because df_i isn't), op1_i will ironically never be 
  inlined.  We need to fix this somehow -- perhaps allowing inlining
  of INLINE funcitons inside other INLINE functions.

Note [Subtle interaction of recursion and overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
  class C a where { op1,op2 :: a -> a }
  instance C a => C [a] where
    op1 x = op2 x ++ op2 x
    op2 x = ...
  intance C [Int] where
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
are some type variales involved; otherwise there can be no overlap and
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

Note [Inline dfuns unconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The code above unconditionally inlines dict funs.  Here's why.
Consider this program:

    test :: Int -> Int -> Bool
    test x y = (x,y) == (y,x) || test y x
    -- Recursive to avoid making it inline.

This needs the (Eq (Int,Int)) instance.  If we inline that dfun
the code we end up with is good:

    Test.$wtest =
        \r -> case ==# [ww ww1] of wild {
                PrelBase.False -> Test.$wtest ww1 ww;
                PrelBase.True ->
                  case ==# [ww1 ww] of wild1 {
                    PrelBase.False -> Test.$wtest ww1 ww;
                    PrelBase.True -> PrelBase.True [];
                  };
            };
    Test.test = \r [w w1]
            case w of w2 {
              PrelBase.I# ww ->
                  case w1 of w3 { PrelBase.I# ww1 -> Test.$wtest ww ww1; };
            };

If we don't inline the dfun, the code is not nearly as good:

    (==) = case PrelTup.$fEq(,) PrelBase.$fEqInt PrelBase.$fEqInt of tpl {
              PrelBase.:DEq tpl1 tpl2 -> tpl2;
            };

    Test.$wtest =
        \r [ww ww1]
            let { y = PrelBase.I#! [ww1]; } in
            let { x = PrelBase.I#! [ww]; } in
            let { sat_slx = PrelTup.(,)! [y x]; } in
            let { sat_sly = PrelTup.(,)! [x y];
            } in
              case == sat_sly sat_slx of wild {
                PrelBase.False -> Test.$wtest ww1 ww;
                PrelBase.True -> PrelBase.True [];
              };

    Test.test =
        \r [w w1]
            case w of w2 {
              PrelBase.I# ww ->
                  case w1 of w3 { PrelBase.I# ww1 -> Test.$wtest ww ww1; };
            };

Why didn't GHC inline $fEq in those days?  Because it looked big:

    PrelTup.zdfEqZ1T{-rcX-}
        = \ @ a{-reT-} :: * @ b{-reS-} :: *
            zddEq{-rf6-} _Ks :: {PrelBase.Eq{-23-} a{-reT-}}
            zddEq1{-rf7-} _Ks :: {PrelBase.Eq{-23-} b{-reS-}} ->
            let {
              zeze{-rf0-} _Kl :: (b{-reS-} -> b{-reS-} -> PrelBase.Bool{-3c-})
              zeze{-rf0-} = PrelBase.zeze{-01L-}@ b{-reS-} zddEq1{-rf7-} } in
            let {
              zeze1{-rf3-} _Kl :: (a{-reT-} -> a{-reT-} -> PrelBase.Bool{-3c-})
              zeze1{-rf3-} = PrelBase.zeze{-01L-} @ a{-reT-} zddEq{-rf6-} } in
            let {
              zeze2{-reN-} :: ((a{-reT-}, b{-reS-}) -> (a{-reT-}, b{-reS-})-> PrelBase.Bool{-3c-})
              zeze2{-reN-} = \ ds{-rf5-} _Ks :: (a{-reT-}, b{-reS-})
                               ds1{-rf4-} _Ks :: (a{-reT-}, b{-reS-}) ->
                             case ds{-rf5-}
                             of wild{-reW-} _Kd { (a1{-rf2-} _Ks, a2{-reZ-} _Ks) ->
                             case ds1{-rf4-}
                             of wild1{-reX-} _Kd { (b1{-rf1-} _Ks, b2{-reY-} _Ks) ->
                             PrelBase.zaza{-r4e-}
                               (zeze1{-rf3-} a1{-rf2-} b1{-rf1-})
                               (zeze{-rf0-} a2{-reZ-} b2{-reY-})
                             }
                             } } in
            let {
              a1{-reR-} :: ((a{-reT-}, b{-reS-})-> (a{-reT-}, b{-reS-})-> PrelBase.Bool{-3c-})
              a1{-reR-} = \ a2{-reV-} _Ks :: (a{-reT-}, b{-reS-})
                            b1{-reU-} _Ks :: (a{-reT-}, b{-reS-}) ->
                          PrelBase.not{-r6I-} (zeze2{-reN-} a2{-reV-} b1{-reU-})
            } in
              PrelBase.zdwZCDEq{-r8J-} @ (a{-reT-}, b{-reS-}) a1{-reR-} zeze2{-reN-})

and it's not as bad as it seems, because it's further dramatically
simplified: only zeze2 is extracted and its body is simplified.


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
       ; let { idxty_decls = filter (isFamInstDecl . unLoc) tycl_decls }
       ; local_info_tycons <- mapAndRecoverM tcLocalInstDecl1  inst_decls
       ; idx_tycons        <- mapAndRecoverM tcIdxTyInstDeclTL idxty_decls

       ; let { (local_info,
                at_tycons_s)   = unzip local_info_tycons
             ; at_idx_tycon    = concat at_tycons_s ++ idx_tycons
             ; clas_decls      = filter (isClassDecl.unLoc) tycl_decls
             ; implicit_things = concatMap implicitTyThings at_idx_tycon
             }

                -- (2) Add the tycons of indexed types and their implicit
                --     tythings to the global environment
       ; tcExtendGlobalEnv (at_idx_tycon ++ implicit_things) $ do {

                -- (3) Instances from generic class declarations
       ; generic_inst_info <- getGenericInstances clas_decls

                -- Next, construct the instance environment so far, consisting
                -- of
                --   a) local instance decls
                --   b) generic instances
                --   c) local family instance decls
       ; addInsts local_info         $ do {
       ; addInsts generic_inst_info  $ do {
       ; addFamInsts at_idx_tycon    $ do {

                -- (4) Compute instances from "deriving" clauses;
                -- This stuff computes a context for the derived instance
                -- decl, so it needs to know about all the instances possible
                -- NB: class instance declarations can contain derivings as
                --     part of associated data type declarations
	 failIfErrsM		-- If the addInsts stuff gave any errors, don't
				-- try the deriving stuff, becuase that may give
				-- more errors still
       ; (deriv_inst_info, deriv_binds) <- tcDeriving tycl_decls inst_decls
                                                      deriv_decls
       ; addInsts deriv_inst_info   $ do {

       ; gbl_env <- getGblEnv
       ; return (gbl_env,
                  generic_inst_info ++ deriv_inst_info ++ local_info,
                  deriv_binds)
    }}}}}}
  where
    -- Make sure that toplevel type instance are not for associated types.
    -- !!!TODO: Need to perform this check for the TyThing of type functions,
    --          too.
    tcIdxTyInstDeclTL ldecl@(L loc decl) =
      do { tything <- tcFamInstDecl ldecl
         ; setSrcSpan loc $
             when (isAssocFamily tything) $
               addErr $ assocInClassErr (tcdName decl)
         ; return tything
         }
    isAssocFamily (ATyCon tycon) =
      case tyConFamInst_maybe tycon of
        Nothing       -> panic "isAssocFamily: no family?!?"
        Just (fam, _) -> isTyConAssoc fam
    isAssocFamily _ = panic "isAssocFamily: no tycon?!?"

assocInClassErr :: Name -> SDoc
assocInClassErr name =
  ptext (sLit "Associated type") <+> quotes (ppr name) <+>
  ptext (sLit "must be inside a class instance")

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
        ; (clas, inst_tys) <- checkValidInstHead tau
        ; checkValidInstance tyvars theta clas inst_tys

        -- Next, process any associated types.
        ; idx_tycons <- recoverM (return []) $
	  	     do { idx_tycons <- checkNoErrs $ mapAndRecoverM tcFamInstDecl ats
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
                              iBinds = VanillaInst binds uprags },
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

    checkIndexes clas inst_tys (hsAT, ATyCon tycon) =
-- !!!TODO: check that this does the Right Thing for indexed synonyms, too!
      checkIndexes' clas inst_tys hsAT
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
          Just atDecl ->
            case assocTyConArgPoss_maybe atDecl of
              Nothing   -> panic "checkIndexes': AT has no args poss?!?"
              Just poss ->

                -- The following is tricky!  We need to deal with three
                -- complications: (1) The AT possibly only uses a subset of
                -- the class parameters as indexes and those it uses may be in
                -- a different order; (2) the AT may have extra arguments,
                -- which must be type variables; and (3) variables in AT and
                -- instance head will be different `Name's even if their
                -- source lexemes are identical.
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
                let relevantInstTys = map (instTys !!) poss
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
             -> TcM (LHsBinds Id, TcLclEnv)
-- (a) From each class declaration,
--      generate any default-method bindings
-- (b) From each instance decl
--      generate the dfun binding

tcInstDecls2 tycl_decls inst_decls
  = do  { -- (a) Default methods from class decls
          (dm_binds_s, dm_ids_s) <- mapAndUnzipM tcClassDecl2 $
                                    filter (isClassDecl.unLoc) tycl_decls
        ; tcExtendIdEnv (concat dm_ids_s) $ do

          -- (b) instance declarations
        ; inst_binds_s <- mapM tcInstDecl2 inst_decls

          -- Done
        ; let binds = unionManyBags dm_binds_s `unionBags`
                      unionManyBags inst_binds_s
        ; tcl_env <- getLclEnv -- Default method Ids in here
        ; return (binds, tcl_env) }
\end{code}


\begin{code}
tcInstDecl2 :: InstInfo Name -> TcM (LHsBinds Id)
-- Returns a binding for the dfun

------------------------
-- Derived newtype instances; surprisingly tricky!
--
--      class Show a => Foo a b where ...
--      newtype N a = MkN (Tree [a]) deriving( Foo Int )
--
-- The newtype gives an FC axiom looking like
--      axiom CoN a ::  N a :=: Tree [a]
--   (see Note [Newtype coercions] in TyCon for this unusual form of axiom)
--
-- So all need is to generate a binding looking like:
--      dfunFooT :: forall a. (Foo Int (Tree [a], Show (N a)) => Foo Int (N a)
--      dfunFooT = /\a. \(ds:Show (N a)) (df:Foo (Tree [a])).
--                case df `cast` (Foo Int (sym (CoN a))) of
--                   Foo _ op1 .. opn -> Foo ds op1 .. opn
--
-- If there are no superclasses, matters are simpler, because we don't need the case
-- see Note [Newtype deriving superclasses] in TcDeriv.lhs

tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = NewTypeDerived })
  = do  { let dfun_id      = instanceDFunId ispec
              rigid_info   = InstSkol
              origin       = SigOrigin rigid_info
              inst_ty      = idType dfun_id
        ; (inst_tvs', theta, inst_head_ty) <- tcSkolSigType rigid_info inst_ty
                -- inst_head_ty is a PredType

        ; let (cls, cls_inst_tys) = tcSplitDFunHead inst_head_ty
              (class_tyvars, sc_theta, _, _) = classBigSig cls
              cls_tycon = classTyCon cls
              sc_theta' = substTheta (zipOpenTvSubst class_tyvars cls_inst_tys) sc_theta

              Just (initial_cls_inst_tys, last_ty) = snocView cls_inst_tys
              (nt_tycon, tc_args) = tcSplitTyConApp last_ty     -- Can't fail
              rep_ty              = newTyConInstRhs nt_tycon tc_args

              rep_pred     = mkClassPred cls (initial_cls_inst_tys ++ [rep_ty])
                                -- In our example, rep_pred is (Foo Int (Tree [a]))
              the_coercion = make_coercion cls_tycon initial_cls_inst_tys nt_tycon tc_args
                                -- Coercion of kind (Foo Int (Tree [a]) ~ Foo Int (N a)

        ; sc_loc     <- getInstLoc InstScOrigin
        ; sc_dicts   <- newDictBndrs sc_loc sc_theta'
        ; inst_loc   <- getInstLoc origin
        ; dfun_dicts <- newDictBndrs inst_loc theta
        ; this_dict  <- newDictBndr inst_loc (mkClassPred cls cls_inst_tys)
        ; rep_dict   <- newDictBndr inst_loc rep_pred

        -- Figure out bindings for the superclass context from dfun_dicts
        -- Don't include this_dict in the 'givens', else
        -- sc_dicts get bound by just selecting from this_dict!!
        ; sc_binds <- addErrCtxt superClassCtxt $
                      tcSimplifySuperClasses inst_loc this_dict dfun_dicts 
					     (rep_dict:sc_dicts)

	-- It's possible that the superclass stuff might unified something
	-- in the envt with one of the clas_tyvars
	; checkSigTyVars inst_tvs'

        ; let coerced_rep_dict = wrapId the_coercion (instToId rep_dict)

        ; body <- make_body cls_tycon cls_inst_tys sc_dicts coerced_rep_dict
        ; let dict_bind = noLoc $ VarBind (instToId this_dict) (noLoc body)

        ; return (unitBag $ noLoc $
                  AbsBinds inst_tvs' (map instToVar dfun_dicts)
                            [(inst_tvs', dfun_id, instToId this_dict, [])]
                            (dict_bind `consBag` sc_binds)) }
  where
      -----------------------
      --        make_coercion
      -- The inst_head looks like (C s1 .. sm (T a1 .. ak))
      -- But we want the coercion (C s1 .. sm (sym (CoT a1 .. ak)))
      --        with kind (C s1 .. sm (T a1 .. ak)  :=:  C s1 .. sm <rep_ty>)
      --        where rep_ty is the (eta-reduced) type rep of T
      -- So we just replace T with CoT, and insert a 'sym'
      -- NB: we know that k will be >= arity of CoT, because the latter fully eta-reduced

    make_coercion cls_tycon initial_cls_inst_tys nt_tycon tc_args
        | Just co_con <- newTyConCo_maybe nt_tycon
        , let co = mkSymCoercion (mkTyConApp co_con tc_args)
        = WpCast (mkTyConApp cls_tycon (initial_cls_inst_tys ++ [co]))
        | otherwise     -- The newtype is transparent; no need for a cast
        = idHsWrapper

      -----------------------
      --     (make_body C tys scs coreced_rep_dict)
      --                returns
      --     (case coerced_rep_dict of { C _ ops -> C scs ops })
      -- But if there are no superclasses, it returns just coerced_rep_dict
      -- See Note [Newtype deriving superclasses] in TcDeriv.lhs

    make_body cls_tycon cls_inst_tys sc_dicts coerced_rep_dict
        | null sc_dicts         -- Case (a)
        = return coerced_rep_dict
        | otherwise             -- Case (b)
        = do { op_ids            <- newSysLocalIds (fsLit "op") op_tys
             ; dummy_sc_dict_ids <- newSysLocalIds (fsLit "sc") (map idType sc_dict_ids)
             ; let the_pat = ConPatOut { pat_con = noLoc cls_data_con, pat_tvs = [],
                                         pat_dicts = dummy_sc_dict_ids,
                                         pat_binds = emptyLHsBinds,
                                         pat_args = PrefixCon (map nlVarPat op_ids),
                                         pat_ty = pat_ty}
                   the_match = mkSimpleMatch [noLoc the_pat] the_rhs
                   the_rhs = mkHsConApp cls_data_con cls_inst_tys $
                             map HsVar (sc_dict_ids ++ op_ids)

                -- Warning: this HsCase scrutinises a value with a PredTy, which is
                --          never otherwise seen in Haskell source code. It'd be
                --          nicer to generate Core directly!
             ; return (HsCase (noLoc coerced_rep_dict) $
                       MatchGroup [the_match] (mkFunTy pat_ty pat_ty)) }
        where
          sc_dict_ids  = map instToId sc_dicts
          pat_ty       = mkTyConApp cls_tycon cls_inst_tys
          cls_data_con = head (tyConDataCons cls_tycon)
          cls_arg_tys  = dataConInstArgTys cls_data_con cls_inst_tys
          op_tys       = dropList sc_dict_ids cls_arg_tys

------------------------
-- Ordinary instances

tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = VanillaInst monobinds uprags })
  = let
        dfun_id    = instanceDFunId ispec
        rigid_info = InstSkol
        inst_ty    = idType dfun_id
        loc        = getSrcSpan dfun_id
    in
         -- Prime error recovery
    recoverM (return emptyLHsBinds)             $
    setSrcSpan loc                              $
    addErrCtxt (instDeclCtxt2 (idType dfun_id)) $ do

        -- Instantiate the instance decl with skolem constants
    (inst_tyvars', dfun_theta', inst_head') <- tcSkolSigType rigid_info inst_ty
                -- These inst_tyvars' scope over the 'where' part
                -- Those tyvars are inside the dfun_id's type, which is a bit
                -- bizarre, but OK so long as you realise it!
    let
        (clas, inst_tys') = tcSplitDFunHead inst_head'
        (class_tyvars, sc_theta, _, op_items) = classBigSig clas

        -- Instantiate the super-class context with inst_tys
        sc_theta' = substTheta (zipOpenTvSubst class_tyvars inst_tys') sc_theta
        origin    = SigOrigin rigid_info

         -- Create dictionary Ids from the specified instance contexts.
    sc_loc      <- getInstLoc InstScOrigin
    sc_dicts    <- newDictOccs sc_loc sc_theta'		-- These are wanted
    inst_loc    <- getInstLoc origin
    dfun_dicts  <- newDictBndrs inst_loc dfun_theta'	-- Includes equalities
    this_dict   <- newDictBndr inst_loc (mkClassPred clas inst_tys')
                -- Default-method Ids may be mentioned in synthesised RHSs,
                -- but they'll already be in the environment.

        -- Typecheck the methods
    let this_dict_id  	= instToId this_dict
	dfun_lam_vars   = map instToVar dfun_dicts	-- Includes equalities
	prag_fn	= mkPragFun uprags 
	tc_meth = tcInstanceMethod loc clas inst_tyvars'
			  	   dfun_dicts
                   	  	   dfun_theta' inst_tys'
				   this_dict dfun_id
                        	   prag_fn monobinds
    (meth_exprs, meth_binds) <- tcExtendTyVarEnv inst_tyvars'  $
				mapAndUnzipM tc_meth op_items 

    -- Figure out bindings for the superclass context
    -- Don't include this_dict in the 'givens', else
    -- sc_dicts get bound by just selecting  from this_dict!!
    sc_binds <- addErrCtxt superClassCtxt $
                tcSimplifySuperClasses inst_loc this_dict dfun_dicts sc_dicts
		-- Note [Recursive superclasses]

	-- It's possible that the superclass stuff might unified something
	-- in the envt with one of the inst_tyvars'
    checkSigTyVars inst_tyvars'

    -- Deal with 'SPECIALISE instance' pragmas
    prags <- tcPrags dfun_id (filter isSpecInstLSig uprags)

    -- Create the result bindings
    let
        dict_constr   = classDataCon clas
        inline_prag | null dfun_dicts  = []
                    | otherwise        = [L loc (InlinePrag (Inline AlwaysActive True))]
                -- Always inline the dfun; this is an experimental decision
                -- because it makes a big performance difference sometimes.
                -- Often it means we can do the method selection, and then
                -- inline the method as well.  Marcin's idea; see comments below.
                --
                -- BUT: don't inline it if it's a constant dictionary;
                -- we'll get all the benefit without inlining, and we get
                -- a **lot** of code duplication if we inline it
                --
                --      See Note [Inline dfuns] below

        sc_dict_vars  = map instToVar sc_dicts
        dict_bind     = L loc (VarBind this_dict_id dict_rhs)
        dict_rhs      = foldl (\ f a -> L loc (HsApp f (L loc a))) inst_constr meth_exprs
 	inst_constr   = L loc $ wrapId (mkWpApps sc_dict_vars <.> mkWpTyApps inst_tys')
				       (dataConWrapId dict_constr)
                -- We don't produce a binding for the dict_constr; instead we
                -- rely on the simplifier to unfold this saturated application
                -- We do this rather than generate an HsCon directly, because
                -- it means that the special cases (e.g. dictionary with only one
                -- member) are dealt with by the common MkId.mkDataConWrapId code rather
                -- than needing to be repeated here.


        main_bind = noLoc $ AbsBinds
                            inst_tyvars'
                            dfun_lam_vars
                            [(inst_tyvars', dfun_id, this_dict_id, inline_prag ++ prags)]
                            (dict_bind `consBag` sc_binds)

    showLIE (text "instance")
    return (main_bind `consBag` unionManyBags meth_binds)
\end{code}

Note [Recursive superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #1470 for why we would *like* to add "this_dict" to the 
available instances here.  But we can't do so because then the superclases
get satisfied by selection from this_dict, and that leads to an immediate
loop.  What we need is to add this_dict to Avails without adding its 
superclasses, and we currently have no way to do that.


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
tcInstanceMethod :: SrcSpan -> Class -> [TcTyVar] -> [Inst]
	 	 -> TcThetaType -> [TcType]
		 -> Inst -> Id
          	 -> TcPragFun -> LHsBinds Name 
	  	 -> (Id, DefMeth)
          	 -> TcM (HsExpr Id, LHsBinds Id)
	-- The returned inst_meth_ids all have types starting
	--	forall tvs. theta => ...

tcInstanceMethod loc clas tyvars dfun_dicts theta inst_tys 
		 this_dict dfun_id prag_fn binds_in (sel_id, dm_info)
  = do	{ cloned_this <- cloneDict this_dict
		-- Need to clone the dict in case it is floated out, and
		-- then clashes with its friends
	; uniq1 <- newUnique
	; let local_meth_name = mkInternalName uniq1 sel_occ loc   -- Same OccName
	      this_dict_bind  = L loc $ VarBind (instToId cloned_this) $ 
				L loc $ wrapId meth_wrapper dfun_id
	      mb_this_bind | null tyvars = Nothing
			   | otherwise   = Just (cloned_this, this_dict_bind)
		-- Only need the this_dict stuff if there are type variables
		-- involved; otherwise overlap is not possible
		-- See Note [Subtle interaction of recursion and overlap]	

	      tc_body rn_bind = do { (meth_id, tc_binds) <- tcInstanceMethodBody 
						InstSkol clas tyvars dfun_dicts theta inst_tys
						mb_this_bind sel_id 
						local_meth_name
						meth_sig_fn meth_prag_fn rn_bind
				   ; return (wrapId meth_wrapper meth_id, tc_binds) }

	; case (findMethodBind sel_name local_meth_name binds_in, dm_info) of
		-- There is a user-supplied method binding, so use it
	    (Just user_bind, _) -> tc_body user_bind

		-- The user didn't supply a method binding, so we have to make 
		-- up a default binding, in a way depending on the default-method info

	    (Nothing, GenDefMeth) -> do		-- Derivable type classes stuff
			{ meth_bind <- mkGenericDefMethBind clas inst_tys sel_id local_meth_name
			; tc_body meth_bind }

	    (Nothing, NoDefMeth) -> do		-- No default method in the class
			{ warn <- doptM Opt_WarnMissingMethods		
                        ; warnTc (warn  -- Warn only if -fwarn-missing-methods
				  && reportIfUnused (getOccName sel_id))
					-- Don't warn about _foo methods
			         omitted_meth_warn
			; return (error_rhs, emptyBag) }

	    (Nothing, DefMeth) -> do	-- An polymorphic default method
			{   -- Build the typechecked version directly, 
			    -- without calling typecheck_method; 
			    -- see Note [Default methods in instances]
			  dm_name <- lookupImportedName (mkDefMethRdrName sel_name)
					-- Might not be imported, but will be an OrigName
			; dm_id   <- tcLookupId dm_name
			; return (wrapId dm_wrapper dm_id, emptyBag) } }
  where
    sel_name = idName sel_id
    sel_occ  = nameOccName sel_name
    this_dict_id = instToId this_dict

    meth_prag_fn _ = prag_fn sel_name
    meth_sig_fn _  = Just []	-- The 'Just' says "yes, there's a type sig"
			-- But there are no scoped type variables from local_method_id
			-- Only the ones from the instance decl itself, which are already
			-- in scope.  Example:
			--	class C a where { op :: forall b. Eq b => ... }
			-- 	instance C [c] where { op = <rhs> }
			-- In <rhs>, 'c' is scope but 'b' is not!

    error_rhs    = HsApp error_fun error_msg
    error_fun    = L loc $ wrapId (WpTyApp meth_tau) nO_METHOD_BINDING_ERROR_ID
    error_msg    = L loc (HsLit (HsStringPrim (mkFastString error_string)))
    meth_tau     = funResultTy (applyTys (idType sel_id) inst_tys)
    error_string = showSDoc (hcat [ppr loc, text "|", ppr sel_id ])

    dm_wrapper   = WpApp this_dict_id <.> mkWpTyApps inst_tys 

    omitted_meth_warn :: SDoc
    omitted_meth_warn = ptext (sLit "No explicit method nor default method for")
                        <+> quotes (ppr sel_id)

    dfun_lam_vars = map instToVar dfun_dicts
    meth_wrapper = mkWpApps dfun_lam_vars <.> mkWpTyApps (mkTyVarTys tyvars)


wrapId :: HsWrapper -> id -> HsExpr id
wrapId wrapper id = mkHsWrap wrapper (HsVar id)
\end{code}

Note [Default methods in instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this

   class Baz v x where
      foo :: x -> x
      foo y = y

   instance Baz Int Int

From the class decl we get

   $dmfoo :: forall v x. Baz v x => x -> x

Notice that the type is ambiguous.  That's fine, though. The instance decl generates

   $dBazIntInt = MkBaz ($dmfoo Int Int $dBazIntInt)

BUT this does mean we must generate the dictionary translation directly, rather
than generating source-code and type-checking it.  That was the bug ing
Trac #1061. In any case it's less work to generate the translated version!


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
    (_,_,cls,tys) = tcSplitDFunTy dfun_ty

inst_decl_ctxt :: SDoc -> SDoc
inst_decl_ctxt doc = ptext (sLit "In the instance declaration for") <+> quotes doc

superClassCtxt :: SDoc
superClassCtxt = ptext (sLit "When checking the super-classes of an instance declaration")

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
      , ptext (sLit "Found") <+> ppr ty <+> ptext (sLit "but expected") <+>
         ppr instTy
      ]
\end{code}
