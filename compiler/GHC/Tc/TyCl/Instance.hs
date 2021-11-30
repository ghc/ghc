{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Typechecking instance declarations
module GHC.Tc.TyCl.Instance
   ( tcInstDecls1
   , tcInstDeclsDeriv
   , tcInstDecls2
   )
where

import GHC.Prelude

import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Gen.Bind
import GHC.Tc.TyCl
import GHC.Tc.TyCl.Utils ( addTyConsToGblEnv )
import GHC.Tc.TyCl.Class ( tcClassDecl2, ClassScopedTVEnv, tcATDefault,
                           HsSigFun, mkHsSigFun, badMethodErr,
                           findMethodBind, instantiateMethod )
import GHC.Tc.Solver( pushLevelAndSolveEqualitiesX, reportUnsolvedEqualities )
import GHC.Tc.Gen.Sig
import GHC.Tc.Utils.Monad
import GHC.Tc.Validity
import GHC.Tc.Utils.Zonk
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.TyCl.Build
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Class( AssocInstInfo(..), isNotAssociated )
import GHC.Core.Multiplicity
import GHC.Core.InstEnv
import GHC.Tc.Instance.Family
import GHC.Core.FamInstEnv
import GHC.Tc.Deriv
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.Unify
import GHC.Core        ( Expr(..), mkApps, mkVarApps, mkLams )
import GHC.Core.Make   ( nO_METHOD_BINDING_ERROR_ID )
import GHC.Core.Unfold.Make ( mkInlineUnfoldingWithArity, mkDFunUnfolding )
import GHC.Core.Type
import GHC.Core.SimpleOpt
import GHC.Core.Predicate( classMethodInstTy )
import GHC.Tc.Types.Evidence
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.Class
import GHC.Types.Error
import GHC.Types.Var as Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Data.Bag
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Utils.Logger
import GHC.Data.FastString
import GHC.Types.Id
import GHC.Types.SourceText
import GHC.Data.List.SetOps
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Data.BooleanFormula ( isUnsatisfied, pprBooleanFormulaNice )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Tuple
import GHC.Data.Maybe
import Data.List( mapAccumL )


{-
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
Here is how we translate instance declarations into Core

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
        {-# NOINLINE CONLIKE df_i #-}   -- Never inline dictionary functions
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
  df_i, leaving a *self*-recursive op1_i.  (If op1_i doesn't call op at
  the same type, it won't mention df_i, so there won't be recursion in
  the first place.)

* If op1_i is marked INLINE by the user there's a danger that we won't
  inline df_i in it, and that in turn means that (since it'll be a
  loop-breaker because df_i isn't), op1_i will ironically never be
  inlined.  But this is OK: the recursion breaking happens by way of
  a RULE (the magic ClassOp rule above), and RULES work inside InlineRule
  unfoldings. See Note [RULEs enabled in InitialPhase] in GHC.Core.Opt.Simplify.Utils

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

 * We make GHC.Core.Unfold.exprIsConApp_maybe spot a DFunUnfolding and return
   a suitable constructor application -- inlining df "on the fly" as it
   were.

 * ClassOp rules: We give the ClassOp 'op2' a BuiltinRule that
   extracts the right piece iff its argument satisfies
   exprIsConApp_maybe.  This is done in GHC.Types.Id.Make.mkDictSelId

 * We make 'df' CONLIKE, so that shared uses still match; eg
      let d = df d1 d2
      in ...(op2 d)...(op1 d)...

Note [Single-method classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the class has just one method (or, more accurately, just one element
of {superclasses + methods}), then we use a different strategy.

   class C a where op :: a -> a
   instance C a => C [a] where op = <blah>

We translate the class decl into a newtype, which just gives a
top-level axiom. The "constructor" MkC expands to a cast, as does the
class-op selector.

   axiom Co:C a :: C a ~ (a->a)

   op :: forall a. C a -> (a -> a)
   op a d = d |> (Co:C a)

   MkC :: forall a. (a->a) -> C a
   MkC = /\a.\op. op |> (sym Co:C a)

The clever RULE stuff doesn't work now, because ($df a d) isn't
a constructor application, so exprIsConApp_maybe won't return
Just <blah>.

Instead, we simply rely on the fact that casts are cheap:

   $df :: forall a. C a => C [a]
   {-# INLINE df #-}  -- NB: INLINE this
   $df = /\a. \d. MkC [a] ($cop_list a d)
       = $cop_list |> forall a. C a -> (sym (Co:C [a]))

   $cop_list :: forall a. C a => [a] -> [a]
   $cop_list = <blah>

So if we see
   (op ($df a d))
we'll inline 'op' and '$df', since both are simply casts, and
good things happen.

Why do we use this different strategy?  Because otherwise we
end up with non-inlined dictionaries that look like
    $df = $cop |> blah
which adds an extra indirection to every use, which seems stupid.  See
#4138 for an example (although the regression reported there
wasn't due to the indirection).

There is an awkward wrinkle though: we want to be very
careful when we have
    instance C a => C [a] where
      {-# INLINE op #-}
      op = ...
then we'll get an INLINE pragma on $cop_list but it's important that
$cop_list only inlines when it's applied to *two* arguments (the
dictionary and the list argument).  So we must not eta-expand $df
above.  We ensure that this doesn't happen by putting an INLINE
pragma on the dfun itself; after all, it ends up being just a cast.

There is one more dark corner to the INLINE story, even more deeply
buried.  Consider this (#3772):

    class DeepSeq a => C a where
      gen :: Int -> a

    instance C a => C [a] where
      gen n = ...

    class DeepSeq a where
      deepSeq :: a -> b -> b

    instance DeepSeq a => DeepSeq [a] where
      {-# INLINE deepSeq #-}
      deepSeq xs b = foldr deepSeq b xs

That gives rise to these defns:

    $cdeepSeq :: DeepSeq a -> [a] -> b -> b
    -- User INLINE( 3 args )!
    $cdeepSeq a (d:DS a) b (x:[a]) (y:b) = ...

    $fDeepSeq[] :: DeepSeq a -> DeepSeq [a]
    -- DFun (with auto INLINE pragma)
    $fDeepSeq[] a d = $cdeepSeq a d |> blah

    $cp1 a d :: C a => DeepSep [a]
    -- We don't want to eta-expand this, lest
    -- $cdeepSeq gets inlined in it!
    $cp1 a d = $fDeepSep[] a (scsel a d)

    $fC[] :: C a => C [a]
    -- Ordinary DFun
    $fC[] a d = MkC ($cp1 a d) ($cgen a d)

Here $cp1 is the code that generates the superclass for C [a].  The
issue is this: we must not eta-expand $cp1 either, or else $fDeepSeq[]
and then $cdeepSeq will inline there, which is definitely wrong.  Like
on the dfun, we solve this by adding an INLINE pragma to $cp1.

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
[Overlapping instances] in GHC.Core.InstEnv).  But in *this* case it's wrong to
complain, because we just want to delegate to the op2 of this same
instance.

Why is this justified?  Because we generate a (C [a]) constraint in
a context in which 'a' cannot be instantiated to anything that matches
other overlapping instances, or else we would not be executing this
version of op1 in the first place.

It might even be a bit disguised:

  nullFail :: C [a] => [a] -> [a]
  nullFail x = op2 x ++ op2 x

  instance C a => C [a] where
    op1 x = nullFail x

Precisely this is used in package 'regex-base', module Context.hs.
See the overlapping instances for RegexContext, and the fact that they
call 'nullFail' just like the example above.  The DoCon package also
does the same thing; it shows up in module Fraction.hs.

Conclusion: when typechecking the methods in a C [a] instance, we want to
treat the 'a' as an *existential* type variable, in the sense described
by Note [Binding when looking up instances].  That is why isOverlappableTyVar
responds True to an InstSkol, which is the kind of skolem we use in
tcInstDecl2.


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



************************************************************************
*                                                                      *
\subsection{Extracting instance decls}
*                                                                      *
************************************************************************

Gather up the instance declarations from their various sources
-}

tcInstDecls1    -- Deal with both source-code and imported instance decls
   :: [LInstDecl GhcRn]         -- Source code instance decls
   -> TcM (TcGblEnv,            -- The full inst env
           [InstInfo GhcRn],    -- Source-code instance decls to process;
                                -- contains all dfuns for this module
           [DerivInfo],         -- From data family instances
           ThBindEnv)           -- TH binding levels

tcInstDecls1 inst_decls
  = do {    -- Do class and family instance declarations
       ; stuff <- mapAndRecoverM tcLocalInstDecl inst_decls

       ; let (local_infos_s, fam_insts_s, datafam_deriv_infos) = unzip3 stuff
             fam_insts   = concat fam_insts_s
             local_infos = concat local_infos_s

       ; (gbl_env, th_bndrs) <-
           addClsInsts local_infos $
           addFamInsts fam_insts

       ; return ( gbl_env
                , local_infos
                , concat datafam_deriv_infos
                , th_bndrs ) }

-- | Use DerivInfo for data family instances (produced by tcInstDecls1),
--   datatype declarations (TyClDecl), and standalone deriving declarations
--   (DerivDecl) to check and process all derived class instances.
tcInstDeclsDeriv
  :: [DerivInfo]
  -> [LDerivDecl GhcRn]
  -> TcM (TcGblEnv, [InstInfo GhcRn], HsValBinds GhcRn)
tcInstDeclsDeriv deriv_infos derivds
  = do th_stage <- getStage -- See Note [Deriving inside TH brackets]
       if isBrackStage th_stage
       then do { gbl_env <- getGblEnv
               ; return (gbl_env, bagToList emptyBag, emptyValBindsOut) }
       else do { (tcg_env, info_bag, valbinds) <- tcDeriving deriv_infos derivds
               ; return (tcg_env, bagToList info_bag, valbinds) }

addClsInsts :: [InstInfo GhcRn] -> TcM a -> TcM a
addClsInsts infos thing_inside
  = tcExtendLocalInstEnv (map iSpec infos) thing_inside

addFamInsts :: [FamInst] -> TcM (TcGblEnv, ThBindEnv)
-- Extend (a) the family instance envt
--        (b) the type envt with stuff from data type decls
addFamInsts fam_insts
  = tcExtendLocalFamInstEnv fam_insts $
    tcExtendGlobalEnv axioms          $
    do { traceTc "addFamInsts" (pprFamInsts fam_insts)
       ; (gbl_env, th_bndrs) <- addTyConsToGblEnv data_rep_tycons
                    -- Does not add its axiom; that comes
                    -- from adding the 'axioms' above
       ; return (gbl_env, th_bndrs)
       }
  where
    axioms = map (ACoAxiom . toBranchedAxiom . famInstAxiom) fam_insts
    data_rep_tycons = famInstsRepTyCons fam_insts
      -- The representation tycons for 'data instances' declarations

{-
Note [Deriving inside TH brackets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a declaration bracket
  [d| data T = A | B deriving( Show ) |]

there is really no point in generating the derived code for deriving(
Show) and then type-checking it. This will happen at the call site
anyway, and the type check should never fail!  Moreover (#6005)
the scoping of the generated code inside the bracket does not seem to
work out.

The easy solution is simply not to generate the derived instances at
all.  (A less brutal solution would be to generate them with no
bindings.)  This will become moot when we shift to the new TH plan, so
the brutal solution will do.
-}

tcLocalInstDecl :: LInstDecl GhcRn
                -> TcM ([InstInfo GhcRn], [FamInst], [DerivInfo])
        -- A source-file instance declaration
        -- Type-check all the stuff before the "where"
        --
        -- We check for respectable instance type, and context
tcLocalInstDecl (L loc (TyFamInstD { tfid_inst = decl }))
  = do { fam_inst <- tcTyFamInstDecl NotAssociated (L loc decl)
       ; return ([], [fam_inst], []) }

tcLocalInstDecl (L loc (DataFamInstD { dfid_inst = decl }))
  = do { (fam_inst, m_deriv_info) <- tcDataFamInstDecl NotAssociated emptyVarEnv (L loc decl)
       ; return ([], [fam_inst], maybeToList m_deriv_info) }

tcLocalInstDecl (L loc (ClsInstD { cid_inst = decl }))
  = do { (insts, fam_insts, deriv_infos) <- tcClsInstDecl (L loc decl)
       ; return (insts, fam_insts, deriv_infos) }

tcClsInstDecl :: LClsInstDecl GhcRn
              -> TcM ([InstInfo GhcRn], [FamInst], [DerivInfo])
-- The returned DerivInfos are for any associated data families
tcClsInstDecl (L loc (ClsInstDecl { cid_poly_ty = hs_ty, cid_binds = binds
                                  , cid_sigs = uprags, cid_tyfam_insts = ats
                                  , cid_overlap_mode = overlap_mode
                                  , cid_datafam_insts = adts }))
  = setSrcSpanA loc                      $
    addErrCtxt (instDeclCtxt1 hs_ty)  $
    do  { dfun_ty <- tcHsClsInstType (InstDeclCtxt False) hs_ty
        ; let (tyvars, theta, clas, inst_tys) = tcSplitDFunTy dfun_ty
             -- NB: tcHsClsInstType does checkValidInstance

        ; (subst, skol_tvs) <- tcInstSkolTyVars InstSkol tyvars
        ; let tv_skol_prs = [ (tyVarName tv, skol_tv)
                            | (tv, skol_tv) <- tyvars `zip` skol_tvs ]
              -- Map from the skolemized Names to the original Names.
              -- See Note [Associated data family instances and di_scoped_tvs].
              tv_skol_env = mkVarEnv $ map swap tv_skol_prs
              n_inferred = countWhile ((== Inferred) . binderArgFlag) $
                           fst $ splitForAllTyCoVarBinders dfun_ty
              visible_skol_tvs = drop n_inferred skol_tvs

        ; traceTc "tcLocalInstDecl 1" (ppr dfun_ty $$ ppr (invisibleTyBndrCount dfun_ty) $$ ppr skol_tvs)

        -- Next, process any associated types.
        ; (datafam_stuff, tyfam_insts)
             <- tcExtendNameTyVarEnv tv_skol_prs $
                do  { let mini_env   = mkVarEnv (classTyVars clas `zip` substTys subst inst_tys)
                          mini_subst = mkTvSubst (mkInScopeSet (mkVarSet skol_tvs)) mini_env
                          mb_info    = InClsInst { ai_class = clas
                                                 , ai_tyvars = visible_skol_tvs
                                                 , ai_inst_env = mini_env }
                    ; df_stuff  <- mapAndRecoverM (tcDataFamInstDecl mb_info tv_skol_env) adts
                    ; tf_insts1 <- mapAndRecoverM (tcTyFamInstDecl mb_info)   ats

                      -- Check for missing associated types and build them
                      -- from their defaults (if available)
                    ; is_boot <- tcIsHsBootOrSig
                    ; let atItems = classATItems clas
                    ; tf_insts2 <- mapM (tcATDefault (locA loc) mini_subst defined_ats)
                                        (if is_boot then [] else atItems)
                      -- Don't default type family instances, but rather omit, in hsig/hs-boot.
                      -- Since hsig/hs-boot files are essentially large binders we want omission
                      -- of the definition to result in no restriction, rather than for example
                      -- attempting to "pattern match" with the invisible defaults and generate
                      -- equalities. Without further handling, this would just result in a panic
                      -- anyway.
                      -- See https://github.com/ghc-proposals/ghc-proposals/pull/320 for
                      -- additional discussion.
                    ; return (df_stuff, tf_insts1 ++ concat tf_insts2) }


        -- Finally, construct the Core representation of the instance.
        -- (This no longer includes the associated types.)
        ; dfun_name <- newDFunName clas inst_tys (getLocA hs_ty)
                -- Dfun location is that of instance *header*

        ; ispec <- newClsInst (fmap unLoc overlap_mode) dfun_name
                              tyvars theta clas inst_tys

        ; let inst_binds = InstBindings
                             { ib_binds = binds
                             , ib_tyvars = map Var.varName tyvars -- Scope over bindings
                             , ib_pragmas = uprags
                             , ib_extensions = []
                             , ib_derived = False }
              inst_info = InstInfo { iSpec  = ispec, iBinds = inst_binds }

              (datafam_insts, m_deriv_infos) = unzip datafam_stuff
              deriv_infos                    = catMaybes m_deriv_infos
              all_insts                      = tyfam_insts ++ datafam_insts

         -- In hs-boot files there should be no bindings
        ; let no_binds = isEmptyLHsBinds binds && null uprags
        ; is_boot <- tcIsHsBootOrSig
        ; failIfTc (is_boot && not no_binds) TcRnIllegalHsBootFileDecl

        ; return ( [inst_info], all_insts, deriv_infos ) }
  where
    defined_ats = mkNameSet (map (tyFamInstDeclName . unLoc) ats)
                  `unionNameSet`
                  mkNameSet (map (unLoc . feqn_tycon
                                        . dfid_eqn
                                        . unLoc) adts)

{-
************************************************************************
*                                                                      *
               Type family instances
*                                                                      *
************************************************************************

Family instances are somewhat of a hybrid.  They are processed together with
class instance heads, but can contain data constructors and hence they share a
lot of kinding and type checking code with ordinary algebraic data types (and
GADTs).
-}

tcTyFamInstDecl :: AssocInstInfo
                -> LTyFamInstDecl GhcRn -> TcM FamInst
  -- "type instance"
  -- See Note [Associated type instances]
tcTyFamInstDecl mb_clsinfo (L loc decl@(TyFamInstDecl { tfid_eqn = eqn }))
  = setSrcSpanA loc           $
    tcAddTyFamInstCtxt decl  $
    do { let fam_lname = feqn_tycon eqn
       ; fam_tc <- tcLookupLocatedTyCon fam_lname
       ; tcFamInstDeclChecks mb_clsinfo fam_tc

         -- (0) Check it's an open type family
       ; checkTc (isTypeFamilyTyCon fam_tc)     (wrongKindOfFamily fam_tc)
       ; checkTc (isOpenTypeFamilyTyCon fam_tc) (notOpenFamily fam_tc)

         -- (1) do the work of verifying the synonym group
         -- For some reason we don't have a location for the equation
         -- itself, so we make do with the location of family name
       ; co_ax_branch <- tcTyFamInstEqn fam_tc mb_clsinfo
                                        (L (na2la $ getLoc fam_lname) eqn)

         -- (2) check for validity
       ; checkConsistentFamInst mb_clsinfo fam_tc co_ax_branch
       ; checkValidCoAxBranch fam_tc co_ax_branch

         -- (3) construct coercion axiom
       ; rep_tc_name <- newFamInstAxiomName fam_lname [coAxBranchLHS co_ax_branch]
       ; let axiom = mkUnbranchedCoAxiom rep_tc_name fam_tc co_ax_branch
       ; newFamInst SynFamilyInst axiom }


---------------------
tcFamInstDeclChecks :: AssocInstInfo -> TyCon -> TcM ()
-- Used for both type and data families
tcFamInstDeclChecks mb_clsinfo fam_tc
  = do { -- Type family instances require -XTypeFamilies
         -- and can't (currently) be in an hs-boot file
       ; traceTc "tcFamInstDecl" (ppr fam_tc)
       ; type_families <- xoptM LangExt.TypeFamilies
       ; is_boot       <- tcIsHsBootOrSig   -- Are we compiling an hs-boot file?
       ; checkTc type_families $ badFamInstDecl fam_tc
       ; checkTc (not is_boot) $ badBootFamInstDeclErr

       -- Check that it is a family TyCon, and that
       -- oplevel type instances are not for associated types.
       ; checkTc (isFamilyTyCon fam_tc) (notFamily fam_tc)

       ; when (isNotAssociated mb_clsinfo &&   -- Not in a class decl
               isTyConAssoc fam_tc)            -- but an associated type
              (addErr $ assocInClassErr fam_tc)
       }

{- Note [Associated type instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We allow this:
  class C a where
    type T x a
  instance C Int where
    type T (S y) Int = y
    type T Z     Int = Char

Note that
  a) The variable 'x' is not bound by the class decl
  b) 'x' is instantiated to a non-type-variable in the instance
  c) There are several type instance decls for T in the instance

All this is fine.  Of course, you can't give any *more* instances
for (T ty Int) elsewhere, because it's an *associated* type.


************************************************************************
*                                                                      *
               Data family instances
*                                                                      *
************************************************************************

For some reason data family instances are a lot more complicated
than type family instances
-}

tcDataFamInstDecl ::
     AssocInstInfo
  -> TyVarEnv Name -- If this is an associated data family instance, maps the
                   -- parent class's skolemized type variables to their
                   -- original Names. If this is a non-associated instance,
                   -- this will be empty.
                   -- See Note [Associated data family instances and di_scoped_tvs].
  -> LDataFamInstDecl GhcRn -> TcM (FamInst, Maybe DerivInfo)
  -- "newtype instance" and "data instance"
tcDataFamInstDecl mb_clsinfo tv_skol_env
    (L loc decl@(DataFamInstDecl { dfid_eqn =
      FamEqn { feqn_bndrs  = outer_bndrs
             , feqn_pats   = hs_pats
             , feqn_tycon  = lfam_name@(L _ fam_name)
             , feqn_fixity = fixity
             , feqn_rhs    = HsDataDefn { dd_ND      = new_or_data
                                        , dd_cType   = cType
                                        , dd_ctxt    = hs_ctxt
                                        , dd_cons    = hs_cons
                                        , dd_kindSig = m_ksig
                                        , dd_derivs  = derivs } }}))
  = setSrcSpanA loc            $
    tcAddDataFamInstCtxt decl  $
    do { fam_tc <- tcLookupLocatedTyCon lfam_name

       ; tcFamInstDeclChecks mb_clsinfo fam_tc

       -- Check that the family declaration is for the right kind
       ; checkTc (isDataFamilyTyCon fam_tc) (wrongKindOfFamily fam_tc)
       ; gadt_syntax <- dataDeclChecks fam_name new_or_data hs_ctxt hs_cons
          -- Do /not/ check that the number of patterns = tyConArity fam_tc
          -- See [Arity of data families] in GHC.Core.FamInstEnv
       ; (qtvs, pats, res_kind, stupid_theta)
             <- tcDataFamInstHeader mb_clsinfo fam_tc outer_bndrs fixity
                                    hs_ctxt hs_pats m_ksig new_or_data

       -- Eta-reduce the axiom if possible
       -- Quite tricky: see Note [Implementing eta reduction for data families]
       ; let (eta_pats, eta_tcbs) = eta_reduce fam_tc pats
             eta_tvs       = map binderVar eta_tcbs
             post_eta_qtvs = filterOut (`elem` eta_tvs) qtvs

             full_tcbs = mkTyConBindersPreferAnon post_eta_qtvs
                            (tyCoVarsOfType (mkSpecForAllTys eta_tvs res_kind))
                         ++ eta_tcbs
                 -- Put the eta-removed tyvars at the end
                 -- Remember, qtvs is in arbitrary order, except kind vars are
                 -- first, so there is no reason to suppose that the eta_tvs
                 -- (obtained from the pats) are at the end (#11148)

       -- Eta-expand the representation tycon until it has result
       -- kind `TYPE r`, for some `r`. If UnliftedNewtypes is not enabled, we
       -- go one step further and ensure that it has kind `TYPE 'LiftedRep`.
       --
       -- See also Note [Arity of data families] in GHC.Core.FamInstEnv
       -- NB: we can do this after eta-reducing the axiom, because if
       --     we did it before the "extra" tvs from etaExpandAlgTyCon
       --     would always be eta-reduced
       --
       ; (extra_tcbs, final_res_kind) <- etaExpandAlgTyCon full_tcbs res_kind

       -- Check the result kind; it may come from a user-written signature.
       -- See Note [Datatype return kinds] in GHC.Tc.TyCl point 4(a)
       ; let extra_pats  = map (mkTyVarTy . binderVar) extra_tcbs
             all_pats    = pats `chkAppend` extra_pats
             orig_res_ty = mkTyConApp fam_tc all_pats
             ty_binders  = full_tcbs `chkAppend` extra_tcbs

       ; traceTc "tcDataFamInstDecl" $
         vcat [ text "Fam tycon:" <+> ppr fam_tc
              , text "Pats:" <+> ppr pats
              , text "visibilities:" <+> ppr (tcbVisibilities fam_tc pats)
              , text "all_pats:" <+> ppr all_pats
              , text "ty_binders" <+> ppr ty_binders
              , text "fam_tc_binders:" <+> ppr (tyConBinders fam_tc)
              , text "res_kind:" <+> ppr res_kind
              , text "final_res_kind:" <+> ppr final_res_kind
              , text "eta_pats" <+> ppr eta_pats
              , text "eta_tcbs" <+> ppr eta_tcbs ]

       ; (rep_tc, axiom) <- fixM $ \ ~(rec_rep_tc, _) ->
           do { data_cons <- tcExtendTyVarEnv qtvs $
                  -- For H98 decls, the tyvars scope
                  -- over the data constructors
                  tcConDecls new_or_data (DDataInstance orig_res_ty)
                             rec_rep_tc ty_binders final_res_kind
                             hs_cons

              ; rep_tc_name <- newFamInstTyConName lfam_name pats
              ; axiom_name  <- newFamInstAxiomName lfam_name [pats]
              ; tc_rhs <- case new_or_data of
                     DataType -> return $
                        mkLevPolyDataTyConRhs
                          (isFixedRuntimeRepKind final_res_kind)
                          data_cons
                     NewType  -> assert (not (null data_cons)) $
                                 mkNewTyConRhs rep_tc_name rec_rep_tc (head data_cons)

              ; let ax_rhs = mkTyConApp rep_tc (mkTyVarTys post_eta_qtvs)
                    axiom  = mkSingleCoAxiom Representational axiom_name
                                 post_eta_qtvs eta_tvs [] fam_tc eta_pats ax_rhs
                    parent = DataFamInstTyCon axiom fam_tc all_pats

                      -- NB: Use the full ty_binders from the pats. See bullet toward
                      -- the end of Note [Data type families] in GHC.Core.TyCon
                    rep_tc   = mkAlgTyCon rep_tc_name
                                          ty_binders final_res_kind
                                          (map (const Nominal) ty_binders)
                                          (fmap unLoc cType) stupid_theta
                                          tc_rhs parent
                                          gadt_syntax
                 -- We always assume that indexed types are recursive.  Why?
                 -- (1) Due to their open nature, we can never be sure that a
                 -- further instance might not introduce a new recursive
                 -- dependency.  (2) They are always valid loop breakers as
                 -- they involve a coercion.
              ; return (rep_tc, axiom) }

       -- Remember to check validity; no recursion to worry about here
       -- Check that left-hand sides are ok (mono-types, no type families,
       -- consistent instantiations, etc)
       ; let ax_branch = coAxiomSingleBranch axiom
       ; checkConsistentFamInst mb_clsinfo fam_tc ax_branch
       ; checkValidCoAxBranch fam_tc ax_branch
       ; checkValidTyCon rep_tc

       ; let scoped_tvs = map mk_deriv_info_scoped_tv_pr (tyConTyVars rep_tc)
             m_deriv_info = case derivs of
               []    -> Nothing
               preds ->
                 Just $ DerivInfo { di_rep_tc  = rep_tc
                                  , di_scoped_tvs = scoped_tvs
                                  , di_clauses = preds
                                  , di_ctxt    = tcMkDataFamInstCtxt decl }

       ; fam_inst <- newFamInst (DataFamilyInst rep_tc) axiom
       ; return (fam_inst, m_deriv_info) }
  where
    eta_reduce :: TyCon -> [Type] -> ([Type], [TyConBinder])
    -- See Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom
    -- Splits the incoming patterns into two: the [TyVar]
    -- are the patterns that can be eta-reduced away.
    -- e.g.     T [a] Int a d c   ==>  (T [a] Int a, [d,c])
    --
    -- NB: quadratic algorithm, but types are small here
    eta_reduce fam_tc pats
        = go (reverse (zip3 pats fvs_s vis_s)) []
        where
          vis_s :: [TyConBndrVis]
          vis_s = tcbVisibilities fam_tc pats

          fvs_s :: [TyCoVarSet]  -- 1-1 correspondence with pats
                                 -- Each elt is the free vars of all /earlier/ pats
          (_, fvs_s) = mapAccumL add_fvs emptyVarSet pats
          add_fvs fvs pat = (fvs `unionVarSet` tyCoVarsOfType pat, fvs)

    go ((pat, fvs_to_the_left, tcb_vis):pats) etad_tvs
      | Just tv <- getTyVar_maybe pat
      , not (tv `elemVarSet` fvs_to_the_left)
      = go pats (Bndr tv tcb_vis : etad_tvs)
    go pats etad_tvs = (reverse (map fstOf3 pats), etad_tvs)

    -- Create a Name-TyVar mapping to bring into scope when typechecking any
    -- deriving clauses this data family instance may have.
    -- See Note [Associated data family instances and di_scoped_tvs].
    mk_deriv_info_scoped_tv_pr :: TyVar -> (Name, TyVar)
    mk_deriv_info_scoped_tv_pr tv =
      let n = lookupWithDefaultVarEnv tv_skol_env (tyVarName tv) tv
      in (n, tv)

{-
Note [Associated data family instances and di_scoped_tvs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some care is required to implement `deriving` correctly for associated data
family instances. Consider this example from #18055:

  class C a where
    data D a

  class X a b

  instance C (Maybe a) where
    data D (Maybe a) deriving (X a)

When typechecking the `X a` in `deriving (X a)`, we must ensure that the `a`
from the instance header is brought into scope. This is the role of
di_scoped_tvs, which maps from the original, renamed `a` to the skolemized,
typechecked `a`. When typechecking the `deriving` clause, this mapping will be
consulted when looking up the `a` in `X a`.

A naïve attempt at creating the di_scoped_tvs is to simply reuse the
tyConTyVars of the representation TyCon for `data D (Maybe a)`. This is only
half correct, however. We do want the typechecked `a`'s Name in the /range/
of the mapping, but we do not want it in the /domain/ of the mapping.
To ensure that the original `a`'s Name ends up in the domain, we consult a
TyVarEnv (passed as an argument to tcDataFamInstDecl) that maps from the
typechecked `a`'s Name to the original `a`'s Name. In the even that
tcDataFamInstDecl is processing a non-associated data family instance, this
TyVarEnv will simply be empty, and there is nothing to worry about.
-}

-----------------------
tcDataFamInstHeader
    :: AssocInstInfo -> TyCon -> HsOuterFamEqnTyVarBndrs GhcRn
    -> LexicalFixity -> Maybe (LHsContext GhcRn)
    -> HsTyPats GhcRn -> Maybe (LHsKind GhcRn)
    -> NewOrData
    -> TcM ([TyVar], [Type], Kind, ThetaType)
-- The "header" of a data family instance is the part other than
-- the data constructors themselves
--    e.g.  data instance D [a] :: * -> * where ...
-- Here the "header" is the bit before the "where"
tcDataFamInstHeader mb_clsinfo fam_tc outer_bndrs fixity
                    hs_ctxt hs_pats m_ksig new_or_data
  = do { traceTc "tcDataFamInstHeader {" (ppr fam_tc <+> ppr hs_pats)
       ; (tclvl, wanted, (scoped_tvs, (stupid_theta, lhs_ty, master_res_kind, instance_res_kind)))
            <- pushLevelAndSolveEqualitiesX "tcDataFamInstHeader" $
               bindOuterFamEqnTKBndrs FamInstSkol outer_bndrs                 $
               do { stupid_theta <- tcHsContext hs_ctxt
                  ; (lhs_ty, lhs_kind) <- tcFamTyPats fam_tc hs_pats
                  ; (lhs_applied_ty, lhs_applied_kind)
                      <- tcInstInvisibleTyBinders lhs_ty lhs_kind
                      -- See Note [Data family/instance return kinds]
                      -- in GHC.Tc.TyCl point (DF3)

                  -- Ensure that the instance is consistent
                  -- with its parent class
                  ; addConsistencyConstraints mb_clsinfo lhs_ty

                  -- Add constraints from the result signature
                  ; res_kind <- tc_kind_sig m_ksig

                  -- Do not add constraints from the data constructors
                  -- See Note [Kind inference for data family instances]

                  -- Check that the result kind of the TyCon applied to its args
                  -- is compatible with the explicit signature (or Type, if there
                  -- is none)
                  ; let hs_lhs = nlHsTyConApp fixity (getName fam_tc) hs_pats
                  ; _ <- unifyKind (Just (ppr hs_lhs)) lhs_applied_kind res_kind

                  ; traceTc "tcDataFamInstHeader" $
                    vcat [ ppr fam_tc, ppr m_ksig, ppr lhs_applied_kind, ppr res_kind ]
                  ; return ( stupid_theta
                           , lhs_applied_ty
                           , lhs_applied_kind
                           , res_kind ) }

       -- This code (and the stuff immediately above) is very similar
       -- to that in tcTyFamInstEqnGuts.  Maybe we should abstract the
       -- common code; but for the moment I concluded that it's
       -- clearer to duplicate it.  Still, if you fix a bug here,
       -- check there too!

       -- See GHC.Tc.TyCl Note [Generalising in tcFamTyPatsGuts]
       ; dvs  <- candidateQTyVarsOfTypes (lhs_ty : mkTyVarTys scoped_tvs)
       ; qtvs <- quantifyTyVars FamInstSkol TryNotToDefaultNonStandardTyVars dvs
       ; reportUnsolvedEqualities FamInstSkol qtvs tclvl wanted

       -- Zonk the patterns etc into the Type world
       ; ze           <- mkEmptyZonkEnv NoFlexi
       ; (ze, qtvs)   <- zonkTyBndrsX           ze qtvs
       ; lhs_ty       <- zonkTcTypeToTypeX      ze lhs_ty
       ; stupid_theta <- zonkTcTypesToTypesX    ze stupid_theta
       ; master_res_kind   <- zonkTcTypeToTypeX ze master_res_kind
       ; instance_res_kind <- zonkTcTypeToTypeX ze instance_res_kind

       -- We check that res_kind is OK with checkDataKindSig in
       -- tcDataFamInstDecl, after eta-expansion.  We need to check that
       -- it's ok because res_kind can come from a user-written kind signature.
       -- See Note [Datatype return kinds], point (4a)

       ; checkDataKindSig (DataInstanceSort new_or_data) master_res_kind
       ; checkDataKindSig (DataInstanceSort new_or_data) instance_res_kind

       -- Check that type patterns match the class instance head
       -- The call to splitTyConApp_maybe here is just an inlining of
       -- the body of unravelFamInstPats.
       ; pats <- case splitTyConApp_maybe lhs_ty of
           Just (_, pats) -> pure pats
           Nothing -> pprPanic "tcDataFamInstHeader" (ppr lhs_ty)

       ; return (qtvs, pats, master_res_kind, stupid_theta) }
  where
    fam_name  = tyConName fam_tc
    data_ctxt = DataKindCtxt fam_name

    -- See Note [Implementation of UnliftedNewtypes] in GHC.Tc.TyCl, families (2),
    -- and Note [Implementation of UnliftedDatatypes].
    tc_kind_sig Nothing
      = do { unlifted_newtypes  <- xoptM LangExt.UnliftedNewtypes
           ; unlifted_datatypes <- xoptM LangExt.UnliftedDatatypes
           ; case new_or_data of
               NewType  | unlifted_newtypes  -> newOpenTypeKind
               DataType | unlifted_datatypes -> newOpenTypeKind
               _                             -> pure liftedTypeKind
           }

    -- See Note [Result kind signature for a data family instance]
    tc_kind_sig (Just hs_kind)
      = do { sig_kind <- tcLHsKindSig data_ctxt hs_kind
           ; lvl <- getTcLevel
           ; let (tvs, inner_kind) = tcSplitForAllInvisTyVars sig_kind
           ; (subst, _tvs') <- tcInstSkolTyVarsAt unkSkol lvl False emptyTCvSubst tvs
             -- Perhaps surprisingly, we don't need the skolemised tvs themselves
           ; return (substTy subst inner_kind) }

{- Note [Result kind signature for a data family instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The expected type might have a forall at the type. Normally, we
can't skolemise in kinds because we don't have type-level lambda.
But here, we're at the top-level of an instance declaration, so
we actually have a place to put the regeneralised variables.
Thus: skolemise away. cf. GHC.Tc.Utils.Unify.tcSkolemise
Examples in indexed-types/should_compile/T12369

Note [Implementing eta reduction for data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data D :: * -> * -> * -> * -> *

   data instance D [(a,b)] p q :: * -> * where
      D1 :: blah1
      D2 :: blah2

Then we'll generate a representation data type
  data Drep a b p q z where
      D1 :: blah1
      D2 :: blah2

and an axiom to connect them
  axiom AxDrep forall a b p q z. D [(a,b]] p q z = Drep a b p q z

except that we'll eta-reduce the axiom to
  axiom AxDrep forall a b. D [(a,b]] = Drep a b

This is described at some length in Note [Eta reduction for data families]
in GHC.Core.Coercion.Axiom. There are several fiddly subtleties lurking here,
however, so this Note aims to describe these subtleties:

* The representation tycon Drep is parameterised over the free
  variables of the pattern, in no particular order. So there is no
  guarantee that 'p' and 'q' will come last in Drep's parameters, and
  in the right order.  So, if the /patterns/ of the family instance
  are eta-reducible, we re-order Drep's parameters to put the
  eta-reduced type variables last.

* Although we eta-reduce the axiom, we eta-/expand/ the representation
  tycon Drep.  The kind of D says it takes four arguments, but the
  data instance header only supplies three.  But the AlgTyCon for Drep
  itself must have enough TyConBinders so that its result kind is Type.
  So, with etaExpandAlgTyCon we make up some extra TyConBinders.
  See point (3) in Note [Datatype return kinds] in GHC.Tc.TyCl.

* The result kind in the instance might be a polykind, like this:
     data family DP a :: forall k. k -> *
     data instance DP [b] :: forall k1 k2. (k1,k2) -> *

  So in type-checking the LHS (DP Int) we need to check that it is
  more polymorphic than the signature.  To do that we must skolemise
  the signature and instantiate the call of DP.  So we end up with
     data instance DP [b] @(k1,k2) (z :: (k1,k2)) where

  Note that we must parameterise the representation tycon DPrep over
  'k1' and 'k2', as well as 'b'.

  The skolemise bit is done in tc_kind_sig, while the instantiate bit
  is done by tcFamTyPats.

* Very fiddly point.  When we eta-reduce to
     axiom AxDrep forall a b. D [(a,b]] = Drep a b

  we want the kind of (D [(a,b)]) to be the same as the kind of
  (Drep a b).  This ensures that applying the axiom doesn't change the
  kind.  Why is that hard?  Because the kind of (Drep a b) depends on
  the TyConBndrVis on Drep's arguments. In particular do we have
    (forall (k::*). blah) or (* -> blah)?

  We must match whatever D does!  In #15817 we had
      data family X a :: forall k. * -> *   -- Note: a forall that is not used
      data instance X Int b = MkX

  So the data instance is really
      data istance X Int @k b = MkX

  The axiom will look like
      axiom    X Int = Xrep

  and it's important that XRep :: forall k * -> *, following X.

  To achieve this we get the TyConBndrVis flags from tcbVisibilities,
  and use those flags for any eta-reduced arguments.  Sigh.

* The final turn of the knife is that tcbVisibilities is itself
  tricky to sort out.  Consider
      data family D k :: k
  Then consider D (forall k2. k2 -> k2) Type Type
  The visibility flags on an application of D may affected by the arguments
  themselves.  Heavy sigh.  But not truly hard; that's what tcbVisibilities
  does.

* Happily, we don't need to worry about the possibility of
  building an inhomogeneous axiom, described in GHC.Tc.TyCl.Build
  Note [Newtype eta and homogeneous axioms].   For example
     type F :: Type -> forall (b :: Type) -> Type
     data family F a b
     newtype instance F Int b = MkF (Proxy b)
  we get a newtype, and a eta-reduced axiom connecting the data family
  with the newtype:
     type R:FIntb :: forall (b :: Type) -> Type
     newtype R:FIntb b = MkF (Proxy b)
     axiom Foo.D:R:FIntb0 :: F Int = Foo.R:FIntb
  Now the subtleties of Note [Newtype eta and homogeneous axioms] are
  dealt with by the newtype (via mkNewTyConRhs called in tcDataFamInstDecl)
  while the axiom connecting F Int ~ R:FIntb is eta-reduced, but the
  quantifer 'b' is derived from the original data family F, and so the
  kinds will always match.

Note [Kind inference for data family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GADT-style data type declaration, where I have used
fresh variables in the data constructor's type, to stress that c,d are
quite distinct from a,b.
   data T a b where
     MkT :: forall c d. c d -> T c d

Following Note [Inferring kinds for type declarations] in GHC.Tc.TyCl,
to infer T's kind, we initially give T :: kappa, a monomorpic kind,
gather constraints from the header and data constructors, and conclude
   T :: (kappa1 -> type) -> kappa1 -> Type
Then we generalise, giving
   T :: forall k. (k->Type) -> k -> Type

Now what about a data /instance/ decl
   data family T :: forall k. (k->Type) -> k -> Type

   data instance T p Int where ...

No doubt here! The poly-kinded T is instantiated with k=Type, so the
header really looks like
   data instance T @Type (p :: Type->Type) Int where ...

But what about this?
   data instance T p q where
      MkT :: forall r. r Int -> T r Int

So what kind do 'p' and 'q' have?  No clues from the header, but from
the data constructor we can clearly see that (r :: Type->Type).  Does
that mean that the the /entire data instance/ is instantiated at Type,
like this?
   data instance T @Type (p :: Type->Type) (q :: Type) where
      ...

Not at all! This is a /GADT/-style decl, so the kind argument might
be specialised in this particular data constructor, thus:
   data instance T @k (p :: k->Type) (q :: k) where
     MkT :: forall (r :: Type -> Type).
            r Int -> T @Type r Int
(and perhaps specialised differently in some other data
constructor MkT2).

The key difference in this case and 'data T' at the top of this Note
is that we have no known kind for 'data T'. We thus forbid different
specialisations of T in its constructors, in an attempt to avoid
inferring polymorphic recursion. In data family T, however, there is
no problem with polymorphic recursion: we already /fully know/ T's
kind -- that came from the family declaration, and is not influenced
by the data instances -- and hence we /can/ specialise T's kind
differently in different GADT data constructors.

SHORT SUMMARY: in a data instance decl, it's not clear whether kind
constraints arising from the data constructors should be considered
local to the (GADT) data /constructor/ or should apply to the entire
data instance.

DESIGN CHOICE: in data/newtype family instance declarations, we ignore
the /data constructor/ declarations altogether, looking only at the
data instance /header/.

Observations:
* This choice is simple to describe, as well as simple to implement.
  For a data/newtype instance decl, the instance kinds are influenced
  /only/ by the header.

* We could treat Haskell-98 style data-instance decls differently, by
  taking the data constructors into account, since there are no GADT
  issues.  But we don't, for simplicity, and because it means you can
  understand the data type instance by looking only at the header.

* Newtypes can be declared in GADT syntax, but they can't do GADT-style
  specialisation, so like Haskell-98 definitions we could take the
  data constructors into account.  Again we don't, for the same reason.

So for now at least, we keep the simplest choice. See #18891 and !4419
for more discussion of this issue.

Kind inference for data types (Xie et al) https://arxiv.org/abs/1911.06153
takes a slightly different approach.
-}


{- *********************************************************************
*                                                                      *
      Class instance declarations, pass 2
*                                                                      *
********************************************************************* -}

tcInstDecls2 :: [LTyClDecl GhcRn] -> [InstInfo GhcRn] -> ClassScopedTVEnv
             -> TcM (LHsBinds GhcTc)
-- (a) From each class declaration,
--      generate any default-method bindings
-- (b) From each instance decl
--      generate the dfun binding

tcInstDecls2 tycl_decls inst_decls class_scoped_tv_env
  = do  { -- (a) Default methods from class decls
          let class_decls = filter (isClassDecl . unLoc) tycl_decls
        ; dm_binds_s <- mapM (tcClassDecl2 class_scoped_tv_env) class_decls
        ; let dm_binds = unionManyBags dm_binds_s

          -- (b) instance declarations
        ; let dm_ids = collectHsBindsBinders CollNoDictBinders dm_binds
              -- Add the default method Ids (again)
              -- (they were already added in GHC.Tc.TyCl.Utils.tcAddImplicits)
              -- See Note [Default methods in the type environment]
        ; inst_binds_s <- tcExtendGlobalValEnv dm_ids $
                          mapM tcInstDecl2 inst_decls

          -- Done
        ; return (dm_binds `unionBags` unionManyBags inst_binds_s) }

{- Note [Default methods in the type environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The default method Ids are already in the type environment (see Note
[Default method Ids and Template Haskell] in TcTyDcls), BUT they
don't have their InlinePragmas yet.  Usually that would not matter,
because the simplifier propagates information from binding site to
use.  But, unusually, when compiling instance decls we *copy* the
INLINE pragma from the default method to the method for that
particular operation (see Note [INLINE and default methods] below).

So right here in tcInstDecls2 we must re-extend the type envt with
the default method Ids replete with their INLINE pragmas.  Urk.
-}

tcInstDecl2 :: InstInfo GhcRn -> TcM (LHsBinds GhcTc)
            -- Returns a binding for the dfun
tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = ibinds })
  = recoverM (return emptyLHsBinds)             $
    setSrcSpan loc                              $
    addErrCtxt (instDeclCtxt2 (idType dfun_id)) $
    do {  -- Instantiate the instance decl with skolem constants
       ; (inst_tyvars, dfun_theta, inst_head) <- tcSkolDFunType InstSkol dfun_id
       ; dfun_ev_vars <- newEvVars dfun_theta
                     -- We instantiate the dfun_id with superSkolems.
                     -- See Note [Subtle interaction of recursion and overlap]
                     -- and Note [Binding when looking up instances]

       ; let (clas, inst_tys) = tcSplitDFunHead inst_head
             (class_tyvars, sc_theta, _, op_items) = classBigSig clas
             sc_theta' = substTheta (zipTvSubst class_tyvars inst_tys) sc_theta

       ; traceTc "tcInstDecl2" (vcat [ppr inst_tyvars, ppr inst_tys, ppr dfun_theta, ppr sc_theta'])

                      -- Deal with 'SPECIALISE instance' pragmas
                      -- See Note [SPECIALISE instance pragmas]
       ; spec_inst_info@(spec_inst_prags,_) <- tcSpecInstPrags dfun_id ibinds

         -- Typecheck superclasses and methods
         -- See Note [Typechecking plan for instance declarations]
       ; dfun_ev_binds_var <- newTcEvBinds
       ; let dfun_ev_binds = TcEvBinds dfun_ev_binds_var
       ; (tclvl, (sc_meth_ids, sc_meth_binds, sc_meth_implics))
             <- pushTcLevelM $
                do { (sc_ids, sc_binds, sc_implics)
                        <- tcSuperClasses dfun_id clas inst_tyvars dfun_ev_vars
                                          inst_tys dfun_ev_binds
                                          sc_theta'

                      -- Typecheck the methods
                   ; (meth_ids, meth_binds, meth_implics)
                        <- tcMethods dfun_id clas inst_tyvars dfun_ev_vars
                                     inst_tys dfun_ev_binds spec_inst_info
                                     op_items ibinds

                   ; return ( sc_ids     ++          meth_ids
                            , sc_binds   `unionBags` meth_binds
                            , sc_implics `unionBags` meth_implics ) }

       ; imp <- newImplication
       ; emitImplication $
         imp { ic_tclvl  = tclvl
             , ic_skols  = inst_tyvars
             , ic_given  = dfun_ev_vars
             , ic_wanted = mkImplicWC sc_meth_implics
             , ic_binds  = dfun_ev_binds_var
             , ic_info   = InstSkol }

       -- Create the result bindings
       ; self_dict <- newDict clas inst_tys
       ; let class_tc      = classTyCon clas
             loc'          = noAnnSrcSpan loc
             [dict_constr] = tyConDataCons class_tc
             dict_bind = mkVarBind self_dict (L loc' con_app_args)

                     -- We don't produce a binding for the dict_constr; instead we
                     -- rely on the simplifier to unfold this saturated application
                     -- We do this rather than generate an HsCon directly, because
                     -- it means that the special cases (e.g. dictionary with only one
                     -- member) are dealt with by the common MkId.mkDataConWrapId
                     -- code rather than needing to be repeated here.
                     --    con_app_tys  = MkD ty1 ty2
                     --    con_app_scs  = MkD ty1 ty2 sc1 sc2
                     --    con_app_args = MkD ty1 ty2 sc1 sc2 op1 op2
             con_app_tys  = mkHsWrap (mkWpTyApps inst_tys) $
                            mkConLikeTc (RealDataCon dict_constr)
                       -- NB: We *can* have covars in inst_tys, in the case of
                       -- promoted GADT constructors.

             con_app_args = foldl' app_to_meth con_app_tys sc_meth_ids

             app_to_meth :: HsExpr GhcTc -> Id -> HsExpr GhcTc
             app_to_meth fun meth_id = HsApp noComments (L loc' fun)
                                            (L loc' (wrapId arg_wrapper meth_id))

             inst_tv_tys = mkTyVarTys inst_tyvars
             arg_wrapper = mkWpEvVarApps dfun_ev_vars <.> mkWpTyApps inst_tv_tys

             is_newtype = isNewTyCon class_tc
             dfun_id_w_prags = addDFunPrags dfun_id sc_meth_ids
             dfun_spec_prags
                | is_newtype = SpecPrags []
                | otherwise  = SpecPrags spec_inst_prags
                    -- Newtype dfuns just inline unconditionally,
                    -- so don't attempt to specialise them

             export = ABE { abe_ext  = noExtField
                          , abe_wrap = idHsWrapper
                          , abe_poly = dfun_id_w_prags
                          , abe_mono = self_dict
                          , abe_prags = dfun_spec_prags }
                          -- NB: see Note [SPECIALISE instance pragmas]
             main_bind = AbsBinds { abs_ext = noExtField
                                  , abs_tvs = inst_tyvars
                                  , abs_ev_vars = dfun_ev_vars
                                  , abs_exports = [export]
                                  , abs_ev_binds = []
                                  , abs_binds = unitBag dict_bind
                                  , abs_sig = True }

       ; return (unitBag (L loc' main_bind)
                  `unionBags` sc_meth_binds)
       }
 where
   dfun_id = instanceDFunId ispec
   loc     = getSrcSpan dfun_id

addDFunPrags :: DFunId -> [Id] -> DFunId
-- DFuns need a special Unfolding and InlinePrag
--    See Note [ClassOp/DFun selection]
--    and Note [Single-method classes]
-- It's easiest to create those unfoldings right here, where
-- have all the pieces in hand, even though we are messing with
-- Core at this point, which the typechecker doesn't usually do
-- However we take care to build the unfolding using the TyVars from
-- the DFunId rather than from the skolem pieces that the typechecker
-- is messing with.
addDFunPrags dfun_id sc_meth_ids
 | is_newtype
  = dfun_id `setIdUnfolding`  mkInlineUnfoldingWithArity 0 defaultSimpleOpts con_app
            `setInlinePragma` alwaysInlinePragma { inl_sat = Just 0 }
 | otherwise
 = dfun_id `setIdUnfolding`  mkDFunUnfolding dfun_bndrs dict_con dict_args
           `setInlinePragma` dfunInlinePragma
 where
   con_app    = mkLams dfun_bndrs $
                mkApps (Var (dataConWrapId dict_con)) dict_args
                -- This application will satisfy the Core invariants
                -- from Note [Representation polymorphism invariants] in GHC.Core,
                -- because typeclass method types are never unlifted.
   dict_args  = map Type inst_tys ++
                [mkVarApps (Var id) dfun_bndrs | id <- sc_meth_ids]

   (dfun_tvs, dfun_theta, clas, inst_tys) = tcSplitDFunTy (idType dfun_id)
   ev_ids      = mkTemplateLocalsNum 1                    dfun_theta
   dfun_bndrs  = dfun_tvs ++ ev_ids
   clas_tc     = classTyCon clas
   [dict_con]  = tyConDataCons clas_tc
   is_newtype  = isNewTyCon clas_tc

wrapId :: HsWrapper -> Id -> HsExpr GhcTc
wrapId wrapper id = mkHsWrap wrapper (HsVar noExtField (noLocA id))

{- Note [Typechecking plan for instance declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For instance declarations we generate the following bindings and implication
constraints.  Example:

   instance Ord a => Ord [a] where compare = <compare-rhs>

generates this:

   Bindings:
      -- Method bindings
      $ccompare :: forall a. Ord a => a -> a -> Ordering
      $ccompare = /\a \(d:Ord a). let <meth-ev-binds> in ...

      -- Superclass bindings
      $cp1Ord :: forall a. Ord a => Eq [a]
      $cp1Ord = /\a \(d:Ord a). let <sc-ev-binds>
               in dfEqList (dw :: Eq a)

   Constraints:
      forall a. Ord a =>
                -- Method constraint
             (forall. (empty) => <constraints from compare-rhs>)
                -- Superclass constraint
          /\ (forall. (empty) => dw :: Eq a)

Notice that

 * Per-meth/sc implication.  There is one inner implication per
   superclass or method, with no skolem variables or givens.  The only
   reason for this one is to gather the evidence bindings privately
   for this superclass or method.  This implication is generated
   by checkInstConstraints.

 * Overall instance implication. There is an overall enclosing
   implication for the whole instance declaration, with the expected
   skolems and givens.  We need this to get the correct "redundant
   constraint" warnings, gathering all the uses from all the methods
   and superclasses.  See GHC.Tc.Solver Note [Tracking redundant
   constraints]

 * The given constraints in the outer implication may generate
   evidence, notably by superclass selection.  Since the method and
   superclass bindings are top-level, we want that evidence copied
   into *every* method or superclass definition.  (Some of it will
   be usused in some, but dead-code elimination will drop it.)

   We achieve this by putting the evidence variable for the overall
   instance implication into the AbsBinds for each method/superclass.
   Hence the 'dfun_ev_binds' passed into tcMethods and tcSuperClasses.
   (And that in turn is why the abs_ev_binds field of AbBinds is a
   [TcEvBinds] rather than simply TcEvBinds.

   This is a bit of a hack, but works very nicely in practice.

 * Note that if a method has a locally-polymorphic binding, there will
   be yet another implication for that, generated by tcPolyCheck
   in tcMethodBody. E.g.
          class C a where
            foo :: forall b. Ord b => blah


************************************************************************
*                                                                      *
      Type-checking superclasses
*                                                                      *
************************************************************************
-}

tcSuperClasses :: DFunId -> Class -> [TcTyVar] -> [EvVar] -> [TcType]
               -> TcEvBinds
               -> TcThetaType
               -> TcM ([EvVar], LHsBinds GhcTc, Bag Implication)
-- Make a new top-level function binding for each superclass,
-- something like
--    $Ordp1 :: forall a. Ord a => Eq [a]
--    $Ordp1 = /\a \(d:Ord a). dfunEqList a (sc_sel d)
--
-- See Note [Recursive superclasses] for why this is so hard!
-- In effect, we build a special-purpose solver for the first step
-- of solving each superclass constraint
tcSuperClasses dfun_id cls tyvars dfun_evs inst_tys dfun_ev_binds sc_theta
  = do { (ids, binds, implics) <- mapAndUnzip3M tc_super (zip sc_theta [fIRST_TAG..])
       ; return (ids, listToBag binds, listToBag implics) }
  where
    loc = getSrcSpan dfun_id
    size = sizeTypes inst_tys
    tc_super (sc_pred, n)
      = do { (sc_implic, ev_binds_var, sc_ev_tm)
                <- checkInstConstraints $ emitWanted (ScOrigin size) sc_pred

           ; sc_top_name  <- newName (mkSuperDictAuxOcc n (getOccName cls))
           ; sc_ev_id     <- newEvVar sc_pred
           ; addTcEvBind ev_binds_var $ mkWantedEvBind sc_ev_id sc_ev_tm
           ; let sc_top_ty = mkInfForAllTys tyvars $
                             mkPhiTy (map idType dfun_evs) sc_pred
                 sc_top_id = mkLocalId sc_top_name Many sc_top_ty
                 export = ABE { abe_ext  = noExtField
                              , abe_wrap = idHsWrapper
                              , abe_poly = sc_top_id
                              , abe_mono = sc_ev_id
                              , abe_prags = noSpecPrags }
                 local_ev_binds = TcEvBinds ev_binds_var
                 bind = AbsBinds { abs_ext      = noExtField
                                 , abs_tvs      = tyvars
                                 , abs_ev_vars  = dfun_evs
                                 , abs_exports  = [export]
                                 , abs_ev_binds = [dfun_ev_binds, local_ev_binds]
                                 , abs_binds    = emptyBag
                                 , abs_sig      = False }
           ; return (sc_top_id, L (noAnnSrcSpan loc) bind, sc_implic) }

-------------------
checkInstConstraints :: TcM result
                     -> TcM (Implication, EvBindsVar, result)
-- See Note [Typechecking plan for instance declarations]
checkInstConstraints thing_inside
  = do { (tclvl, wanted, result) <- pushLevelAndCaptureConstraints  $
                                    thing_inside

       ; ev_binds_var <- newTcEvBinds
       ; implic <- newImplication
       ; let implic' = implic { ic_tclvl  = tclvl
                              , ic_wanted = wanted
                              , ic_binds  = ev_binds_var
                              , ic_info   = InstSkol }

       ; return (implic', ev_binds_var, result) }

{-
Note [Recursive superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #3731, #4809, #5751, #5913, #6117, #6161, which all
describe somewhat more complicated situations, but ones
encountered in practice.

See also tests tcrun020, tcrun021, tcrun033, and #11427.

----- THE PROBLEM --------
The problem is that it is all too easy to create a class whose
superclass is bottom when it should not be.

Consider the following (extreme) situation:
        class C a => D a where ...
        instance D [a] => D [a] where ...   (dfunD)
        instance C [a] => C [a] where ...   (dfunC)
Although this looks wrong (assume D [a] to prove D [a]), it is only a
more extreme case of what happens with recursive dictionaries, and it
can, just about, make sense because the methods do some work before
recursing.

To implement the dfunD we must generate code for the superclass C [a],
which we had better not get by superclass selection from the supplied
argument:
       dfunD :: forall a. D [a] -> D [a]
       dfunD = \d::D [a] -> MkD (scsel d) ..

Otherwise if we later encounter a situation where
we have a [Wanted] dw::D [a] we might solve it thus:
     dw := dfunD dw
Which is all fine except that now ** the superclass C is bottom **!

The instance we want is:
       dfunD :: forall a. D [a] -> D [a]
       dfunD = \d::D [a] -> MkD (dfunC (scsel d)) ...

----- THE SOLUTION --------
The basic solution is simple: be very careful about using superclass
selection to generate a superclass witness in a dictionary function
definition.  More precisely:

  Superclass Invariant: in every class dictionary,
                        every superclass dictionary field
                        is non-bottom

To achieve the Superclass Invariant, in a dfun definition we can
generate a guaranteed-non-bottom superclass witness from:
  (sc1) one of the dictionary arguments itself (all non-bottom)
  (sc2) an immediate superclass of a smaller dictionary
  (sc3) a call of a dfun (always returns a dictionary constructor)

The tricky case is (sc2).  We proceed by induction on the size of
the (type of) the dictionary, defined by GHC.Tc.Validity.sizeTypes.
Let's suppose we are building a dictionary of size 3, and
suppose the Superclass Invariant holds of smaller dictionaries.
Then if we have a smaller dictionary, its immediate superclasses
will be non-bottom by induction.

What does "we have a smaller dictionary" mean?  It might be
one of the arguments of the instance, or one of its superclasses.
Here is an example, taken from CmmExpr:
       class Ord r => UserOfRegs r a where ...
(i1)   instance UserOfRegs r a => UserOfRegs r (Maybe a) where
(i2)   instance (Ord r, UserOfRegs r CmmReg) => UserOfRegs r CmmExpr where

For (i1) we can get the (Ord r) superclass by selection from (UserOfRegs r a),
since it is smaller than the thing we are building (UserOfRegs r (Maybe a).

But for (i2) that isn't the case, so we must add an explicit, and
perhaps surprising, (Ord r) argument to the instance declaration.

Here's another example from #6161:

       class       Super a => Duper a  where ...
       class Duper (Fam a) => Foo a    where ...
(i3)   instance Foo a => Duper (Fam a) where ...
(i4)   instance              Foo Float where ...

It would be horribly wrong to define
   dfDuperFam :: Foo a -> Duper (Fam a)  -- from (i3)
   dfDuperFam d = MkDuper (sc_sel1 (sc_sel2 d)) ...

   dfFooFloat :: Foo Float               -- from (i4)
   dfFooFloat = MkFoo (dfDuperFam dfFooFloat) ...

Now the Super superclass of Duper is definitely bottom!

This won't happen because when processing (i3) we can use the
superclasses of (Foo a), which is smaller, namely Duper (Fam a).  But
that is *not* smaller than the target so we can't take *its*
superclasses.  As a result the program is rightly rejected, unless you
add (Super (Fam a)) to the context of (i3).

Note [Solving superclass constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How do we ensure that every superclass witness is generated by
one of (sc1) (sc2) or (sc3) in Note [Recursive superclasses].
Answer:

  * Superclass "wanted" constraints have CtOrigin of (ScOrigin size)
    where 'size' is the size of the instance declaration. e.g.
          class C a => D a where...
          instance blah => D [a] where ...
    The wanted superclass constraint for C [a] has origin
    ScOrigin size, where size = size( D [a] ).

  * (sc1) When we rewrite such a wanted constraint, it retains its
    origin.  But if we apply an instance declaration, we can set the
    origin to (ScOrigin infinity), thus lifting any restrictions by
    making prohibitedSuperClassSolve return False.

  * (sc2) ScOrigin wanted constraints can't be solved from a
    superclass selection, except at a smaller type.  This test is
    implemented by GHC.Tc.Solver.Interact.prohibitedSuperClassSolve

  * The "given" constraints of an instance decl have CtOrigin
    GivenOrigin InstSkol.

  * When we make a superclass selection from InstSkol we use
    a CtOrigin of (InstSCOrigin size), where 'size' is the size of
    the constraint whose superclass we are taking.  And similarly
    when taking the superclass of an InstSCOrigin.  This is implemented
    in GHC.Tc.Solver.Canonical.mk_strict_superclasses (in the
    mk_given_loc helper function).

Note [Silent superclass arguments] (historical interest only)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB1: this note describes our *old* solution to the
     recursive-superclass problem. I'm keeping the Note
     for now, just as institutional memory.
     However, the code for silent superclass arguments
     was removed in late Dec 2014

NB2: the silent-superclass solution introduced new problems
     of its own, in the form of instance overlap.  Tests
     SilentParametersOverlapping, T5051, and T7862 are examples

NB3: the silent-superclass solution also generated tons of
     extra dictionaries.  For example, in monad-transformer
     code, when constructing a Monad dictionary you had to pass
     an Applicative dictionary; and to construct that you need
     a Functor dictionary. Yet these extra dictionaries were
     often never used.  Test T3064 compiled *far* faster after
     silent superclasses were eliminated.

Our solution to this problem "silent superclass arguments".  We pass
to each dfun some ``silent superclass arguments’’, which are the
immediate superclasses of the dictionary we are trying to
construct. In our example:
       dfun :: forall a. C [a] -> D [a] -> D [a]
       dfun = \(dc::C [a]) (dd::D [a]) -> DOrd dc ...
Notice the extra (dc :: C [a]) argument compared to the previous version.

This gives us:

     -----------------------------------------------------------
     DFun Superclass Invariant
     ~~~~~~~~~~~~~~~~~~~~~~~~
     In the body of a DFun, every superclass argument to the
     returned dictionary is
       either   * one of the arguments of the DFun,
       or       * constant, bound at top level
     -----------------------------------------------------------

This net effect is that it is safe to treat a dfun application as
wrapping a dictionary constructor around its arguments (in particular,
a dfun never picks superclasses from the arguments under the
dictionary constructor). No superclass is hidden inside a dfun
application.

The extra arguments required to satisfy the DFun Superclass Invariant
always come first, and are called the "silent" arguments.  You can
find out how many silent arguments there are using Id.dfunNSilent;
and then you can just drop that number of arguments to see the ones
that were in the original instance declaration.

DFun types are built (only) by MkId.mkDictFunId, so that is where we
decide what silent arguments are to be added.
-}

{-
************************************************************************
*                                                                      *
      Type-checking an instance method
*                                                                      *
************************************************************************

tcMethod
- Make the method bindings, as a [(NonRec, HsBinds)], one per method
- Remembering to use fresh Name (the instance method Name) as the binder
- Bring the instance method Ids into scope, for the benefit of tcInstSig
- Use sig_fn mapping instance method Name -> instance tyvars
- Ditto prag_fn
- Use tcValBinds to do the checking
-}

tcMethods :: DFunId -> Class
          -> [TcTyVar] -> [EvVar]
          -> [TcType]
          -> TcEvBinds
          -> ([LTcSpecPrag], TcPragEnv)
          -> [ClassOpItem]
          -> InstBindings GhcRn
          -> TcM ([Id], LHsBinds GhcTc, Bag Implication)
        -- The returned inst_meth_ids all have types starting
        --      forall tvs. theta => ...
tcMethods dfun_id clas tyvars dfun_ev_vars inst_tys
                  dfun_ev_binds (spec_inst_prags, prag_fn) op_items
                  (InstBindings { ib_binds      = binds
                                , ib_tyvars     = lexical_tvs
                                , ib_pragmas    = sigs
                                , ib_extensions = exts
                                , ib_derived    = is_derived })
  = tcExtendNameTyVarEnv (lexical_tvs `zip` tyvars) $
       -- The lexical_tvs scope over the 'where' part
    do { traceTc "tcInstMeth" (ppr sigs $$ ppr binds)
       ; checkMinimalDefinition
       ; checkMethBindMembership
       ; (ids, binds, mb_implics) <- set_exts exts $
                                     unset_warnings_deriving $
                                     mapAndUnzip3M tc_item op_items
       ; return (ids, listToBag binds, listToBag (catMaybes mb_implics)) }
  where
    set_exts :: [LangExt.Extension] -> TcM a -> TcM a
    set_exts es thing = foldr setXOptM thing es

    -- See Note [Avoid -Winaccessible-code when deriving]
    unset_warnings_deriving :: TcM a -> TcM a
    unset_warnings_deriving
      | is_derived = unsetWOptM Opt_WarnInaccessibleCode
      | otherwise  = id

    hs_sig_fn = mkHsSigFun sigs
    inst_loc  = getSrcSpan dfun_id

    ----------------------
    tc_item :: ClassOpItem -> TcM (Id, LHsBind GhcTc, Maybe Implication)
    tc_item (sel_id, dm_info)
      | Just (user_bind, bndr_loc, prags) <- findMethodBind (idName sel_id) binds prag_fn
      = tcMethodBody clas tyvars dfun_ev_vars inst_tys
                              dfun_ev_binds is_derived hs_sig_fn
                              spec_inst_prags prags
                              sel_id user_bind bndr_loc
      | otherwise
      = do { traceTc "tc_def" (ppr sel_id)
           ; tc_default sel_id dm_info }

    ----------------------
    tc_default :: Id -> DefMethInfo
               -> TcM (TcId, LHsBind GhcTc, Maybe Implication)

    tc_default sel_id (Just (dm_name, _))
      = do { (meth_bind, inline_prags) <- mkDefMethBind dfun_id clas sel_id dm_name
           ; tcMethodBody clas tyvars dfun_ev_vars inst_tys
                          dfun_ev_binds is_derived hs_sig_fn
                          spec_inst_prags inline_prags
                          sel_id meth_bind inst_loc }

    tc_default sel_id Nothing     -- No default method at all
      = do { traceTc "tc_def: warn" (ppr sel_id)
           ; (meth_id, _) <- mkMethIds clas tyvars dfun_ev_vars
                                       inst_tys sel_id
           ; dflags <- getDynFlags
           ; let meth_bind = mkVarBind meth_id $
                             mkLHsWrap lam_wrapper (error_rhs dflags)
           ; return (meth_id, meth_bind, Nothing) }
      where
        inst_loc' = noAnnSrcSpan inst_loc
        error_rhs dflags = L inst_loc'
                                 $ HsApp noComments error_fun (error_msg dflags)
        error_fun    = L inst_loc' $
                       wrapId (mkWpTyApps
                                [ getRuntimeRep meth_tau, meth_tau])
                              nO_METHOD_BINDING_ERROR_ID
        error_msg dflags = L inst_loc'
                                    (HsLit noComments (HsStringPrim NoSourceText
                                              (unsafeMkByteString (error_string dflags))))
        meth_tau     = classMethodInstTy sel_id inst_tys
        error_string dflags = showSDoc dflags
                              (hcat [ppr inst_loc, vbar, ppr sel_id ])
        lam_wrapper  = mkWpTyLams tyvars <.> mkWpLams dfun_ev_vars

    ----------------------
    -- Check if one of the minimal complete definitions is satisfied
    checkMinimalDefinition
      = whenIsJust (isUnsatisfied methodExists (classMinimalDef clas)) $
        warnUnsatisfiedMinimalDefinition

    methodExists meth = isJust (findMethodBind meth binds prag_fn)

    ----------------------
    -- Check if any method bindings do not correspond to the class.
    -- See Note [Mismatched class methods and associated type families].
    checkMethBindMembership
      = mapM_ (addErrTc . badMethodErr clas) mismatched_meths
      where
        bind_nms         = map unLoc $ collectMethodBinders binds
        cls_meth_nms     = map (idName . fst) op_items
        mismatched_meths = bind_nms `minusList` cls_meth_nms

{-
Note [Mismatched class methods and associated type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's entirely possible for someone to put methods or associated type family
instances inside of a class in which it doesn't belong. For instance, we'd
want to fail if someone wrote this:

  instance Eq () where
    type Rep () = Maybe
    compare = undefined

Since neither the type family `Rep` nor the method `compare` belong to the
class `Eq`. Normally, this is caught in the renamer when resolving RdrNames,
since that would discover that the parent class `Eq` is incorrect.

However, there is a scenario in which the renamer could fail to catch this:
if the instance was generated through Template Haskell, as in #12387. In that
case, Template Haskell will provide fully resolved names (e.g.,
`GHC.Classes.compare`), so the renamer won't notice the sleight-of-hand going
on. For this reason, we also put an extra validity check for this in the
typechecker as a last resort.

Note [Avoid -Winaccessible-code when deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Winaccessible-code can be particularly noisy when deriving instances for
GADTs. Consider the following example (adapted from #8128):

  data T a where
    MkT1 :: Int -> T Int
    MkT2 :: T Bool
    MkT3 :: T Bool
  deriving instance Eq (T a)
  deriving instance Ord (T a)

In the derived Ord instance, GHC will generate the following code:

  instance Ord (T a) where
    compare x y
      = case x of
          MkT2
            -> case y of
                 MkT1 {} -> GT
                 MkT2    -> EQ
                 _       -> LT
          ...

However, that MkT1 is unreachable, since the type indices for MkT1 and MkT2
differ, so if -Winaccessible-code is enabled, then deriving this instance will
result in unwelcome warnings.

One conceivable approach to fixing this issue would be to change `deriving Ord`
such that it becomes smarter about not generating unreachable cases. This,
however, would be a highly nontrivial refactor, as we'd have to propagate
through typing information everywhere in the algorithm that generates Ord
instances in order to determine which cases were unreachable. This seems like
a lot of work for minimal gain, so we have opted not to go for this approach.

Instead, we take the much simpler approach of always disabling
-Winaccessible-code for derived code. To accomplish this, we do the following:

1. In tcMethods (which typechecks method bindings), disable
   -Winaccessible-code.
2. When creating Implications during typechecking, record this flag
   (in ic_warn_inaccessible) at the time of creation.
3. After typechecking comes error reporting, where GHC must decide how to
   report inaccessible code to the user, on an Implication-by-Implication
   basis. If an Implication's DynFlags indicate that -Winaccessible-code was
   disabled, then don't bother reporting it. That's it!
-}

------------------------
tcMethodBody :: Class -> [TcTyVar] -> [EvVar] -> [TcType]
             -> TcEvBinds -> Bool
             -> HsSigFun
             -> [LTcSpecPrag] -> [LSig GhcRn]
             -> Id -> LHsBind GhcRn -> SrcSpan
             -> TcM (TcId, LHsBind GhcTc, Maybe Implication)
tcMethodBody clas tyvars dfun_ev_vars inst_tys
                     dfun_ev_binds is_derived
                     sig_fn spec_inst_prags prags
                     sel_id (L bind_loc meth_bind) bndr_loc
  = add_meth_ctxt $
    do { traceTc "tcMethodBody" (ppr sel_id <+> ppr (idType sel_id) $$ ppr bndr_loc)
       ; (global_meth_id, local_meth_id) <- setSrcSpan bndr_loc $
                                            mkMethIds clas tyvars dfun_ev_vars
                                                      inst_tys sel_id

       ; let lm_bind = meth_bind { fun_id = L (noAnnSrcSpan bndr_loc)
                                                        (idName local_meth_id) }
                       -- Substitute the local_meth_name for the binder
                       -- NB: the binding is always a FunBind

            -- taking instance signature into account might change the type of
            -- the local_meth_id
       ; (meth_implic, ev_binds_var, tc_bind)
             <- checkInstConstraints $
                tcMethodBodyHelp sig_fn sel_id local_meth_id (L bind_loc lm_bind)

       ; global_meth_id <- addInlinePrags global_meth_id prags
       ; spec_prags     <- tcSpecPrags global_meth_id prags

        ; let specs  = mk_meth_spec_prags global_meth_id spec_inst_prags spec_prags
              export = ABE { abe_ext   = noExtField
                           , abe_poly  = global_meth_id
                           , abe_mono  = local_meth_id
                           , abe_wrap  = idHsWrapper
                           , abe_prags = specs }

              local_ev_binds = TcEvBinds ev_binds_var
              full_bind = AbsBinds { abs_ext      = noExtField
                                   , abs_tvs      = tyvars
                                   , abs_ev_vars  = dfun_ev_vars
                                   , abs_exports  = [export]
                                   , abs_ev_binds = [dfun_ev_binds, local_ev_binds]
                                   , abs_binds    = tc_bind
                                   , abs_sig      = True }

        ; return (global_meth_id, L bind_loc full_bind, Just meth_implic) }
  where
        -- For instance decls that come from deriving clauses
        -- we want to print out the full source code if there's an error
        -- because otherwise the user won't see the code at all
    add_meth_ctxt thing
      | is_derived = addLandmarkErrCtxt (derivBindCtxt sel_id clas inst_tys) thing
      | otherwise  = thing

tcMethodBodyHelp :: HsSigFun -> Id -> TcId
                 -> LHsBind GhcRn -> TcM (LHsBinds GhcTc)
tcMethodBodyHelp hs_sig_fn sel_id local_meth_id meth_bind
  | Just hs_sig_ty <- hs_sig_fn sel_name
              -- There is a signature in the instance
              -- See Note [Instance method signatures]
  = do { (sig_ty, hs_wrap)
             <- setSrcSpan (getLocA hs_sig_ty) $
                do { inst_sigs <- xoptM LangExt.InstanceSigs
                   ; checkTc inst_sigs (misplacedInstSig sel_name hs_sig_ty)
                   ; let ctxt = FunSigCtxt sel_name NoRRC
                   ; sig_ty  <- tcHsSigType ctxt hs_sig_ty
                   ; let local_meth_ty = idType local_meth_id
                                -- False <=> do not report redundant constraints when
                                --           checking instance-sig <= class-meth-sig
                                -- The instance-sig is the focus here; the class-meth-sig
                                -- is fixed (#18036)
                   ; hs_wrap <- addErrCtxtM (methSigCtxt sel_name sig_ty local_meth_ty) $
                                tcSubTypeSigma ctxt sig_ty local_meth_ty
                   ; return (sig_ty, hs_wrap) }

       ; inner_meth_name <- newName (nameOccName sel_name)
       ; let ctxt = FunSigCtxt sel_name (lhsSigTypeContextSpan hs_sig_ty)
                    -- WantRCC <=> check for redundant constraints in the
                    --          user-specified instance signature
             inner_meth_id  = mkLocalId inner_meth_name Many sig_ty
             inner_meth_sig = CompleteSig { sig_bndr = inner_meth_id
                                          , sig_ctxt = ctxt
                                          , sig_loc  = getLocA hs_sig_ty }


       ; (tc_bind, [inner_id]) <- tcPolyCheck no_prag_fn inner_meth_sig meth_bind

       ; let export = ABE { abe_ext   = noExtField
                          , abe_poly  = local_meth_id
                          , abe_mono  = inner_id
                          , abe_wrap  = hs_wrap
                          , abe_prags = noSpecPrags }

       ; return (unitBag $ L (getLoc meth_bind) $
                 AbsBinds { abs_ext = noExtField, abs_tvs = [], abs_ev_vars = []
                          , abs_exports = [export]
                          , abs_binds = tc_bind, abs_ev_binds = []
                          , abs_sig = True }) }

  | otherwise  -- No instance signature
  = do { let ctxt = FunSigCtxt sel_name NoRRC
                    -- NoRRC <=> don't report redundant constraints
                    -- The signature is not under the users control!
             tc_sig = completeSigFromId ctxt local_meth_id
              -- Absent a type sig, there are no new scoped type variables here
              -- Only the ones from the instance decl itself, which are already
              -- in scope.  Example:
              --      class C a where { op :: forall b. Eq b => ... }
              --      instance C [c] where { op = <rhs> }
              -- In <rhs>, 'c' is scope but 'b' is not!

       ; (tc_bind, _) <- tcPolyCheck no_prag_fn tc_sig meth_bind
       ; return tc_bind }

  where
    sel_name   = idName sel_id
    no_prag_fn = emptyPragEnv   -- No pragmas for local_meth_id;
                                -- they are all for meth_id

------------------------
mkMethIds :: Class -> [TcTyVar] -> [EvVar]
          -> [TcType] -> Id -> TcM (TcId, TcId)
             -- returns (poly_id, local_id), but ignoring any instance signature
             -- See Note [Instance method signatures]
mkMethIds clas tyvars dfun_ev_vars inst_tys sel_id
  = do  { poly_meth_name  <- newName (mkClassOpAuxOcc sel_occ)
        ; local_meth_name <- newName sel_occ
                  -- Base the local_meth_name on the selector name, because
                  -- type errors from tcMethodBody come from here
        ; let poly_meth_id  = mkLocalId poly_meth_name  Many poly_meth_ty
              local_meth_id = mkLocalId local_meth_name Many local_meth_ty

        ; return (poly_meth_id, local_meth_id) }
  where
    sel_name      = idName sel_id
    -- Force so that a thunk doesn't end up in a Name (#19619)
    !sel_occ      = nameOccName sel_name
    local_meth_ty = instantiateMethod clas sel_id inst_tys
    poly_meth_ty  = mkSpecSigmaTy tyvars theta local_meth_ty
    theta         = map idType dfun_ev_vars

methSigCtxt :: Name -> TcType -> TcType -> TidyEnv -> TcM (TidyEnv, SDoc)
methSigCtxt sel_name sig_ty meth_ty env0
  = do { (env1, sig_ty)  <- zonkTidyTcType env0 sig_ty
       ; (env2, meth_ty) <- zonkTidyTcType env1 meth_ty
       ; let msg = hang (text "When checking that instance signature for" <+> quotes (ppr sel_name))
                      2 (vcat [ text "is more general than its signature in the class"
                              , text "Instance sig:" <+> ppr sig_ty
                              , text "   Class sig:" <+> ppr meth_ty ])
       ; return (env2, msg) }

misplacedInstSig :: Name -> LHsSigType GhcRn -> TcRnMessage
misplacedInstSig name hs_ty
  = TcRnUnknownMessage $ mkPlainError noHints $
    vcat [ hang (text "Illegal type signature in instance declaration:")
              2 (hang (pprPrefixName name)
                    2 (dcolon <+> ppr hs_ty))
         , text "(Use InstanceSigs to allow this)" ]

{- Note [Instance method signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With -XInstanceSigs we allow the user to supply a signature for the
method in an instance declaration.  Here is an artificial example:

       data T a = MkT a
       instance Ord a => Ord (T a) where
         (>) :: forall b. b -> b -> Bool
         (>) = error "You can't compare Ts"

The instance signature can be *more* polymorphic than the instantiated
class method (in this case: Age -> Age -> Bool), but it cannot be less
polymorphic.  Moreover, if a signature is given, the implementation
code should match the signature, and type variables bound in the
singature should scope over the method body.

We achieve this by building a TcSigInfo for the method, whether or not
there is an instance method signature, and using that to typecheck
the declaration (in tcMethodBody).  That means, conveniently,
that the type variables bound in the signature will scope over the body.

What about the check that the instance method signature is more
polymorphic than the instantiated class method type?  We just do a
tcSubType call in tcMethodBodyHelp, and generate a nested AbsBind, like
this (for the example above

 AbsBind { abs_tvs = [a], abs_ev_vars = [d:Ord a]
         , abs_exports
             = ABExport { (>) :: forall a. Ord a => T a -> T a -> Bool
                        , gr_lcl :: T a -> T a -> Bool }
         , abs_binds
             = AbsBind { abs_tvs = [], abs_ev_vars = []
                       , abs_exports = ABExport { gr_lcl :: T a -> T a -> Bool
                                                , gr_inner :: forall b. b -> b -> Bool }
                       , abs_binds = AbsBind { abs_tvs = [b], abs_ev_vars = []
                                             , ..etc.. }
               } }

Wow!  Three nested AbsBinds!
 * The outer one abstracts over the tyvars and dicts for the instance
 * The middle one is only present if there is an instance signature,
   and does the impedance matching for that signature
 * The inner one is for the method binding itself against either the
   signature from the class, or the instance signature.
-}

----------------------
mk_meth_spec_prags :: Id -> [LTcSpecPrag] -> [LTcSpecPrag] -> TcSpecPrags
        -- Adapt the 'SPECIALISE instance' pragmas to work for this method Id
        -- There are two sources:
        --   * spec_prags_for_me: {-# SPECIALISE op :: <blah> #-}
        --   * spec_prags_from_inst: derived from {-# SPECIALISE instance :: <blah> #-}
        --     These ones have the dfun inside, but [perhaps surprisingly]
        --     the correct wrapper.
        -- See Note [Handling SPECIALISE pragmas] in GHC.Tc.Gen.Bind
mk_meth_spec_prags meth_id spec_inst_prags spec_prags_for_me
  = SpecPrags (spec_prags_for_me ++ spec_prags_from_inst)
  where
    spec_prags_from_inst
       | isInlinePragma (idInlinePragma meth_id)
       = []  -- Do not inherit SPECIALISE from the instance if the
             -- method is marked INLINE, because then it'll be inlined
             -- and the specialisation would do nothing. (Indeed it'll provoke
             -- a warning from the desugarer
       | otherwise
       = [ L inst_loc (SpecPrag meth_id wrap inl)
         | L inst_loc (SpecPrag _       wrap inl) <- spec_inst_prags]


mkDefMethBind :: DFunId -> Class -> Id -> Name
              -> TcM (LHsBind GhcRn, [LSig GhcRn])
-- The is a default method (vanailla or generic) defined in the class
-- So make a binding   op = $dmop @t1 @t2
-- where $dmop is the name of the default method in the class,
-- and t1,t2 are the instance types.
-- See Note [Default methods in instances] for why we use
-- visible type application here
mkDefMethBind dfun_id clas sel_id dm_name
  = do  { logger <- getLogger
        ; dm_id <- tcLookupId dm_name
        ; let inline_prag = idInlinePragma dm_id
              inline_prags | isAnyInlinePragma inline_prag
                           = [noLocA (InlineSig noAnn fn inline_prag)]
                           | otherwise
                           = []
                 -- Copy the inline pragma (if any) from the default method
                 -- to this version. Note [INLINE and default methods]

              fn   = noLocA (idName sel_id)
              visible_inst_tys = [ ty | (tcb, ty) <- tyConBinders (classTyCon clas) `zip` inst_tys
                                      , tyConBinderArgFlag tcb /= Inferred ]
              rhs  = foldl' mk_vta (nlHsVar dm_name) visible_inst_tys
              bind = noLocA $ mkTopFunBind Generated fn $
                             [mkSimpleMatch (mkPrefixFunRhs fn) [] rhs]

        ; liftIO (putDumpFileMaybe logger Opt_D_dump_deriv "Filling in method body"
                   FormatHaskell
                   (vcat [ppr clas <+> ppr inst_tys,
                          nest 2 (ppr sel_id <+> equals <+> ppr rhs)]))

       ; return (bind, inline_prags) }
  where
    (_, _, _, inst_tys) = tcSplitDFunTy (idType dfun_id)

    mk_vta :: LHsExpr GhcRn -> Type -> LHsExpr GhcRn
    mk_vta fun ty = noLocA (HsAppType noExtField fun (mkEmptyWildCardBndrs $ nlHsParTy
                                                $ noLocA $ XHsType ty))
       -- NB: use visible type application
       -- See Note [Default methods in instances]

----------------------
derivBindCtxt :: Id -> Class -> [Type ] -> SDoc
derivBindCtxt sel_id clas tys
   = vcat [ text "When typechecking the code for" <+> quotes (ppr sel_id)
          , nest 2 (text "in a derived instance for"
                    <+> quotes (pprClassPred clas tys) <> colon)
          , nest 2 $ text "To see the code I am typechecking, use -ddump-deriv" ]

warnUnsatisfiedMinimalDefinition :: ClassMinimalDef -> TcM ()
warnUnsatisfiedMinimalDefinition mindef
  = do { warn <- woptM Opt_WarnMissingMethods
       ; let msg = TcRnUnknownMessage $
               mkPlainDiagnostic (WarningWithFlag Opt_WarnMissingMethods) noHints message
       ; diagnosticTc warn msg
       }
  where
    message = vcat [text "No explicit implementation for"
                   ,nest 2 $ pprBooleanFormulaNice mindef
                   ]

{-
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

Notice that the type is ambiguous.  So we use Visible Type Application
to disambiguate:

   $dBazIntInt = MkBaz fooIntInt
   fooIntInt = $dmfoo @Int @Int

Lacking VTA we'd get ambiguity errors involving the default method.  This applies
equally to vanilla default methods (#1061) and generic default methods
(#12220).

Historical note: before we had VTA we had to generate
post-type-checked code, which took a lot more code, and didn't work for
generic default methods.

Note [INLINE and default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Default methods need special case.  They are supposed to behave rather like
macros.  For example

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

Note carefully:

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


************************************************************************
*                                                                      *
        Specialise instance pragmas
*                                                                      *
************************************************************************

Note [SPECIALISE instance pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   instance (Ix a, Ix b) => Ix (a,b) where
     {-# SPECIALISE instance Ix (Int,Int) #-}
     range (x,y) = ...

We make a specialised version of the dictionary function, AND
specialised versions of each *method*.  Thus we should generate
something like this:

  $dfIxPair :: (Ix a, Ix b) => Ix (a,b)
  {-# DFUN [$crangePair, ...] #-}
  {-# SPECIALISE $dfIxPair :: Ix (Int,Int) #-}
  $dfIxPair da db = Ix ($crangePair da db) (...other methods...)

  $crange :: (Ix a, Ix b) -> ((a,b),(a,b)) -> [(a,b)]
  {-# SPECIALISE $crange :: ((Int,Int),(Int,Int)) -> [(Int,Int)] #-}
  $crange da db = <blah>

The SPECIALISE pragmas are acted upon by the desugarer, which generate

  dii :: Ix Int
  dii = ...

  $s$dfIxPair :: Ix ((Int,Int),(Int,Int))
  {-# DFUN [$crangePair di di, ...] #-}
  $s$dfIxPair = Ix ($crangePair di di) (...)

  {-# RULE forall (d1,d2:Ix Int). $dfIxPair Int Int d1 d2 = $s$dfIxPair #-}

  $s$crangePair :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
  $c$crangePair = ...specialised RHS of $crangePair...

  {-# RULE forall (d1,d2:Ix Int). $crangePair Int Int d1 d2 = $s$crangePair #-}

Note that

  * The specialised dictionary $s$dfIxPair is very much needed, in case we
    call a function that takes a dictionary, but in a context where the
    specialised dictionary can be used.  See #7797.

  * The ClassOp rule for 'range' works equally well on $s$dfIxPair, because
    it still has a DFunUnfolding.  See Note [ClassOp/DFun selection]

  * A call (range ($dfIxPair Int Int d1 d2)) might simplify two ways:
       --> {ClassOp rule for range}     $crangePair Int Int d1 d2
       --> {SPEC rule for $crangePair}  $s$crangePair
    or thus:
       --> {SPEC rule for $dfIxPair}    range $s$dfIxPair
       --> {ClassOpRule for range}      $s$crangePair
    It doesn't matter which way.

  * We want to specialise the RHS of both $dfIxPair and $crangePair,
    but the SAME HsWrapper will do for both!  We can call tcSpecPrag
    just once, and pass the result (in spec_inst_info) to tcMethods.
-}

tcSpecInstPrags :: DFunId -> InstBindings GhcRn
                -> TcM ([LTcSpecPrag], TcPragEnv)
tcSpecInstPrags dfun_id (InstBindings { ib_binds = binds, ib_pragmas = uprags })
  = do { spec_inst_prags <- mapM (wrapLocAM (tcSpecInst dfun_id)) $
                            filter isSpecInstLSig uprags
             -- The filter removes the pragmas for methods
       ; return (spec_inst_prags, mkPragEnv uprags binds) }

------------------------------
tcSpecInst :: Id -> Sig GhcRn -> TcM TcSpecPrag
tcSpecInst dfun_id prag@(SpecInstSig _ _ hs_ty)
  = addErrCtxt (spec_ctxt prag) $
    do  { spec_dfun_ty <- tcHsClsInstType SpecInstCtxt hs_ty
        ; co_fn <- tcSpecWrapper SpecInstCtxt (idType dfun_id) spec_dfun_ty
        ; return (SpecPrag dfun_id co_fn defaultInlinePragma) }
  where
    spec_ctxt prag = hang (text "In the pragma:") 2 (ppr prag)

tcSpecInst _  _ = panic "tcSpecInst"

{-
************************************************************************
*                                                                      *
\subsection{Error messages}
*                                                                      *
************************************************************************
-}

instDeclCtxt1 :: LHsSigType GhcRn -> SDoc
instDeclCtxt1 hs_inst_ty
  = inst_decl_ctxt (ppr (getLHsInstDeclHead hs_inst_ty))

instDeclCtxt2 :: Type -> SDoc
instDeclCtxt2 dfun_ty
  = inst_decl_ctxt (ppr (mkClassPred cls tys))
  where
    (_,_,cls,tys) = tcSplitDFunTy dfun_ty

inst_decl_ctxt :: SDoc -> SDoc
inst_decl_ctxt doc = hang (text "In the instance declaration for")
                        2 (quotes doc)

badBootFamInstDeclErr :: TcRnMessage
badBootFamInstDeclErr
  = TcRnUnknownMessage $ mkPlainError noHints $ text "Illegal family instance in hs-boot file"

notFamily :: TyCon -> TcRnMessage
notFamily tycon
  = TcRnUnknownMessage $ mkPlainError noHints $
    vcat [ text "Illegal family instance for" <+> quotes (ppr tycon)
         , nest 2 $ parens (ppr tycon <+> text "is not an indexed type family")]

assocInClassErr :: TyCon -> TcRnMessage
assocInClassErr name
 = TcRnUnknownMessage $ mkPlainError noHints $
   text "Associated type" <+> quotes (ppr name) <+>
   text "must be inside a class instance"

badFamInstDecl :: TyCon -> TcRnMessage
badFamInstDecl tc_name
  = TcRnUnknownMessage $ mkPlainError noHints $
    vcat [ text "Illegal family instance for" <+>
           quotes (ppr tc_name)
         , nest 2 (parens $ text "Use TypeFamilies to allow indexed type families") ]

notOpenFamily :: TyCon -> TcRnMessage
notOpenFamily tc
  = TcRnUnknownMessage $ mkPlainError noHints $
  text "Illegal instance for closed family" <+> quotes (ppr tc)
