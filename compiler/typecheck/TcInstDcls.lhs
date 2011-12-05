%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcInstDecls: Typechecking instance declarations

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcInstDcls ( tcInstDecls1, tcInstDecls2 ) where

#include "HsVersions.h"

import HsSyn
import TcBinds
import TcTyClsDecls
import TcClassDcl
import TcPat      ( addInlinePrags )
import TcRnMonad
import TcMType
import TcType
import BuildTyCl
import Inst
import InstEnv
import FamInst
import FamInstEnv
import TcDeriv
import TcEnv
import TcHsType
import TcUnify
import MkCore     ( nO_METHOD_BINDING_ERROR_ID )
import Type
import TcEvidence
import TyCon
import DataCon
import Class
import Var
import VarEnv
import VarSet     ( mkVarSet, varSetElems )
import Pair
import CoreUnfold ( mkDFunUnfolding )
import CoreSyn    ( Expr(Var), CoreExpr, varToCoreExpr )
import PrelNames  ( typeableClassNames )

import Bag
import BasicTypes
import DynFlags
import FastString
import Id
import MkId
import Name
import NameSet
import Outputable
import SrcLoc
import Util

import Control.Monad
import Data.Maybe
import Maybes     ( orElse )
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
Trac #4138 for an example (although the regression reported there
wasn't due to the indirction).

There is an awkward wrinkle though: we want to be very
careful when we have
    instance C a => C [a] where
      {-# INLINE op #-}
      op = ...
then we'll get an INLINE pragma on $cop_list but it's important that
$cop_list only inlines when it's applied to *two* arguments (the
dictionary and the list argument).  So we nust not eta-expand $df
above.  We ensure that this doesn't happen by putting an INLINE
pragma on the dfun itself; after all, it ends up being just a cast.

There is one more dark corner to the INLINE story, even more deeply
buried.  Consider this (Trac #3772):

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
       ; idx_tycons        <- mapAndRecoverM tcTopFamInstDecl $
                              filter (isFamInstDecl . unLoc) tycl_decls
       ; local_info_tycons <- mapAndRecoverM tcLocalInstDecl1  inst_decls

       ; let { (local_info,
                at_tycons_s)   = unzip local_info_tycons
             ; at_idx_tycons   = concat at_tycons_s ++ idx_tycons
             ; at_things       = map ATyCon at_idx_tycons }

                -- (2) Add the tycons of indexed types and their implicit
                --     tythings to the global environment
       ; tcExtendGlobalEnvImplicit at_things $ do
       { tcg_env <- tcAddImplicits at_things
       ; setGblEnv tcg_env $


                -- Next, construct the instance environment so far, consisting
                -- of
                --   (a) local instance decls
                --   (b) local family instance decls
         addInsts local_info         $
         addFamInsts at_idx_tycons   $ do {

                -- (3) Compute instances from "deriving" clauses;
                -- This stuff computes a context for the derived instance
                -- decl, so it needs to know about all the instances possible
                -- NB: class instance declarations can contain derivings as
                --     part of associated data type declarations
         failIfErrsM    -- If the addInsts stuff gave any errors, don't
                        -- try the deriving stuff, because that may give
                        -- more errors still

       ; (gbl_env, deriv_inst_info, deriv_binds)
              <- tcDeriving tycl_decls inst_decls deriv_decls

       -- Check that if the module is compiled with -XSafe, there are no
       -- hand written instances of Typeable as then unsafe casts could be
       -- performed. Derived instances are OK.
       ; dflags <- getDOpts
       ; when (safeLanguageOn dflags) $
             mapM_ (\x -> when (typInstCheck x)
                               (addErrAt (getSrcSpan $ iSpec x) typInstErr))
                   local_info
       -- As above but for Safe Inference mode.
       ; when (safeInferOn dflags) $
             mapM_ (\x -> when (typInstCheck x) recordUnsafeInfer) local_info

       ; return ( gbl_env
                , (bagToList deriv_inst_info) ++ local_info
                , deriv_binds)
    }}}
  where
    typInstCheck ty = is_cls (iSpec ty) `elem` typeableClassNames
    typInstErr = ptext $ sLit $ "Can't create hand written instances of Typeable in Safe"
                              ++ " Haskell! Can only derive them"

addInsts :: [InstInfo Name] -> TcM a -> TcM a
addInsts infos thing_inside
  = tcExtendLocalInstEnv (map iSpec infos) thing_inside

addFamInsts :: [TyCon] -> TcM a -> TcM a
addFamInsts tycons thing_inside
  = tcExtendLocalFamInstEnv (map mkLocalFamInst tycons) thing_inside
\end{code}

\begin{code}
tcLocalInstDecl1 :: LInstDecl Name
                 -> TcM (InstInfo Name, [TyCon])
        -- A source-file instance declaration
        -- Type-check all the stuff before the "where"
        --
        -- We check for respectable instance type, and context
tcLocalInstDecl1 (L loc (InstDecl poly_ty binds uprags ats))
  = setSrcSpan loc                      $
    addErrCtxt (instDeclCtxt1 poly_ty)  $

    do  { is_boot <- tcIsHsBoot
        ; checkTc (not is_boot || (isEmptyLHsBinds binds && null uprags))
                  badBootDeclErr

        ; (tyvars, theta, clas, inst_tys) <- tcHsInstHead InstDeclCtxt poly_ty
        ; let mini_env = mkVarEnv (classTyVars clas `zip` inst_tys)

        -- Next, process any associated types.
        ; traceTc "tcLocalInstDecl" (ppr poly_ty)
        ; idx_tycons0 <- tcExtendTyVarEnv tyvars $
                         mapAndRecoverM (tcAssocDecl clas mini_env) ats

        -- Check for missing associated types and build them
        -- from their defaults (if available)
        ; let defined_ats = mkNameSet $ map (tcdName . unLoc) ats
              check_at_instance (fam_tc, defs)
                 -- User supplied instances ==> everything is OK
                | tyConName fam_tc `elemNameSet` defined_ats = return (Nothing, [])
                 -- No defaults ==> generate a warning
                | null defs                                  = return (Just (tyConName fam_tc), [])
                 -- No user instance, have defaults ==> instatiate them
                | otherwise = do
                    defs' <- forM defs $ \(ATD tvs pat_tys rhs _loc) -> do
                      let mini_env_subst = mkTvSubst (mkInScopeSet (mkVarSet tvs)) mini_env
                          tvs' = varSetElems (tyVarsOfType rhs')
                          pat_tys' = substTys mini_env_subst pat_tys
                          rhs' = substTy mini_env_subst rhs
                      rep_tc_name <- newFamInstTyConName (noLoc (tyConName fam_tc)) pat_tys'
                      buildSynTyCon rep_tc_name tvs'
                                    (SynonymTyCon rhs')
                                    (mkArrowKinds (map tyVarKind tvs') (typeKind rhs'))
                                    NoParentTyCon (Just (fam_tc, pat_tys'))
                    return (Nothing, defs')
        ; missing_at_stuff <- mapM check_at_instance (classATItems clas)
        
        ; let (omitted, idx_tycons1) = unzip missing_at_stuff
        ; warn <- woptM Opt_WarnMissingMethods
        ; mapM_ (warnTc warn . omittedATWarn) (catMaybes omitted)

        -- Finally, construct the Core representation of the instance.
        -- (This no longer includes the associated types.)
        ; dfun_name <- newDFunName clas inst_tys (getLoc poly_ty)
                -- Dfun location is that of instance *header*

        ; overlap_flag <- getOverlapFlag
        ; let dfun  	= mkDictFunId dfun_name tyvars theta clas inst_tys
              ispec 	= mkLocalInstance dfun overlap_flag
              inst_info = InstInfo { iSpec  = ispec, iBinds = VanillaInst binds uprags False }

        ; return (inst_info, idx_tycons0 ++ concat idx_tycons1) }
\end{code}


%************************************************************************
%*                                                                      *
               Type checking family instances
%*                                                                      *
%************************************************************************

Family instances are somewhat of a hybrid.  They are processed together with
class instance heads, but can contain data constructors and hence they share a
lot of kinding and type checking code with ordinary algebraic data types (and
GADTs).

\begin{code}
tcTopFamInstDecl :: LTyClDecl Name -> TcM TyCon
tcTopFamInstDecl (L loc decl)
  = setSrcSpan loc      $
    tcAddDeclCtxt decl  $
    tcFamInstDecl TopLevel decl

tcFamInstDecl :: TopLevelFlag -> TyClDecl Name -> TcM TyCon
tcFamInstDecl top_lvl decl
  = do { -- type family instances require -XTypeFamilies
         -- and can't (currently) be in an hs-boot file
       ; traceTc "tcFamInstDecl" (ppr decl)
       ; let fam_tc_lname = tcdLName decl
       ; type_families <- xoptM Opt_TypeFamilies
       ; is_boot <- tcIsHsBoot   -- Are we compiling an hs-boot file?
       ; checkTc type_families $ badFamInstDecl fam_tc_lname
       ; checkTc (not is_boot) $ badBootFamInstDeclErr

       -- Look up the family TyCon and check for validity including
       -- check that toplevel type instances are not for associated types.
       ; fam_tc <- tcLookupLocatedTyCon fam_tc_lname
       ; checkTc (isFamilyTyCon fam_tc) (notFamily fam_tc)
       ; when (isTopLevel top_lvl && isTyConAssoc fam_tc)
              (addErr $ assocInClassErr fam_tc_lname)

         -- Now check the type/data instance itself
         -- This is where type and data decls are treated separately
       ; tc <- tcFamInstDecl1 fam_tc decl
       ; checkValidTyCon tc     -- Remember to check validity;
                                -- no recursion to worry about here

       ; return tc }

tcFamInstDecl1 :: TyCon -> TyClDecl Name -> TcM TyCon

  -- "type instance"
tcFamInstDecl1 fam_tc (decl@TySynonym {})
  = do { -- (1) do the work of verifying the synonym
       ; (t_tvs, t_typats, t_rhs) <- tcSynFamInstDecl fam_tc decl

         -- (2) check the well-formedness of the instance
       ; checkValidFamInst t_typats t_rhs

         -- (3) construct representation tycon
       ; rep_tc_name <- newFamInstTyConName (tcdLName decl) t_typats
       ; buildSynTyCon rep_tc_name t_tvs
                       (SynonymTyCon t_rhs)
                       (typeKind t_rhs)
                       NoParentTyCon (Just (fam_tc, t_typats))
       }

  -- "newtype instance" and "data instance"
tcFamInstDecl1 fam_tc (decl@TyData { tcdND = new_or_data, tcdCtxt = ctxt
                                   , tcdTyVars = tvs, tcdTyPats = Just pats
                                  , tcdCons = cons})
  = do { -- Check that the family declaration is for the right kind
         checkTc (isFamilyTyCon fam_tc) (notFamily fam_tc)
       ; checkTc (isAlgTyCon fam_tc) (wrongKindOfFamily fam_tc)

         -- Kind check type patterns
       ; tcFamTyPats fam_tc tvs pats (\_always_star -> kcDataDecl decl) $ 
           \tvs' pats' resultKind -> do

         -- Check that left-hand side contains no type family applications
         -- (vanilla synonyms are fine, though, and we checked for
         -- foralls earlier)
       { mapM_ checkTyFamFreeness pats'
         
         -- Result kind must be '*' (otherwise, we have too few patterns)
       ; checkTc (isLiftedTypeKind resultKind) $ tooFewParmsErr (tyConArity fam_tc)

       ; stupid_theta <- tcHsKindedContext =<< kcHsContext ctxt
       ; dataDeclChecks (tcdName decl) new_or_data stupid_theta cons

         -- Construct representation tycon
       ; rep_tc_name <- newFamInstTyConName (tcdLName decl) pats'
       ; let ex_ok = True       -- Existentials ok for type families!
       ; fixM (\ rep_tycon -> do
             { let orig_res_ty = mkTyConApp fam_tc pats'
             ; data_cons <- tcConDecls new_or_data ex_ok rep_tycon
                                       (tvs', orig_res_ty) cons
             ; tc_rhs <-
                 case new_or_data of
                   DataType -> return (mkDataTyConRhs data_cons)
                   NewType  -> ASSERT( not (null data_cons) )
                               mkNewTyConRhs rep_tc_name rep_tycon (head data_cons)
             ; buildAlgTyCon rep_tc_name tvs' stupid_theta tc_rhs Recursive
                             h98_syntax NoParentTyCon (Just (fam_tc, pats'))
                 -- We always assume that indexed types are recursive.  Why?
                 -- (1) Due to their open nature, we can never be sure that a
                 -- further instance might not introduce a new recursive
                 -- dependency.  (2) They are always valid loop breakers as
                 -- they involve a coercion.
             })
       } }
    where
         h98_syntax = case cons of      -- All constructors have same shape
                        L _ (ConDecl { con_res = ResTyGADT _ }) : _ -> False
                        _ -> True

tcFamInstDecl1 _ d = pprPanic "tcFamInstDecl1" (ppr d)


----------------
tcAssocDecl :: Class           -- ^ Class of associated type
            -> VarEnv Type     -- ^ Instantiation of class TyVars
            -> LTyClDecl Name  -- ^ RHS
            -> TcM TyCon
tcAssocDecl clas mini_env (L loc decl)
  = setSrcSpan loc      $
    tcAddDeclCtxt decl  $
    do { at_tc <- tcFamInstDecl NotTopLevel decl
       ; let Just (fam_tc, at_tys) = tyConFamInst_maybe at_tc
  
       -- Check that the associated type comes from this class
       ; checkTc (Just clas == tyConAssoc_maybe fam_tc)
                 (badATErr (className clas) (tyConName at_tc))

       -- See Note [Checking consistent instantiation] in TcTyClsDecls
       ; zipWithM_ check_arg (tyConTyVars fam_tc) at_tys

       ; return at_tc }
  where
    check_arg fam_tc_tv at_ty
      | Just inst_ty <- lookupVarEnv mini_env fam_tc_tv
      = checkTc (inst_ty `eqType` at_ty) 
                (wrongATArgErr at_ty inst_ty)
      | otherwise
      = return ()   -- Allow non-type-variable instantiation
                    -- See Note [Associated type instances]
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
        ; inst_binds_s <- tcExtendLetEnv TopLevel dm_ids $
                          mapM tcInstDecl2 inst_decls

          -- Done
        ; return (dm_binds `unionBags` unionManyBags inst_binds_s) }
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

So right here in tcInstDecls2 we must re-extend the type envt with
the default method Ids replete with their INLINE pragmas.  Urk.

\begin{code}

tcInstDecl2 :: InstInfo Name -> TcM (LHsBinds Id)
            -- Returns a binding for the dfun
tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = ibinds })
  = recoverM (return emptyLHsBinds)             $
    setSrcSpan loc                              $
    addErrCtxt (instDeclCtxt2 (idType dfun_id)) $
    do {  -- Instantiate the instance decl with skolem constants
       ; (inst_tyvars, dfun_theta, inst_head) <- tcSkolDFunType (idType dfun_id)
                     -- We instantiate the dfun_id with superSkolems.
                     -- See Note [Subtle interaction of recursion and overlap]
                     -- and Note [Binding when looking up instances]
       ; let (clas, inst_tys) = tcSplitDFunHead inst_head
             (class_tyvars, sc_theta, sc_sels, op_items) = classBigSig clas
             sc_theta' = substTheta (zipOpenTvSubst class_tyvars inst_tys) sc_theta
       ; dfun_ev_vars <- newEvVars dfun_theta

       ; (sc_args, sc_binds)
             <- mapAndUnzipM (tcSuperClass inst_tyvars dfun_ev_vars)
                              (sc_sels `zip` sc_theta')

       -- Deal with 'SPECIALISE instance' pragmas
       -- See Note [SPECIALISE instance pragmas]
       ; spec_info@(spec_inst_prags,_) <- tcSpecInstPrags dfun_id ibinds

        -- Typecheck the methods
       ; (meth_ids, meth_binds)
           <- tcExtendTyVarEnv inst_tyvars $
                -- The inst_tyvars scope over the 'where' part
                -- Those tyvars are inside the dfun_id's type, which is a bit
                -- bizarre, but OK so long as you realise it!
              tcInstanceMethods dfun_id clas inst_tyvars dfun_ev_vars
                                inst_tys spec_info
                                op_items ibinds

       -- Create the result bindings
       ; self_dict <- newDict clas inst_tys
       ; let class_tc      = classTyCon clas
             [dict_constr] = tyConDataCons class_tc
             dict_bind     = mkVarBind self_dict (L loc con_app_args)

                     -- We don't produce a binding for the dict_constr; instead we
                     -- rely on the simplifier to unfold this saturated application
                     -- We do this rather than generate an HsCon directly, because
                     -- it means that the special cases (e.g. dictionary with only one
                     -- member) are dealt with by the common MkId.mkDataConWrapId
                     -- code rather than needing to be repeated here.
                     --    con_app_tys  = MkD ty1 ty2
                     --    con_app_scs  = MkD ty1 ty2 sc1 sc2
                     --    con_app_args = MkD ty1 ty2 sc1 sc2 op1 op2
             con_app_tys  = wrapId (mkWpTyApps inst_tys)
                                   (dataConWrapId dict_constr)
             con_app_scs  = mkHsWrap (mkWpEvApps (map mk_sc_ev_term sc_args)) con_app_tys
             con_app_args = foldl mk_app con_app_scs $
                            map (wrapId arg_wrapper) meth_ids

             mk_app :: HsExpr Id -> HsExpr Id -> HsExpr Id
             mk_app fun arg = HsApp (L loc fun) (L loc arg)

             mk_sc_ev_term :: EvVar -> EvTerm
             mk_sc_ev_term sc
               | null inst_tv_tys
               , null dfun_ev_vars = EvId sc
               | otherwise         = EvDFunApp sc inst_tv_tys dfun_ev_vars

             inst_tv_tys    = mkTyVarTys inst_tyvars
             arg_wrapper = mkWpEvVarApps dfun_ev_vars <.> mkWpTyApps inst_tv_tys

                -- Do not inline the dfun; instead give it a magic DFunFunfolding
                -- See Note [ClassOp/DFun selection]
                -- See also note [Single-method classes]
             dfun_id_w_fun
                | isNewTyCon class_tc
                = dfun_id `setInlinePragma` alwaysInlinePragma { inl_sat = Just 0 }
                | otherwise
                = dfun_id `setIdUnfolding`  mkDFunUnfolding dfun_ty dfun_args
                          `setInlinePragma` dfunInlinePragma

             dfun_args :: [CoreExpr]
             dfun_args = map varToCoreExpr sc_args ++
                         map Var           meth_ids

             export = ABE { abe_wrap = idHsWrapper, abe_poly = dfun_id_w_fun
                          , abe_mono = self_dict, abe_prags = SpecPrags spec_inst_prags }
             main_bind = AbsBinds { abs_tvs = inst_tyvars
                                  , abs_ev_vars = dfun_ev_vars
                                  , abs_exports = [export]
                                  , abs_ev_binds = emptyTcEvBinds
                                  , abs_binds = unitBag dict_bind }

       ; return (unitBag (L loc main_bind) `unionBags`
                 listToBag meth_binds      `unionBags`
                 unionManyBags sc_binds)
       }
 where
   dfun_ty   = idType dfun_id
   dfun_id   = instanceDFunId ispec
   loc       = getSrcSpan dfun_id

------------------------------
tcSuperClass :: [TcTyVar] -> [EvVar]
             -> (Id, PredType)
             -> TcM (TcId, LHsBinds TcId)

-- Build a top level decl like
--      sc_op = /\a \d. let sc = ... in
--                      sc
-- and return sc_op, that binding

tcSuperClass tyvars ev_vars (sc_sel, sc_pred)
  = do { (ev_binds, sc_dict)
             <- newImplication InstSkol tyvars ev_vars $
                emitWanted ScOrigin sc_pred

       ; uniq <- newUnique
       ; let sc_op_ty   = mkForAllTys tyvars $ mkPiTypes ev_vars (varType sc_dict)
             sc_op_name = mkDerivedInternalName mkClassOpAuxOcc uniq
                                                (getName sc_sel)
             sc_op_id   = mkLocalId sc_op_name sc_op_ty
             sc_op_bind = mkVarBind sc_op_id (L noSrcSpan $ wrapId sc_wrapper sc_dict)
             sc_wrapper = mkWpTyLams tyvars
                          <.> mkWpLams ev_vars
                          <.> mkWpLet ev_binds

       ; return (sc_op_id, unitBag sc_op_bind) }

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
\end{code}

Note [Superclass loop avoidance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following (extreme) situation:
        class C a => D a where ...
        instance D [a] => D [a] where ...
Although this looks wrong (assume D [a] to prove D [a]), it is only a
more extreme case of what happens with recursive dictionaries, and it
can, just about, make sense because the methods do some work before
recursing.

To implement the dfun we must generate code for the superclass C [a],
which we had better not get by superclass selection from the supplied
argument:
       dfun :: forall a. D [a] -> D [a]
       dfun = \d::D [a] -> MkD (scsel d) ..

Rather, we want to get it by finding an instance for (C [a]).  We
achieve this by
    not making the superclasses of a "wanted"
    available for solving wanted constraints.

Test case SCLoop tests this fix.

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
        ; (tyvars, theta, clas, tys) <- tcHsInstHead SpecInstCtxt hs_ty
        ; let spec_dfun_ty = mkDictFunTy tyvars theta clas tys

        ; co_fn <- tcSubType (SpecPragOrigin name) SpecInstCtxt
                             (idType dfun_id) spec_dfun_ty
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
                  -> ([Located TcSpecPrag], PragFun)
                  -> [(Id, DefMeth)]
                  -> InstBindings Name
                  -> TcM ([Id], [LHsBind Id])
        -- The returned inst_meth_ids all have types starting
        --      forall tvs. theta => ...
tcInstanceMethods dfun_id clas tyvars dfun_ev_vars inst_tys
                  (spec_inst_prags, prag_fn)
                  op_items (VanillaInst binds _ standalone_deriv)
  = mapAndUnzipM tc_item op_items
  where
    ----------------------
    tc_item :: (Id, DefMeth) -> TcM (Id, LHsBind Id)
    tc_item (sel_id, dm_info)
      = case findMethodBind (idName sel_id) binds of
            Just user_bind -> tc_body sel_id standalone_deriv user_bind
            Nothing        -> traceTc "tc_def" (ppr sel_id) >> 
                              tc_default sel_id dm_info

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
                          tyvars dfun_ev_vars
                          meth_id1 local_meth_id meth_sig_fn
                          (mk_meth_spec_prags meth_id1 spec_prags)
                          rn_bind
           ; return (meth_id1, bind) }

    ----------------------
    tc_default :: Id -> DefMeth -> TcM (TcId, LHsBind Id)

    tc_default sel_id (GenDefMeth dm_name)
      = do { meth_bind <- mkGenericDefMethBind clas inst_tys sel_id dm_name
           ; tc_body sel_id False {- Not generated code? -} meth_bind }

    tc_default sel_id NoDefMeth     -- No default method at all
      = do { traceTc "tc_def: warn" (ppr sel_id)
           ; warnMissingMethod sel_id
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

    tc_default sel_id (DefMeth dm_name) -- A polymorphic default method
      = do {   -- Build the typechecked version directly,
                 -- without calling typecheck_method;
                 -- see Note [Default methods in instances]
                 -- Generate   /\as.\ds. let self = df as ds
                 --                      in $dm inst_tys self
                 -- The 'let' is necessary only because HsSyn doesn't allow
                 -- you to apply a function to a dictionary *expression*.

           ; self_dict <- newDict clas inst_tys
           ; let self_ev_bind = EvBind self_dict
                                (EvDFunApp dfun_id (mkTyVarTys tyvars) dfun_ev_vars)

           ; (meth_id, local_meth_id) <- mkMethIds clas tyvars dfun_ev_vars
                                                   inst_tys sel_id
           ; dm_id <- tcLookupId dm_name
           ; let dm_inline_prag = idInlinePragma dm_id
                 rhs = HsWrap (mkWpEvVarApps [self_dict] <.> mkWpTyApps inst_tys) $
                       HsVar dm_id

                 meth_bind = mkVarBind local_meth_id (L loc rhs)
                 meth_id1 = meth_id `setInlinePragma` dm_inline_prag
                        -- Copy the inline pragma (if any) from the default
                        -- method to this version. Note [INLINE and default methods]

                  
                 export = ABE { abe_wrap = idHsWrapper, abe_poly = meth_id1
                              , abe_mono = local_meth_id
                              , abe_prags = mk_meth_spec_prags meth_id1 [] }
                 bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars = dfun_ev_vars
                                 , abs_exports = [export]
                                 , abs_ev_binds = EvBinds (unitBag self_ev_bind)
                                 , abs_binds    = unitBag meth_bind }
             -- Default methods in an instance declaration can't have their own
             -- INLINE or SPECIALISE pragmas. It'd be possible to allow them, but
             -- currently they are rejected with
             --           "INLINE pragma lacks an accompanying binding"

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
    meth_sig_fn _ = Just ([],loc)       -- The 'Just' says "yes, there's a type sig"
        -- But there are no scoped type variables from local_method_id
        -- Only the ones from the instance decl itself, which are already
        -- in scope.  Example:
        --      class C a where { op :: forall b. Eq b => ... }
        --      instance C [c] where { op = <rhs> }
        -- In <rhs>, 'c' is scope but 'b' is not!

        -- For instance decls that come from standalone deriving clauses
        -- we want to print out the full source code if there's an error
        -- because otherwise the user won't see the code at all
    add_meth_ctxt sel_id generated_code rn_bind thing
      | generated_code = addLandmarkErrCtxt (derivBindCtxt sel_id clas inst_tys rn_bind) thing
      | otherwise      = thing


tcInstanceMethods dfun_id clas tyvars dfun_ev_vars inst_tys
                  _ op_items (NewTypeDerived coi _)

-- Running example:
--   class Show b => Foo a b where
--     op :: a -> b -> b
--   newtype N a = MkN (Tree [a])
--   deriving instance (Show p, Foo Int p) => Foo Int (N p)
--               -- NB: standalone deriving clause means
--               --     that the contex is user-specified
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
     Just (init_inst_tys, _) = snocView inst_tys
     rep_ty   = pFst (tcCoercionKind co)  -- [p]
     rep_pred = mkClassPred clas (init_inst_tys ++ [rep_ty])

     -- co : [p] ~ T p
     co = mkTcSymCo (mkTcInstCos coi (mkTyVarTys tyvars))

     ----------------
     tc_item :: (TcEvBinds, EvVar) -> (Id, DefMeth) -> TcM (TcId, LHsBind TcId)
     tc_item (rep_ev_binds, rep_d) (sel_id, _)
       = do { (meth_id, local_meth_id) <- mkMethIds clas tyvars dfun_ev_vars
                                                    inst_tys sel_id

            ; let meth_rhs  = wrapId (mk_op_wrapper sel_id rep_d) sel_id
                  meth_bind = mkVarBind local_meth_id (L loc meth_rhs)
                  export = ABE { abe_wrap = idHsWrapper, abe_poly = meth_id
                               , abe_mono = local_meth_id, abe_prags = noSpecPrags }
                  bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars = dfun_ev_vars
                                   , abs_exports = [export]
                                   , abs_ev_binds = rep_ev_binds
                                   , abs_binds = unitBag $ meth_bind }

            ; return (meth_id, L loc bind) }

     ----------------
     mk_op_wrapper :: Id -> EvVar -> HsWrapper
     mk_op_wrapper sel_id rep_d
       = WpCast (liftTcCoSubstWith sel_tvs (map mkTcReflCo init_inst_tys ++ [co])
                                   local_meth_ty)
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
--        , nest 2 $ pprSetDepth AllTheWay $ ppr bind ]

warnMissingMethod :: Id -> TcM ()
warnMissingMethod sel_id
  = do { warn <- woptM Opt_WarnMissingMethods
       ; traceTc "warn" (ppr sel_id <+> ppr warn <+> ppr (not (startsWithUnderscore (getOccName sel_id))))
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
                        HsForAllTy _ _ _ (L _ ty') -> ppr ty'
                        _                          -> ppr hs_inst_ty)     -- Don't expect this
instDeclCtxt2 :: Type -> SDoc
instDeclCtxt2 dfun_ty
  = inst_decl_ctxt (ppr (mkClassPred cls tys))
  where
    (_,_,cls,tys) = tcSplitDFunTy dfun_ty

inst_decl_ctxt :: SDoc -> SDoc
inst_decl_ctxt doc = ptext (sLit "In the instance declaration for") <+> quotes doc

omittedATWarn :: Name -> SDoc
omittedATWarn at
  = ptext (sLit "No explicit AT declaration for") <+> quotes (ppr at)

badBootFamInstDeclErr :: SDoc
badBootFamInstDeclErr
  = ptext (sLit "Illegal family instance in hs-boot file")

notFamily :: TyCon -> SDoc
notFamily tycon
  = vcat [ ptext (sLit "Illegal family instance for") <+> quotes (ppr tycon)
         , nest 2 $ parens (ppr tycon <+> ptext (sLit "is not an indexed type family"))]

tooFewParmsErr :: Arity -> SDoc
tooFewParmsErr arity
  = ptext (sLit "Family instance has too few parameters; expected") <+>
    ppr arity

assocInClassErr :: Located Name -> SDoc
assocInClassErr name
 = ptext (sLit "Associated type") <+> quotes (ppr name) <+>
   ptext (sLit "must be inside a class instance")

badFamInstDecl :: Located Name -> SDoc
badFamInstDecl tc_name
  = vcat [ ptext (sLit "Illegal family instance for") <+>
           quotes (ppr tc_name)
         , nest 2 (parens $ ptext (sLit "Use -XTypeFamilies to allow indexed type families")) ]
\end{code}
