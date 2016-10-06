{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Handles @deriving@ clauses on @data@ declarations.
-}

{-# LANGUAGE CPP #-}

module TcDeriv ( tcDeriving, DerivInfo(..), mkDerivInfos ) where

#include "HsVersions.h"

import HsSyn
import DynFlags

import TcRnMonad
import FamInst
import TcDerivInfer
import TcDerivUtils
import TcValidity( allDistinctTyVars )
import TcClassDcl( tcATDefault, tcMkDeclCtxt )
import TcEnv
import TcGenDeriv                       -- Deriv stuff
import InstEnv
import Inst
import FamInstEnv
import TcHsType
import TcMType

import RnNames( extendGlobalRdrEnvRn )
import RnBinds
import RnEnv
import RnSource   ( addTcgDUs )
import Avail

import Unify( tcUnifyTy )
import BasicTypes ( DerivStrategy(..) )
import Class
import Type
import ErrUtils
import DataCon
import Maybes
import RdrName
import Name
import NameSet
import TyCon
import TcType
import Var
import VarEnv
import VarSet
import PrelNames
import SrcLoc
import Util
import Outputable
import FastString
import Bag
import Pair
import FV (fvVarList, unionFV, mkFVs)
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.List

{-
************************************************************************
*                                                                      *
                Overview
*                                                                      *
************************************************************************

Overall plan
~~~~~~~~~~~~
1.  Convert the decls (i.e. data/newtype deriving clauses,
    plus standalone deriving) to [EarlyDerivSpec]

2.  Infer the missing contexts for the InferTheta's

3.  Add the derived bindings, generating InstInfos
-}

data EarlyDerivSpec = InferTheta (DerivSpec ThetaOrigin)
                    | GivenTheta (DerivSpec ThetaType)
        -- InferTheta ds => the context for the instance should be inferred
        --      In this case ds_theta is the list of all the constraints
        --      needed, such as (Eq [a], Eq a), together with a suitable CtLoc
        --      to get good error messages.
        --      The inference process is to reduce this to a
        --      simpler form (e.g. Eq a)
        --
        -- GivenTheta ds => the exact context for the instance is supplied
        --                  by the programmer; it is ds_theta
        -- See Note [Inferring the instance context] in TcDerivInfer

earlyDSLoc :: EarlyDerivSpec -> SrcSpan
earlyDSLoc (InferTheta spec) = ds_loc spec
earlyDSLoc (GivenTheta spec) = ds_loc spec

splitEarlyDerivSpec :: [EarlyDerivSpec] -> ([DerivSpec ThetaOrigin], [DerivSpec ThetaType])
splitEarlyDerivSpec [] = ([],[])
splitEarlyDerivSpec (InferTheta spec : specs) =
    case splitEarlyDerivSpec specs of (is, gs) -> (spec : is, gs)
splitEarlyDerivSpec (GivenTheta spec : specs) =
    case splitEarlyDerivSpec specs of (is, gs) -> (is, spec : gs)

instance Outputable EarlyDerivSpec where
  ppr (InferTheta spec) = ppr spec <+> text "(Infer)"
  ppr (GivenTheta spec) = ppr spec <+> text "(Given)"

{-
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
E.g.            newtype Foo a = Foo a deriving( Show, Num, Eq )
Then the Show instance is not done via Coercible; it shows
        Foo 3 as "Foo 3"
The Num instance is derived via Coercible, but the Show superclass
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

-}

-- | Stuff needed to process a datatype's `deriving` clauses
data DerivInfo = DerivInfo { di_rep_tc  :: TyCon
                             -- ^ The data tycon for normal datatypes,
                             -- or the *representation* tycon for data families
                           , di_clauses :: [LHsDerivingClause Name]
                           , di_ctxt    :: SDoc -- ^ error context
                           }

-- | Extract `deriving` clauses of proper data type (skips data families)
mkDerivInfos :: [LTyClDecl Name] -> TcM [DerivInfo]
mkDerivInfos decls = concatMapM (mk_deriv . unLoc) decls
  where

    mk_deriv decl@(DataDecl { tcdLName = L _ data_name
                            , tcdDataDefn =
                                HsDataDefn { dd_derivs = L _ clauses } })
      = do { tycon <- tcLookupTyCon data_name
           ; return [DerivInfo { di_rep_tc = tycon, di_clauses = clauses
                               , di_ctxt = tcMkDeclCtxt decl }] }
    mk_deriv _ = return []

{-

************************************************************************
*                                                                      *
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
*                                                                      *
************************************************************************
-}

tcDeriving  :: [DerivInfo]       -- All `deriving` clauses
            -> [LDerivDecl Name] -- All stand-alone deriving declarations
            -> TcM (TcGblEnv, Bag (InstInfo Name), HsValBinds Name)
tcDeriving deriv_infos deriv_decls
  = recoverM (do { g <- getGblEnv
                 ; return (g, emptyBag, emptyValBindsOut)}) $
    do  {       -- Fish the "deriving"-related information out of the TcEnv
                -- And make the necessary "equations".
          is_boot <- tcIsHsBootOrSig
        ; traceTc "tcDeriving" (ppr is_boot)

        ; early_specs <- makeDerivSpecs is_boot deriv_infos deriv_decls
        ; traceTc "tcDeriving 1" (ppr early_specs)

        ; let (infer_specs, given_specs) = splitEarlyDerivSpec early_specs
        ; insts1 <- mapM genInst given_specs

        -- the stand-alone derived instances (@insts1@) are used when inferring
        -- the contexts for "deriving" clauses' instances (@infer_specs@)
        ; final_specs <- extendLocalInstEnv (map (iSpec . fstOf3) insts1) $
                         simplifyInstanceContexts infer_specs

        ; insts2 <- mapM genInst final_specs

        ; let (inst_infos, deriv_stuff, maybe_fvs) = unzip3 (insts1 ++ insts2)
        ; loc <- getSrcSpanM
        ; let (binds, famInsts) = genAuxBinds loc (unionManyBags deriv_stuff)

        ; dflags <- getDynFlags

        ; (inst_info, rn_binds, rn_dus) <-
            renameDeriv is_boot inst_infos binds

        ; unless (isEmptyBag inst_info) $
             liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances"
                        (ddump_deriving inst_info rn_binds famInsts))

        ; gbl_env <- tcExtendLocalFamInstEnv (bagToList famInsts) $
                     tcExtendLocalInstEnv (map iSpec (bagToList inst_info)) getGblEnv
        ; let all_dus = rn_dus `plusDU` usesOnly (NameSet.mkFVs $ catMaybes maybe_fvs)
        ; return (addTcgDUs gbl_env all_dus, inst_info, rn_binds) }
  where
    ddump_deriving :: Bag (InstInfo Name) -> HsValBinds Name
                   -> Bag FamInst             -- ^ Rep type family instances
                   -> SDoc
    ddump_deriving inst_infos extra_binds repFamInsts
      =    hang (text "Derived instances:")
              2 (vcat (map (\i -> pprInstInfoDetails i $$ text "") (bagToList inst_infos))
                 $$ ppr extra_binds)
        $$ hangP "GHC.Generics representation types:"
             (vcat (map pprRepTy (bagToList repFamInsts)))

    hangP s x = text "" $$ hang (ptext (sLit s)) 2 x

-- Prints the representable type family instance
pprRepTy :: FamInst -> SDoc
pprRepTy fi@(FamInst { fi_tys = lhs })
  = text "type" <+> ppr (mkTyConApp (famInstTyCon fi) lhs) <+>
      equals <+> ppr rhs
  where rhs = famInstRHS fi

renameDeriv :: Bool
            -> [InstInfo RdrName]
            -> Bag (LHsBind RdrName, LSig RdrName)
            -> TcM (Bag (InstInfo Name), HsValBinds Name, DefUses)
renameDeriv is_boot inst_infos bagBinds
  | is_boot     -- If we are compiling a hs-boot file, don't generate any derived bindings
                -- The inst-info bindings will all be empty, but it's easier to
                -- just use rn_inst_info to change the type appropriately
  = do  { (rn_inst_infos, fvs) <- mapAndUnzipM rn_inst_info inst_infos
        ; return ( listToBag rn_inst_infos
                 , emptyValBindsOut, usesOnly (plusFVs fvs)) }

  | otherwise
  = discardWarnings $         -- Discard warnings about unused bindings etc
    setXOptM LangExt.EmptyCase $  -- Derived decls (for empty types) can have
                                  --    case x of {}
    setXOptM LangExt.ScopedTypeVariables $  -- Derived decls (for newtype-deriving) can
    setXOptM LangExt.KindSignatures $       -- used ScopedTypeVariables & KindSignatures
    do  {
        -- Bring the extra deriving stuff into scope
        -- before renaming the instances themselves
        ; traceTc "rnd" (vcat (map (\i -> pprInstInfoDetails i $$ text "") inst_infos))
        ; (aux_binds, aux_sigs) <- mapAndUnzipBagM return bagBinds
        ; let aux_val_binds = ValBindsIn aux_binds (bagToList aux_sigs)
        ; rn_aux_lhs <- rnTopBindsLHS emptyFsEnv aux_val_binds
        ; let bndrs = collectHsValBinders rn_aux_lhs
        ; envs <- extendGlobalRdrEnvRn (map avail bndrs) emptyFsEnv ;
        ; setEnvs envs $
    do  { (rn_aux, dus_aux) <- rnValBindsRHS (TopSigCtxt (mkNameSet bndrs)) rn_aux_lhs
        ; (rn_inst_infos, fvs_insts) <- mapAndUnzipM rn_inst_info inst_infos
        ; return (listToBag rn_inst_infos, rn_aux,
                  dus_aux `plusDU` usesOnly (plusFVs fvs_insts)) } }

  where
    rn_inst_info :: InstInfo RdrName -> TcM (InstInfo Name, FreeVars)
    rn_inst_info
      inst_info@(InstInfo { iSpec = inst
                          , iBinds = InstBindings
                            { ib_binds = binds
                            , ib_tyvars = tyvars
                            , ib_pragmas = sigs
                            , ib_extensions = exts -- Only for type-checking
                            , ib_derived = sa } })
        =  ASSERT( null sigs )
           bindLocalNamesFV tyvars $
           do { (rn_binds,_, fvs) <- rnMethodBinds False (is_cls_nm inst) [] binds []
              ; let binds' = InstBindings { ib_binds = rn_binds
                                          , ib_tyvars = tyvars
                                          , ib_pragmas = []
                                          , ib_extensions = exts
                                          , ib_derived = sa }
              ; return (inst_info { iBinds = binds' }, fvs) }

{-
Note [Newtype deriving and unused constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (see Trac #1954):

  module Bug(P) where
  newtype P a = MkP (IO a) deriving Monad

If you compile with -Wunused-binds you do not expect the warning
"Defined but not used: data consructor MkP". Yet the newtype deriving
code does not explicitly mention MkP, but it should behave as if you
had written
  instance Monad P where
     return x = MkP (return x)
     ...etc...

So we want to signal a user of the data constructor 'MkP'.
This is the reason behind the (Maybe Name) part of the return type
of genInst.

Note [Why we don't pass rep_tc into deriveTyData]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Down in the bowels of mkEqnHelp, we need to convert the fam_tc back into
the rep_tc by means of a lookup. And yet we have the rep_tc right here!
Why look it up again? Answer: it's just easier this way.
We drop some number of arguments from the end of the datatype definition
in deriveTyData. The arguments are dropped from the fam_tc.
This action may drop a *different* number of arguments
passed to the rep_tc, depending on how many free variables, etc., the
dropped patterns have.

Also, this technique carries over the kind substitution from deriveTyData
nicely.

************************************************************************
*                                                                      *
                From HsSyn to DerivSpec
*                                                                      *
************************************************************************

@makeDerivSpecs@ fishes around to find the info about needed derived instances.
-}

makeDerivSpecs :: Bool
               -> [DerivInfo]
               -> [LDerivDecl Name]
               -> TcM [EarlyDerivSpec]
makeDerivSpecs is_boot deriv_infos deriv_decls
  = do  { eqns1 <- concatMapM (recoverM (return []) . deriveDerivInfo)  deriv_infos
        ; eqns2 <- concatMapM (recoverM (return []) . deriveStandalone) deriv_decls
        ; let eqns = eqns1 ++ eqns2

        ; if is_boot then   -- No 'deriving' at all in hs-boot files
              do { unless (null eqns) (add_deriv_err (head eqns))
                 ; return [] }
          else return eqns }
  where
    add_deriv_err eqn
       = setSrcSpan (earlyDSLoc eqn) $
         addErr (hang (text "Deriving not permitted in hs-boot file")
                    2 (text "Use an instance declaration instead"))

------------------------------------------------------------------
-- | Process a `deriving` clause
deriveDerivInfo :: DerivInfo -> TcM [EarlyDerivSpec]
deriveDerivInfo (DerivInfo { di_rep_tc = rep_tc, di_clauses = clauses
                           , di_ctxt = err_ctxt })
  = addErrCtxt err_ctxt $
    concatMapM (deriveForClause . unLoc) clauses
  where
    tvs = tyConTyVars rep_tc
    (tc, tys) = case tyConFamInstSig_maybe rep_tc of
                        -- data family:
                  Just (fam_tc, pats, _) -> (fam_tc, pats)
      -- NB: deriveTyData wants the *user-specified*
      -- name. See Note [Why we don't pass rep_tc into deriveTyData]

                  _ -> (rep_tc, mkTyVarTys tvs)     -- datatype

    deriveForClause :: HsDerivingClause Name -> TcM [EarlyDerivSpec]
    deriveForClause (HsDerivingClause { deriv_clause_strategy = dcs
                                      , deriv_clause_tys      = L _ preds })
      = concatMapM (deriveTyData tvs tc tys (fmap unLoc dcs)) preds

------------------------------------------------------------------
deriveStandalone :: LDerivDecl Name -> TcM [EarlyDerivSpec]
-- Standalone deriving declarations
--  e.g.   deriving instance Show a => Show (T a)
-- Rather like tcLocalInstDecl
deriveStandalone (L loc (DerivDecl deriv_ty deriv_strat' overlap_mode))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do { traceTc "Standalone deriving decl for" (ppr deriv_ty)
       ; let deriv_strat = fmap unLoc deriv_strat'
       ; traceTc "Deriving strategy (standalone deriving)" $
           vcat [ppr deriv_strat, ppr deriv_ty]
       ; (tvs, theta, cls, inst_tys) <- tcHsClsInstType TcType.InstDeclCtxt deriv_ty
       ; traceTc "Standalone deriving;" $ vcat
              [ text "tvs:" <+> ppr tvs
              , text "theta:" <+> ppr theta
              , text "cls:" <+> ppr cls
              , text "tys:" <+> ppr inst_tys ]
                -- C.f. TcInstDcls.tcLocalInstDecl1
       ; checkTc (not (null inst_tys)) derivingNullaryErr

       ; let cls_tys = take (length inst_tys - 1) inst_tys
             inst_ty = last inst_tys
       ; traceTc "Standalone deriving:" $ vcat
              [ text "class:" <+> ppr cls
              , text "class types:" <+> ppr cls_tys
              , text "type:" <+> ppr inst_ty ]

       ; let bale_out msg = failWithTc (derivingThingErr False cls cls_tys
                              inst_ty deriv_strat msg)

       ; case tcSplitTyConApp_maybe inst_ty of
           Just (tc, tc_args)
              | className cls == typeableClassName
              -> do warnUselessTypeable
                    return []

              | isUnboxedTupleTyCon tc
              -> bale_out $ unboxedTyConErr "tuple"

              | isUnboxedSumTyCon tc
              -> bale_out $ unboxedTyConErr "sum"

              | isAlgTyCon tc || isDataFamilyTyCon tc  -- All other classes
              -> do { spec <- mkEqnHelp (fmap unLoc overlap_mode)
                                        tvs cls cls_tys tc tc_args
                                        (Just theta) deriv_strat
                    ; return [spec] }

           _  -> -- Complain about functions, primitive types, etc,
                 bale_out $
                 text "The last argument of the instance must be a data or newtype application"
        }

warnUselessTypeable :: TcM ()
warnUselessTypeable
  = do { warn <- woptM Opt_WarnDerivingTypeable
       ; when warn $ addWarnTc (Reason Opt_WarnDerivingTypeable)
                   $ text "Deriving" <+> quotes (ppr typeableClassName) <+>
                     text "has no effect: all types now auto-derive Typeable" }

------------------------------------------------------------------
deriveTyData :: [TyVar] -> TyCon -> [Type]   -- LHS of data or data instance
                                             --   Can be a data instance, hence [Type] args
             -> Maybe DerivStrategy          -- The optional deriving strategy
             -> LHsSigType Name              -- The deriving predicate
             -> TcM [EarlyDerivSpec]
-- The deriving clause of a data or newtype declaration
-- I.e. not standalone deriving
deriveTyData tvs tc tc_args deriv_strat deriv_pred
  = setSrcSpan (getLoc (hsSigType deriv_pred)) $  -- Use loc of the 'deriving' item
    do  { (deriv_tvs, cls, cls_tys, cls_arg_kinds)
                <- tcExtendTyVarEnv tvs $
                   tcHsDeriv deriv_pred
                -- Deriving preds may (now) mention
                -- the type variables for the type constructor, hence tcExtendTyVarenv
                -- The "deriv_pred" is a LHsType to take account of the fact that for
                -- newtype deriving we allow deriving (forall a. C [a]).

                -- Typeable is special, because Typeable :: forall k. k -> Constraint
                -- so the argument kind 'k' is not decomposable by splitKindFunTys
                -- as is the case for all other derivable type classes
        ; when (length cls_arg_kinds /= 1) $
            failWithTc (nonUnaryErr deriv_pred)
        ; let [cls_arg_kind] = cls_arg_kinds
        ; if className cls == typeableClassName
          then do warnUselessTypeable
                  return []
          else

     do {  -- Given data T a b c = ... deriving( C d ),
           -- we want to drop type variables from T so that (C d (T a)) is well-kinded
          let (arg_kinds, _)  = splitFunTys cls_arg_kind
              n_args_to_drop  = length arg_kinds
              n_args_to_keep  = tyConArity tc - n_args_to_drop
              (tc_args_to_keep, args_to_drop)
                              = splitAt n_args_to_keep tc_args
              inst_ty_kind    = typeKind (mkTyConApp tc tc_args_to_keep)

              -- Match up the kinds, and apply the resulting kind substitution
              -- to the types.  See Note [Unify kinds in deriving]
              -- We are assuming the tycon tyvars and the class tyvars are distinct
              mb_match        = tcUnifyTy inst_ty_kind cls_arg_kind
              enough_args     = n_args_to_keep >= 0

        -- Check that the result really is well-kinded
        ; checkTc (enough_args && isJust mb_match)
                  (derivingKindErr tc cls cls_tys cls_arg_kind enough_args)

        ; let Just kind_subst = mb_match
              ki_subst_range  = getTCvSubstRangeFVs kind_subst
              all_tkvs        = toposortTyVars $
                                fvVarList $ unionFV
                                  (tyCoFVsOfTypes tc_args_to_keep)
                                  (FV.mkFVs deriv_tvs)
              -- See Note [Unification of two kind variables in deriving]
              unmapped_tkvs   = filter (\v -> v `notElemTCvSubst` kind_subst
                                      && not (v `elemVarSet` ki_subst_range))
                                       all_tkvs
              (subst, _)      = mapAccumL substTyVarBndr
                                          kind_subst unmapped_tkvs
              final_tc_args   = substTys subst tc_args_to_keep
              final_cls_tys   = substTys subst cls_tys
              tkvs            = tyCoVarsOfTypesWellScoped $
                                final_cls_tys ++ final_tc_args

        ; traceTc "Deriving strategy (deriving clause)" $
            vcat [ppr deriv_strat, ppr deriv_pred]

        ; traceTc "derivTyData1" (vcat [ pprTvBndrs tvs, ppr tc, ppr tc_args, ppr deriv_pred
                                       , pprTvBndrs (tyCoVarsOfTypesList tc_args)
                                       , ppr n_args_to_keep, ppr n_args_to_drop
                                       , ppr inst_ty_kind, ppr cls_arg_kind, ppr mb_match
                                       , ppr final_tc_args, ppr final_cls_tys ])

        ; traceTc "derivTyData2" (vcat [ ppr tkvs ])

        ; checkTc (allDistinctTyVars (mkVarSet tkvs) args_to_drop)     -- (a, b, c)
                  (derivingEtaErr cls final_cls_tys (mkTyConApp tc final_tc_args))
                -- Check that
                --  (a) The args to drop are all type variables; eg reject:
                --              data instance T a Int = .... deriving( Monad )
                --  (b) The args to drop are all *distinct* type variables; eg reject:
                --              class C (a :: * -> * -> *) where ...
                --              data instance T a a = ... deriving( C )
                --  (c) The type class args, or remaining tycon args,
                --      do not mention any of the dropped type variables
                --              newtype T a s = ... deriving( ST s )
                --              newtype instance K a a = ... deriving( Monad )
                --
                -- It is vital that the implementation of allDistinctTyVars
                -- expand any type synonyms.
                -- See Note [Eta-reducing type synonyms]

        ; spec <- mkEqnHelp Nothing tkvs
                            cls final_cls_tys tc final_tc_args
                            Nothing deriv_strat
        ; traceTc "derivTyData" (ppr spec)
        ; return [spec] } }


{-
Note [Unify kinds in deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #8534)
    data T a b = MkT a deriving( Functor )
    -- where Functor :: (*->*) -> Constraint

So T :: forall k. * -> k -> *.   We want to get
    instance Functor (T * (a:*)) where ...
Notice the '*' argument to T.

Moreover, as well as instantiating T's kind arguments, we may need to instantiate
C's kind args.  Consider (Trac #8865):
  newtype T a b = MkT (Either a b) deriving( Category )
where
  Category :: forall k. (k -> k -> *) -> Constraint
We need to generate the instance
  instance Category * (Either a) where ...
Notice the '*' argument to Category.

So we need to
 * drop arguments from (T a b) to match the number of
   arrows in the (last argument of the) class;
 * and then *unify* kind of the remaining type against the
   expected kind, to figure out how to instantiate C's and T's
   kind arguments.

In the two examples,
 * we unify   kind-of( T k (a:k) ) ~ kind-of( Functor )
         i.e.      (k -> *) ~ (* -> *)   to find k:=*.
         yielding  k:=*

 * we unify   kind-of( Either ) ~ kind-of( Category )
         i.e.      (* -> * -> *)  ~ (k -> k -> k)
         yielding  k:=*

Now we get a kind substitution.  We then need to:

  1. Remove the substituted-out kind variables from the quantified kind vars

  2. Apply the substitution to the kinds of quantified *type* vars
     (and extend the substitution to reflect this change)

  3. Apply that extended substitution to the non-dropped args (types and
     kinds) of the type and class

Forgetting step (2) caused Trac #8893:
  data V a = V [a] deriving Functor
  data P (x::k->*) (a:k) = P (x a) deriving Functor
  data C (x::k->*) (a:k) = C (V (P x a)) deriving Functor

When deriving Functor for P, we unify k to *, but we then want
an instance   $df :: forall (x:*->*). Functor x => Functor (P * (x:*->*))
and similarly for C.  Notice the modified kind of x, both at binding
and occurrence sites.

This can lead to some surprising results when *visible* kind binder is
unified (in contrast to the above examples, in which only non-visible kind
binders were considered). Consider this example from Trac #11732:

    data T k (a :: k) = MkT deriving Functor

Since unification yields k:=*, this results in a generated instance of:

    instance Functor (T *) where ...

which looks odd at first glance, since one might expect the instance head
to be of the form Functor (T k). Indeed, one could envision an alternative
generated instance of:

    instance (k ~ *) => Functor (T k) where

But this does not typecheck as the result of a -XTypeInType design decision:
kind equalities are not allowed to be bound in types, only terms. But in
essence, the two instance declarations are entirely equivalent, since even
though (T k) matches any kind k, the only possibly value for k is *, since
anything else is ill-typed. As a result, we can just as comfortably use (T *).

Another way of thinking about is: deriving clauses often infer constraints.
For example:

    data S a = S a deriving Eq

infers an (Eq a) constraint in the derived instance. By analogy, when we
are deriving Functor, we might infer an equality constraint (e.g., k ~ *).
The only distinction is that GHC instantiates equality constraints directly
during the deriving process.

Another quirk of this design choice manifests when typeclasses have visible
kind parameters. Consider this code (also from Trac #11732):

    class Cat k (cat :: k -> k -> *) where
      catId   :: cat a a
      catComp :: cat b c -> cat a b -> cat a c

    instance Cat * (->) where
      catId   = id
      catComp = (.)

    newtype Fun a b = Fun (a -> b) deriving (Cat k)

Even though we requested an derived instance of the form (Cat k Fun), the
kind unification will actually generate (Cat * Fun) (i.e., the same thing as if
the user wrote deriving (Cat *)).

Note [Unification of two kind variables in deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As a special case of the Note above, it is possible to derive an instance of
a poly-kinded typeclass for a poly-kinded datatype. For example:

    class Category (cat :: k -> k -> *) where
    newtype T (c :: k -> k -> *) a b = MkT (c a b) deriving Category

This case is suprisingly tricky. To see why, let's write out what instance GHC
will attempt to derive (using -fprint-explicit-kinds syntax):

    instance Category k1 (T k2 c) where ...

GHC will attempt to unify k1 and k2, which produces a substitution (kind_subst)
that looks like [k2 :-> k1]. Importantly, we need to apply this substitution to
the type variable binder for c, since its kind is (k2 -> k2 -> *).

We used to accomplish this by doing the following:

    unmapped_tkvs = filter (`notElemTCvSubst` kind_subst) all_tkvs
    (subst, _)    = mapAccumL substTyVarBndr kind_subst unmapped_tkvs

Where all_tkvs contains all kind variables in the class and instance types (in
this case, all_tkvs = [k1,k2]). But since kind_subst only has one mapping,
this results in unmapped_tkvs being [k1], and as a consequence, k1 gets mapped
to another kind variable in subst! That is, subst = [k2 :-> k1, k1 :-> k_new].
This is bad, because applying that substitution yields the following instance:

   instance Category k_new (T k1 c) where ...

In other words, keeping k1 in unmapped_tvks taints the substitution, resulting
in an ill-kinded instance (this caused Trac #11837).

To prevent this, we need to filter out any variable from all_tkvs which either

1. Appears in the domain of kind_subst. notElemTCvSubst checks this.
2. Appears in the range of kind_subst. To do this, we compute the free
   variable set of the range of kind_subst with getTCvSubstRangeFVs, and check
   if a kind variable appears in that set.

Note [Eta-reducing type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One can instantiate a type in a data family instance with a type synonym that
mentions other type variables:

  type Const a b = a
  data family Fam (f :: * -> *) (a :: *)
  newtype instance Fam f (Const a f) = Fam (f a) deriving Functor

With -XTypeInType, it is also possible to define kind synonyms, and they can
mention other types in a datatype declaration. For example,

  type Const a b = a
  newtype T f (a :: Const * f) = T (f a) deriving Functor

When deriving, we need to perform eta-reduction analysis to ensure that none of
the eta-reduced type variables are mentioned elsewhere in the declaration. But
we need to be careful, because if we don't expand through the Const type
synonym, we will mistakenly believe that f is an eta-reduced type variable and
fail to derive Functor, even though the code above is correct (see Trac #11416,
where this was first noticed). For this reason, we expand the type synonyms in
the eta-reduced types before doing any analysis.
-}

mkEqnHelp :: Maybe OverlapMode
          -> [TyVar]
          -> Class -> [Type]
          -> TyCon -> [Type]
          -> DerivContext       -- Just    => context supplied (standalone deriving)
                                -- Nothing => context inferred (deriving on data decl)
          -> Maybe DerivStrategy
          -> TcRn EarlyDerivSpec
-- Make the EarlyDerivSpec for an instance
--      forall tvs. theta => cls (tys ++ [ty])
-- where the 'theta' is optional (that's the Maybe part)
-- Assumes that this declaration is well-kinded

mkEqnHelp overlap_mode tvs cls cls_tys tycon tc_args mtheta deriv_strat
  = do {      -- Find the instance of a data family
              -- Note [Looking up family instances for deriving]
         fam_envs <- tcGetFamInstEnvs
       ; let (rep_tc, rep_tc_args, _co) = tcLookupDataFamInst fam_envs tycon tc_args
              -- If it's still a data family, the lookup failed; i.e no instance exists
       ; when (isDataFamilyTyCon rep_tc)
              (bale_out (text "No family instance for" <+> quotes (pprTypeApp tycon tc_args)))

       -- For standalone deriving (mtheta /= Nothing),
       -- check that all the data constructors are in scope.
       ; rdr_env <- getGlobalRdrEnv
       ; let data_con_names = map dataConName (tyConDataCons rep_tc)
             hidden_data_cons = not (isWiredInName (tyConName rep_tc)) &&
                                (isAbstractTyCon rep_tc ||
                                 any not_in_scope data_con_names)
             not_in_scope dc  = isNothing (lookupGRE_Name rdr_env dc)

       ; addUsedDataCons rdr_env rep_tc
       ; unless (isNothing mtheta || not hidden_data_cons)
                (bale_out (derivingHiddenErr tycon))

       ; dflags <- getDynFlags
       ; if isDataTyCon rep_tc then
            mkDataTypeEqn dflags overlap_mode tvs cls cls_tys
                          tycon tc_args rep_tc rep_tc_args mtheta deriv_strat
         else
            mkNewTypeEqn dflags overlap_mode tvs cls cls_tys
                         tycon tc_args rep_tc rep_tc_args mtheta deriv_strat }
  where
     bale_out msg = failWithTc (derivingThingErr False cls cls_tys
                      (mkTyConApp tycon tc_args) deriv_strat msg)

{-
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
with standalone deriving declarations.

Note [Deriving, type families, and partial applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When there are no type families, it's quite easy:

    newtype S a = MkS [a]
    -- :CoS :: S  ~ []  -- Eta-reduced

    instance Eq [a] => Eq (S a)         -- by coercion sym (Eq (:CoS a)) : Eq [a] ~ Eq (S a)
    instance Monad [] => Monad S        -- by coercion sym (Monad :CoS)  : Monad [] ~ Monad S

When type familes are involved it's trickier:

    data family T a b
    newtype instance T Int a = MkT [a] deriving( Eq, Monad )
    -- :RT is the representation type for (T Int a)
    --  :Co:RT    :: :RT ~ []          -- Eta-reduced!
    --  :CoF:RT a :: T Int a ~ :RT a   -- Also eta-reduced!

    instance Eq [a] => Eq (T Int a)     -- easy by coercion
       -- d1 :: Eq [a]
       -- d2 :: Eq (T Int a) = d1 |> Eq (sym (:Co:RT a ; :coF:RT a))

    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???
       -- d1 :: Monad []
       -- d2 :: Monad (T Int) = d1 |> Monad (sym (:Co:RT ; :coF:RT))

Note the need for the eta-reduced rule axioms.  After all, we can
write it out
    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???
      return x = MkT [x]
      ... etc ...

See Note [Eta reduction for data families] in FamInstEnv

%************************************************************************
%*                                                                      *
                Deriving data types
*                                                                      *
************************************************************************
-}

mkDataTypeEqn :: DynFlags
              -> Maybe OverlapMode
              -> [TyVar]                -- Universally quantified type variables in the instance
              -> Class                  -- Class for which we need to derive an instance
              -> [Type]                 -- Other parameters to the class except the last
              -> TyCon                  -- Type constructor for which the instance is requested
                                        --    (last parameter to the type class)
              -> [Type]                 -- Parameters to the type constructor
              -> TyCon                  -- rep of the above (for type families)
              -> [Type]                 -- rep of the above
              -> DerivContext        -- Context of the instance, for standalone deriving
              -> Maybe DerivStrategy    -- 'Just' if user requests a particular
                                        -- deriving strategy.
                                        -- Otherwise, 'Nothing'.
              -> TcRn EarlyDerivSpec    -- Return 'Nothing' if error

mkDataTypeEqn dflags overlap_mode tvs cls cls_tys
              tycon tc_args rep_tc rep_tc_args mtheta deriv_strat
  = case deriv_strat of
      Just DerivStock -> mk_eqn_stock dflags mtheta cls cls_tys rep_tc
                           go_for_it bale_out
      Just DerivAnyclass -> mk_eqn_anyclass dflags rep_tc cls
                              go_for_it bale_out
      -- GeneralizedNewtypeDeriving makes no sense for non-newtypes
      Just DerivNewtype -> bale_out gndNonNewtypeErr
      -- Lacking a user-requested deriving strategy, we will try to pick
      -- between the stock or anyclass strategies
      Nothing -> mk_eqn_no_mechanism dflags mtheta cls cls_tys rep_tc
                   go_for_it bale_out
  where
    go_for_it    = mk_data_eqn overlap_mode tvs cls cls_tys tycon tc_args rep_tc rep_tc_args mtheta
    bale_out msg = failWithTc (derivingThingErr False cls cls_tys
                     (mkTyConApp tycon tc_args) deriv_strat msg)

mk_data_eqn :: Maybe OverlapMode -> [TyVar] -> Class -> [Type]
            -> TyCon -> [TcType] -> TyCon -> [TcType] -> DerivContext
            -> DerivSpecMechanism -- How GHC should proceed attempting to
                                  -- derive this instance, determined in
                                  -- mkDataTypeEqn/mkNewTypeEqn
            -> TcM EarlyDerivSpec
mk_data_eqn overlap_mode tvs cls cls_tys tycon tc_args rep_tc rep_tc_args
            mtheta mechanism
  = do loc                  <- getSrcSpanM
       dfun_name            <- newDFunName' cls tycon
       case mtheta of
        Nothing -> -- Infer context
          inferConstraints tvs cls cls_tys
                           inst_ty rep_tc rep_tc_args
            $ \inferred_constraints tvs' inst_tys' ->
            return $ InferTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs'
                   , ds_cls = cls, ds_tys = inst_tys'
                   , ds_tc = rep_tc
                   , ds_theta = inferred_constraints
                   , ds_overlap = overlap_mode
                   , ds_mechanism = mechanism }
        Just theta -> do -- Specified context
            return $ GivenTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs
                   , ds_cls = cls, ds_tys = inst_tys
                   , ds_tc = rep_tc
                   , ds_theta = theta
                   , ds_overlap = overlap_mode
                   , ds_mechanism = mechanism }
  where
    inst_ty  = mkTyConApp tycon tc_args
    inst_tys = cls_tys ++ [inst_ty]

mk_eqn_stock :: DynFlags -> DerivContext -> Class -> [Type] -> TyCon
             -> (DerivSpecMechanism -> TcRn EarlyDerivSpec)
             -> (SDoc -> TcRn EarlyDerivSpec)
             -> TcRn EarlyDerivSpec
mk_eqn_stock dflags mtheta cls cls_tys rep_tc go_for_it bale_out
  = case checkSideConditions dflags mtheta cls cls_tys rep_tc of
        CanDerive               -> mk_eqn_stock' cls go_for_it
        DerivableClassError msg -> bale_out msg
        _                       -> bale_out (nonStdErr cls)

mk_eqn_stock' :: Class -> (DerivSpecMechanism -> TcRn EarlyDerivSpec)
                -> TcRn EarlyDerivSpec
mk_eqn_stock' cls go_for_it
  = go_for_it $ case hasStockDeriving cls of
        Just gen_fn -> DerivSpecStock gen_fn
        Nothing ->
          pprPanic "mk_eqn_stock': Not a stock class!" (ppr cls)

mk_eqn_anyclass :: DynFlags -> TyCon -> Class
                -> (DerivSpecMechanism -> TcRn EarlyDerivSpec)
                -> (SDoc -> TcRn EarlyDerivSpec)
                -> TcRn EarlyDerivSpec
mk_eqn_anyclass dflags rep_tc cls go_for_it bale_out
  = case canDeriveAnyClass dflags rep_tc cls of
        Nothing  -> go_for_it DerivSpecAnyClass
        Just msg -> bale_out msg

mk_eqn_no_mechanism :: DynFlags -> DerivContext -> Class -> [Type] -> TyCon
                    -> (DerivSpecMechanism -> TcRn EarlyDerivSpec)
                    -> (SDoc -> TcRn EarlyDerivSpec)
                    -> TcRn EarlyDerivSpec
mk_eqn_no_mechanism dflags mtheta cls cls_tys rep_tc go_for_it bale_out
  = case checkSideConditions dflags mtheta cls cls_tys rep_tc of
        -- NB: pass the *representation* tycon to checkSideConditions
        NonDerivableClass   msg -> bale_out (nonStdErr cls $$ msg)
        DerivableClassError msg -> bale_out msg
        CanDerive               -> mk_eqn_stock' cls go_for_it
        DerivableViaInstance    -> go_for_it DerivSpecAnyClass

{-
************************************************************************
*                                                                      *
                Deriving newtypes
*                                                                      *
************************************************************************
-}

mkNewTypeEqn :: DynFlags -> Maybe OverlapMode -> [TyVar] -> Class
             -> [Type] -> TyCon -> [Type] -> TyCon -> [Type]
             -> DerivContext -> Maybe DerivStrategy
             -> TcRn EarlyDerivSpec
mkNewTypeEqn dflags overlap_mode tvs
             cls cls_tys tycon tc_args rep_tycon rep_tc_args
             mtheta deriv_strat
-- Want: instance (...) => cls (cls_tys ++ [tycon tc_args]) where ...
  = ASSERT( length cls_tys + 1 == classArity cls )
    case deriv_strat of
      Just DerivStock -> mk_eqn_stock dflags mtheta cls cls_tys rep_tycon
                           go_for_it_other bale_out
      Just DerivAnyclass -> mk_eqn_anyclass dflags rep_tycon cls
                              go_for_it_other bale_out
      Just DerivNewtype ->
        -- Since the user explicitly asked for GeneralizedNewtypeDeriving, we
        -- don't need to perform all of the checks we normally would, such as
        -- if the class being derived is known to produce ill-roled coercions
        -- (e.g., Traversable), since we can just derive the instance and let
        -- it error if need be.
        -- See Note [Determining whether newtype-deriving is appropriate]
        if coercion_looks_sensible && newtype_deriving
          then go_for_it_gnd
          else bale_out (cant_derive_err $$
                         if newtype_deriving then empty else suggest_gnd)
      Nothing
        | might_derive_via_coercible
          && ((newtype_deriving && not deriveAnyClass)
               || std_class_via_coercible cls)
       -> go_for_it_gnd
        | otherwise
       -> case checkSideConditions dflags mtheta cls cls_tys rep_tycon of
            DerivableClassError msg
              -- There's a particular corner case where
              --
              -- 1. -XGeneralizedNewtypeDeriving and -XDeriveAnyClass are both
              --    enabled at the same time
              -- 2. We're deriving a particular stock derivable class
              --    (such as Functor)
              --
              -- and the previous cases won't catch it. This fixes the bug
              -- reported in Trac #10598.
              | might_derive_via_coercible && newtype_deriving
             -> go_for_it_gnd
              -- Otherwise, throw an error for a stock class
              | might_derive_via_coercible && not newtype_deriving
             -> bale_out (msg $$ suggest_gnd)
              | otherwise
             -> bale_out msg

            -- Must use newtype deriving or DeriveAnyClass
            NonDerivableClass _msg
              -- Too hard, even with newtype deriving
              | newtype_deriving           -> bale_out cant_derive_err
              -- Try newtype deriving!
              -- Here we suggest GeneralizedNewtypeDeriving even in cases where
              -- it may not be applicable. See Trac #9600.
              | otherwise                  -> bale_out (non_std $$ suggest_gnd)

            -- DerivableViaInstance
            DerivableViaInstance -> do
              -- If both DeriveAnyClass and GeneralizedNewtypeDeriving are
              -- enabled, we take the diplomatic approach of defaulting to
              -- DeriveAnyClass, but emitting a warning about the choice.
              -- See Note [Deriving strategies]
              when (newtype_deriving && deriveAnyClass) $
                addWarnTc NoReason $ sep
                  [ text "Both DeriveAnyClass and"
                    <+> text "GeneralizedNewtypeDeriving are enabled"
                  , text "Defaulting to the DeriveAnyClass strategy"
                    <+> text "for instantiating" <+> ppr cls ]
              go_for_it_other DerivSpecAnyClass
            -- CanDerive
            CanDerive -> mk_eqn_stock' cls go_for_it_other
  where
        newtype_deriving  = xopt LangExt.GeneralizedNewtypeDeriving dflags
        deriveAnyClass    = xopt LangExt.DeriveAnyClass             dflags
        go_for_it_gnd     = do
          traceTc "newtype deriving:" $
            ppr tycon <+> ppr rep_tys <+> ppr all_preds
          dfun_name <- newDFunName' cls tycon
          loc <- getSrcSpanM
          case mtheta of
           Just theta -> return $ GivenTheta $ DS
               { ds_loc = loc
               , ds_name = dfun_name, ds_tvs = dfun_tvs
               , ds_cls = cls, ds_tys = inst_tys
               , ds_tc = rep_tycon
               , ds_theta = theta
               , ds_overlap = overlap_mode
               , ds_mechanism = DerivSpecNewtype rep_inst_ty }
           Nothing -> return $ InferTheta $ DS
               { ds_loc = loc
               , ds_name = dfun_name, ds_tvs = dfun_tvs
               , ds_cls = cls, ds_tys = inst_tys
               , ds_tc = rep_tycon
               , ds_theta = all_preds
               , ds_overlap = overlap_mode
               , ds_mechanism = DerivSpecNewtype rep_inst_ty }
        go_for_it_other = mk_data_eqn overlap_mode tvs cls cls_tys tycon
                                      tc_args rep_tycon rep_tc_args mtheta
        bale_out    = bale_out' newtype_deriving
        bale_out' b = failWithTc . derivingThingErr b cls cls_tys inst_ty
                                                    deriv_strat

        non_std     = nonStdErr cls
        suggest_gnd = text "Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension"

        -- Here is the plan for newtype derivings.  We see
        --        newtype T a1...an = MkT (t ak+1...an) deriving (.., C s1 .. sm, ...)
        -- where t is a type,
        --       ak+1...an is a suffix of a1..an, and are all tyvars
        --       ak+1...an do not occur free in t, nor in the s1..sm
        --       (C s1 ... sm) is a  *partial applications* of class C
        --                      with the last parameter missing
        --       (T a1 .. ak) matches the kind of C's last argument
        --              (and hence so does t)
        -- The latter kind-check has been done by deriveTyData already,
        -- and tc_args are already trimmed
        --
        -- We generate the instance
        --       instance forall ({a1..ak} u fvs(s1..sm)).
        --                C s1 .. sm t => C s1 .. sm (T a1...ak)
        -- where T a1...ap is the partial application of
        --       the LHS of the correct kind and p >= k
        --
        --      NB: the variables below are:
        --              tc_tvs = [a1, ..., an]
        --              tyvars_to_keep = [a1, ..., ak]
        --              rep_ty = t ak .. an
        --              deriv_tvs = fvs(s1..sm) \ tc_tvs
        --              tys = [s1, ..., sm]
        --              rep_fn' = t
        --
        -- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
        -- We generate the instance
        --      instance Monad (ST s) => Monad (T s) where

        nt_eta_arity = newTyConEtadArity rep_tycon
                -- For newtype T a b = MkT (S a a b), the TyCon machinery already
                -- eta-reduces the representation type, so we know that
                --      T a ~ S a a
                -- That's convenient here, because we may have to apply
                -- it to fewer than its original complement of arguments

        -- Note [Newtype representation]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Need newTyConRhs (*not* a recursive representation finder)
        -- to get the representation type. For example
        --      newtype B = MkB Int
        --      newtype A = MkA B deriving( Num )
        -- We want the Num instance of B, *not* the Num instance of Int,
        -- when making the Num instance of A!
        rep_inst_ty = newTyConInstRhs rep_tycon rep_tc_args
        rep_tys     = cls_tys ++ [rep_inst_ty]
        rep_pred    = mkClassPred cls rep_tys
        rep_pred_o  = mkPredOrigin DerivOrigin TypeLevel rep_pred
                -- rep_pred is the representation dictionary, from where
                -- we are gong to get all the methods for the newtype
                -- dictionary

        -- Next we figure out what superclass dictionaries to use
        -- See Note [Newtype deriving superclasses] above
        cls_tyvars = classTyVars cls
        dfun_tvs   = tyCoVarsOfTypesWellScoped inst_tys
        inst_ty    = mkTyConApp tycon tc_args
        inst_tys   = cls_tys ++ [inst_ty]
        sc_theta   = mkThetaOrigin DerivOrigin TypeLevel $
                     substTheta (zipTvSubst cls_tyvars inst_tys) $
                     classSCTheta cls

        -- Next we collect Coercible constraints between
        -- the Class method types, instantiated with the representation and the
        -- newtype type; precisely the constraints required for the
        -- calls to coercible that we are going to generate.
        coercible_constraints =
            [ let (Pair t1 t2) = mkCoerceClassMethEqn cls dfun_tvs inst_tys rep_inst_ty meth
              in mkPredOrigin (DerivOriginCoerce meth t1 t2) TypeLevel
                              (mkReprPrimEqPred t1 t2)
            | meth <- classMethods cls ]

                -- If there are no tyvars, there's no need
                -- to abstract over the dictionaries we need
                -- Example:     newtype T = MkT Int deriving( C )
                -- We get the derived instance
                --              instance C T
                -- rather than
                --              instance C Int => C T
        all_preds = rep_pred_o : coercible_constraints ++ sc_theta -- NB: rep_pred comes first

        -------------------------------------------------------------------
        --  Figuring out whether we can only do this newtype-deriving thing

        -- See Note [Determining whether newtype-deriving is appropriate]
        might_derive_via_coercible
           =  not (non_coercible_class cls)
           && coercion_looks_sensible
--         && not (isRecursiveTyCon tycon)      -- Note [Recursive newtypes]
        coercion_looks_sensible = eta_ok && ats_ok

        -- Check that eta reduction is OK
        eta_ok = nt_eta_arity <= length rep_tc_args
                -- The newtype can be eta-reduced to match the number
                --     of type argument actually supplied
                --        newtype T a b = MkT (S [a] b) deriving( Monad )
                --     Here the 'b' must be the same in the rep type (S [a] b)
                --     And the [a] must not mention 'b'.  That's all handled
                --     by nt_eta_rity.

        ats_ok = null (classATs cls)
               -- No associated types for the class, because we don't
               -- currently generate type 'instance' decls; and cannot do
               -- so for 'data' instance decls

        cant_derive_err
           = vcat [ ppUnless eta_ok eta_msg
                  , ppUnless ats_ok ats_msg ]
        eta_msg   = text "cannot eta-reduce the representation type enough"
        ats_msg   = text "the class has associated types"

{-
Note [Recursive newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Newtype deriving works fine, even if the newtype is recursive.
e.g.    newtype S1 = S1 [T1 ()]
        newtype T1 a = T1 (StateT S1 IO a ) deriving( Monad )
Remember, too, that type families are currently (conservatively) given
a recursive flag, so this also allows newtype deriving to work
for type famillies.

We used to exclude recursive types, because we had a rather simple
minded way of generating the instance decl:
   newtype A = MkA [A]
   instance Eq [A] => Eq A      -- Makes typechecker loop!
But now we require a simple context, so it's ok.

Note [Determining whether newtype-deriving is appropriate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
  newtype NT = MkNT Foo
    deriving C
we have to decide how to perform the deriving. Do we do newtype deriving,
or do we do normal deriving? In general, we prefer to do newtype deriving
wherever possible. So, we try newtype deriving unless there's a glaring
reason not to.

"Glaring reasons not to" include trying to derive a class for which a
coercion-based instance doesn't make sense. These classes are listed in
the definition of non_coercible_class. They include Show (since it must
show the name of the datatype) and Traversable (since a coercion-based
Traversable instance is ill-roled).

However, non_coercible_class is ignored if the user explicitly requests
to derive an instance with GeneralizedNewtypeDeriving using the newtype
deriving strategy. In such a scenario, GHC will unquestioningly try to
derive the instance via coercions (even if the final generated code is
ill-roled!). See Note [Deriving strategies].

Note that newtype deriving might fail, even after we commit to it. This
is because the derived instance uses `coerce`, which must satisfy its
`Coercible` constraint. This is different than other deriving scenarios,
where we're sure that the resulting instance will type-check.

************************************************************************
*                                                                      *
\subsection[TcDeriv-normal-binds]{Bindings for the various classes}
*                                                                      *
************************************************************************

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
-}

-- Generate the InstInfo for the required instance paired with the
--   *representation* tycon for that instance,
-- plus any auxiliary bindings required
--
-- Representation tycons differ from the tycon in the instance signature in
-- case of instances for indexed families.
--
genInst :: DerivSpec ThetaType
        -> TcM (InstInfo RdrName, BagDerivStuff, Maybe Name)
genInst spec@(DS { ds_tvs = tvs, ds_tc = rep_tycon
                 , ds_theta = theta, ds_mechanism = mechanism, ds_tys = tys
                 , ds_cls = clas, ds_loc = loc })
  -- See Note [Bindings for Generalised Newtype Deriving]
  | DerivSpecNewtype rhs_ty <- mechanism
  = do { inst_spec <- newDerivClsInst theta spec
       ; doDerivInstErrorChecks clas inst_spec mechanism
       ; return ( InstInfo
                    { iSpec   = inst_spec
                    , iBinds  = InstBindings
                        { ib_binds      = gen_Newtype_binds loc clas
                                            tvs tys rhs_ty
                          -- Scope over bindings
                        , ib_tyvars     = map Var.varName tvs
                        , ib_pragmas    = []
                        , ib_extensions = [ LangExt.ImpredicativeTypes
                                          , LangExt.RankNTypes ]
                          -- Both these flags are needed for higher-rank uses of coerce
                          -- See Note [Newtype-deriving instances] in TcGenDeriv
                        , ib_derived    = True } }
                , emptyBag
                , Just $ getName $ head $ tyConDataCons rep_tycon ) }
              -- See Note [Newtype deriving and unused constructors]
  | otherwise
  = do { (meth_binds, deriv_stuff) <- genDerivStuff mechanism loc clas
                                        rep_tycon tys tvs
       ; inst_spec <- newDerivClsInst theta spec
       ; doDerivInstErrorChecks clas inst_spec mechanism
       ; traceTc "newder" (ppr inst_spec)
       ; let inst_info
               = InstInfo { iSpec   = inst_spec
                          , iBinds  = InstBindings
                                        { ib_binds = meth_binds
                                        , ib_tyvars = map Var.varName tvs
                                        , ib_pragmas = []
                                        , ib_extensions = []
                                        , ib_derived = True } }
       ; return ( inst_info, deriv_stuff, Nothing ) }

doDerivInstErrorChecks :: Class -> ClsInst -> DerivSpecMechanism -> TcM ()
doDerivInstErrorChecks clas clas_inst mechanism
  = do { traceTc "doDerivInstErrorChecks" (ppr clas_inst)
       ; dflags <- getDynFlags
         -- Check for Generic instances that are derived with an exotic
         -- deriving strategy like DAC
         -- See Note [Deriving strategies]
       ; when (exotic_mechanism && className clas `elem` genericClassNames) $
         do { failIfTc (safeLanguageOn dflags) gen_inst_err
            ; when (safeInferOn dflags) (recordUnsafeInfer emptyBag) } }
  where
    exotic_mechanism = case mechanism of
      DerivSpecStock{} -> False
      _                -> True

    gen_inst_err = hang (text ("Generic instances can only be derived in "
                            ++ "Safe Haskell using the stock strategy.") $+$
                         text "In the following instance:")
                      2 (pprInstanceHdr clas_inst)

-- Generate the bindings needed for a derived class that isn't handled by
-- -XGeneralizedNewtypeDeriving.
genDerivStuff :: DerivSpecMechanism -> SrcSpan -> Class
              -> TyCon -> [Type] -> [TyVar]
              -> TcM (LHsBinds RdrName, BagDerivStuff)
genDerivStuff mechanism loc clas tycon inst_tys tyvars
  = case mechanism of
      -- Try a stock deriver
      DerivSpecStock gen_fn -> gen_fn loc tycon inst_tys

      -- If there isn't a stock deriver, our last resort is -XDeriveAnyClass
      -- (since -XGeneralizedNewtypeDeriving fell through).
      DerivSpecAnyClass -> do
        let mini_env   = mkVarEnv (classTyVars clas `zip` inst_tys)
            mini_subst = mkTvSubst (mkInScopeSet (mkVarSet tyvars)) mini_env
        dflags <- getDynFlags
        tyfam_insts <-
          ASSERT2( isNothing (canDeriveAnyClass dflags tycon clas)
                 , ppr "genDerivStuff: bad derived class" <+> ppr clas )
          mapM (tcATDefault False loc mini_subst emptyNameSet)
               (classATItems clas)
        return ( emptyBag -- No method bindings are needed...
               , listToBag (map DerivFamInst (concat tyfam_insts))
               -- ...but we may need to generate binding for associated type
               -- family default instances.
               -- See Note [DeriveAnyClass and default family instances]
               )

      _ -> panic "genDerivStuff"

{-
Note [Bindings for Generalised Newtype Deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  class Eq a => C a where
     f :: a -> a
  newtype N a = MkN [a] deriving( C )
  instance Eq (N a) where ...

The 'deriving C' clause generates, in effect
  instance (C [a], Eq a) => C (N a) where
     f = coerce (f :: [a] -> [a])

This generates a cast for each method, but allows the superclasse to
be worked out in the usual way.  In this case the superclass (Eq (N
a)) will be solved by the explicit Eq (N a) instance.  We do *not*
create the superclasses by casting the superclass dictionaries for the
representation type.

See the paper "Safe zero-cost coercions for Haskell".

Note [DeriveAnyClass and default family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a class has a associated type family with a default instance, e.g.:

  class C a where
    type T a
    type T a = Char

then there are a couple of scenarios in which a user would expect T a to
default to Char. One is when an instance declaration for C is given without
an implementation for T:

  instance C Int

Another scenario in which this can occur is when the -XDeriveAnyClass extension
is used:

  data Example = Example deriving (C, Generic)

In the latter case, we must take care to check if C has any associated type
families with default instances, because -XDeriveAnyClass will never provide
an implementation for them. We "fill in" the default instances using the
tcATDefault function from TcClsDcl (which is also used in TcInstDcls to handle
the empty instance declaration case).

Note [Deriving strategies]
~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has a notion of deriving strategies, which allow the user to explicitly
request which approach to use when deriving an instance (enabled with the
-XDerivingStrategies language extension). For more information, refer to the
original Trac ticket (#10598) or the associated wiki page:
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies

A deriving strategy can be specified in a deriving clause:

    newtype Foo = MkFoo Bar
      deriving newtype C

Or in a standalone deriving declaration:

    deriving anyclass instance C Foo

-XDerivingStrategies also allows the use of multiple deriving clauses per data
declaration so that a user can derive some instance with one deriving strategy
and other instances with another deriving strategy. For example:

    newtype Baz = Baz Quux
      deriving          (Eq, Ord)
      deriving stock    (Read, Show)
      deriving newtype  (Num, Floating)
      deriving anyclass C

Currently, the deriving strategies are:

* stock: Have GHC implement a "standard" instance for a data type, if possible
  (e.g., Eq, Ord, Generic, Data, Functor, etc.)

* anyclass: Use -XDeriveAnyClass

* newtype: Use -XGeneralizedNewtypeDeriving

If an explicit deriving strategy is not given, GHC has an algorithm it uses to
determine which strategy it will actually use. The algorithm is quite long,
so it lives in the Haskell wiki at
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies
("The deriving strategy resolution algorithm" section).

Internally, GHC uses the DerivStrategy datatype to denote a user-requested
deriving strategy, and it uses the DerivSpecMechanism datatype to denote what
GHC will use to derive the instance after taking the above steps. In other
words, GHC will always settle on a DerivSpecMechnism, even if the user did not
ask for a particular DerivStrategy (using the algorithm linked to above).

************************************************************************
*                                                                      *
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
*                                                                      *
************************************************************************
-}

nonUnaryErr :: LHsSigType Name -> SDoc
nonUnaryErr ct = quotes (ppr ct)
  <+> text "is not a unary constraint, as expected by a deriving clause"

nonStdErr :: Class -> SDoc
nonStdErr cls =
      quotes (ppr cls)
  <+> text "is not a stock derivable class (Eq, Show, etc.)"

gndNonNewtypeErr :: SDoc
gndNonNewtypeErr =
  text "GeneralizedNewtypeDeriving cannot be used on non-newtypes"

derivingNullaryErr :: MsgDoc
derivingNullaryErr = text "Cannot derive instances for nullary classes"

derivingKindErr :: TyCon -> Class -> [Type] -> Kind -> Bool -> MsgDoc
derivingKindErr tc cls cls_tys cls_kind enough_args
  = sep [ hang (text "Cannot derive well-kinded instance of form"
                      <+> quotes (pprClassPred cls cls_tys
                                    <+> parens (ppr tc <+> text "...")))
               2 gen1_suggestion
        , nest 2 (text "Class" <+> quotes (ppr cls)
                      <+> text "expects an argument of kind"
                      <+> quotes (pprKind cls_kind))
        ]
  where
    gen1_suggestion | cls `hasKey` gen1ClassKey && enough_args
                    = text "(Perhaps you intended to use PolyKinds)"
                    | otherwise = Outputable.empty

derivingEtaErr :: Class -> [Type] -> Type -> MsgDoc
derivingEtaErr cls cls_tys inst_ty
  = sep [text "Cannot eta-reduce to an instance of form",
         nest 2 (text "instance (...) =>"
                <+> pprClassPred cls (cls_tys ++ [inst_ty]))]

derivingThingErr :: Bool -> Class -> [Type] -> Type -> Maybe DerivStrategy
                 -> MsgDoc -> MsgDoc
derivingThingErr newtype_deriving clas tys ty deriv_strat why
  = sep [(hang (text "Can't make a derived instance of")
             2 (quotes (ppr pred) <+> via_mechanism)
          $$ nest 2 extra) <> colon,
         nest 2 why]
  where
    extra | Nothing <- deriv_strat, newtype_deriving
          = text "(even with cunning GeneralizedNewtypeDeriving)"
          | otherwise = Outputable.empty
    pred = mkClassPred clas (tys ++ [ty])
    via_mechanism = case deriv_strat of
                      Just strat -> text "with the" <+> ppr strat
                                        <+> text "strategy"
                      Nothing    -> empty

derivingHiddenErr :: TyCon -> SDoc
derivingHiddenErr tc
  = hang (text "The data constructors of" <+> quotes (ppr tc) <+> ptext (sLit "are not all in scope"))
       2 (text "so you cannot derive an instance for it")

standaloneCtxt :: LHsSigType Name -> SDoc
standaloneCtxt ty = hang (text "In the stand-alone deriving instance for")
                       2 (quotes (ppr ty))

unboxedTyConErr :: String -> MsgDoc
unboxedTyConErr thing =
  text "The last argument of the instance cannot be an unboxed" <+> text thing
