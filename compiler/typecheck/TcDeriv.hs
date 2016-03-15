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
import TcErrors( reportAllUnsolved )
import TcValidity( validDerivPred )
import TcClassDcl( tcATDefault, tcMkDeclCtxt )
import TcEnv
import TcGenDeriv                       -- Deriv stuff
import TcGenGenerics
import InstEnv
import Inst
import FamInstEnv
import TcHsType
import TcMType
import TcSimplify
import TcUnify( buildImplicationFor )
import LoadIface( loadInterfaceForName )
import Module( getModule )

import RnNames( extendGlobalRdrEnvRn )
import RnBinds
import RnEnv
import RnSource   ( addTcgDUs )
import HscTypes
import Avail

import Unify( tcUnifyTy )
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
import THNames ( liftClassKey )
import SrcLoc
import Util
import Outputable
import FastString
import Bag
import Pair
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

-- DerivSpec is purely  local to this module
data DerivSpec theta = DS { ds_loc     :: SrcSpan
                          , ds_name    :: Name           -- DFun name
                          , ds_tvs     :: [TyVar]
                          , ds_theta   :: theta
                          , ds_cls     :: Class
                          , ds_tys     :: [Type]
                          , ds_tc      :: TyCon
                          , ds_overlap :: Maybe OverlapMode
                          , ds_newtype :: Maybe Type }  -- The newtype rep type
        -- This spec implies a dfun declaration of the form
        --       df :: forall tvs. theta => C tys
        -- The Name is the name for the DFun we'll build
        -- The tyvars bind all the variables in the theta
        -- For type families, the tycon in
        --       in ds_tys is the *family* tycon
        --       in ds_tc is the *representation* type
        -- For non-family tycons, both are the same

        -- the theta is either the given and final theta, in standalone deriving,
        -- or the not-yet-simplified list of constraints together with their origin

        -- ds_newtype = Just rep_ty  <=> Generalised Newtype Deriving (GND)
        --              Nothing      <=> Vanilla deriving

{-
Example:

     newtype instance T [a] = MkT (Tree a) deriving( C s )
==>
     axiom T [a] = :RTList a
     axiom :RTList a = Tree a

     DS { ds_tvs = [a,s], ds_cls = C, ds_tys = [s, T [a]]
        , ds_tc = :RTList, ds_newtype = Just (Tree a) }
-}

type DerivContext = Maybe ThetaType
   -- Nothing    <=> Vanilla deriving; infer the context of the instance decl
   -- Just theta <=> Standalone deriving: context supplied by programmer

data PredOrigin = PredOrigin PredType CtOrigin TypeOrKind
type ThetaOrigin = [PredOrigin]

mkPredOrigin :: CtOrigin -> TypeOrKind -> PredType -> PredOrigin
mkPredOrigin origin t_or_k pred = PredOrigin pred origin t_or_k

mkThetaOrigin :: CtOrigin -> TypeOrKind -> ThetaType -> ThetaOrigin
mkThetaOrigin origin t_or_k = map (mkPredOrigin origin t_or_k)

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
        -- See Note [Inferring the instance context]

earlyDSLoc :: EarlyDerivSpec -> SrcSpan
earlyDSLoc (InferTheta spec) = ds_loc spec
earlyDSLoc (GivenTheta spec) = ds_loc spec

splitEarlyDerivSpec :: [EarlyDerivSpec] -> ([DerivSpec ThetaOrigin], [DerivSpec ThetaType])
splitEarlyDerivSpec [] = ([],[])
splitEarlyDerivSpec (InferTheta spec : specs) =
    case splitEarlyDerivSpec specs of (is, gs) -> (spec : is, gs)
splitEarlyDerivSpec (GivenTheta spec : specs) =
    case splitEarlyDerivSpec specs of (is, gs) -> (is, spec : gs)

pprDerivSpec :: Outputable theta => DerivSpec theta -> SDoc
pprDerivSpec (DS { ds_loc = l, ds_name = n, ds_tvs = tvs,
                   ds_cls = c, ds_tys = tys, ds_theta = rhs })
  = hang (text "DerivSpec")
       2 (vcat [ text "ds_loc   =" <+> ppr l
               , text "ds_name  =" <+> ppr n
               , text "ds_tvs   =" <+> ppr tvs
               , text "ds_cls   =" <+> ppr c
               , text "ds_tys   =" <+> ppr tys
               , text "ds_theta =" <+> ppr rhs ])

instance Outputable theta => Outputable (DerivSpec theta) where
  ppr = pprDerivSpec

instance Outputable EarlyDerivSpec where
  ppr (InferTheta spec) = ppr spec <+> text "(Infer)"
  ppr (GivenTheta spec) = ppr spec <+> text "(Given)"

instance Outputable PredOrigin where
  ppr (PredOrigin ty _ _) = ppr ty -- The origin is not so interesting when debugging

{- Note [Inferring the instance context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are two sorts of 'deriving':

  * InferTheta: the deriving clause for a data type
      data T a = T1 a deriving( Eq )
    Here we must infer an instance context,
    and generate instance declaration
      instance Eq a => Eq (T a) where ...

  * CheckTheta: standalone deriving
      deriving instance Eq a => Eq (T a)
    Here we only need to fill in the bindings;
    the instance context is user-supplied

For a deriving clause (InferTheta) we must figure out the
instance context (inferConstraints). Suppose we are inferring
the instance context for
    C t1 .. tn (T s1 .. sm)
There are two cases

  * (T s1 .. sm) :: *         (the normal case)
    Then we behave like Eq and guess (C t1 .. tn t)
    for each data constructor arg of type t.  More
    details below.

  * (T s1 .. sm) :: * -> *    (the functor-like case)
    Then we behave like Functor.

In both cases we produce a bunch of un-simplified constraints
and them simplify them in simplifyInstanceContexts; see
Note [Simplifying the instance context].


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

-- | Stuff needed to process a `deriving` clause
data DerivInfo = DerivInfo { di_rep_tc :: TyCon
                             -- ^ The data tycon for normal datatypes,
                             -- or the *representation* tycon for data families
                           , di_preds  :: [LHsSigType Name]
                           , di_ctxt   :: SDoc -- ^ error context
                           }

-- | Extract `deriving` clauses of proper data type (skips data families)
mkDerivInfos :: [TyClGroup Name] -> TcM [DerivInfo]
mkDerivInfos tycls = concatMapM mk_derivs tycls
  where
    mk_derivs (TyClGroup { group_tyclds = decls })
      = concatMapM (mk_deriv . unLoc) decls

    mk_deriv decl@(DataDecl { tcdLName = L _ data_name
                            , tcdDataDefn =
                                HsDataDefn { dd_derivs = Just (L _ preds) } })
      = do { tycon <- tcLookupTyCon data_name
           ; return [DerivInfo { di_rep_tc = tycon, di_preds = preds
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
        ; let (binds, famInsts, extraInstances) =
                genAuxBinds loc (unionManyBags deriv_stuff)

        ; dflags <- getDynFlags

        ; (inst_info, rn_binds, rn_dus) <-
            renameDeriv is_boot (inst_infos ++ (bagToList extraInstances)) binds

        ; unless (isEmptyBag inst_info) $
             liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances"
                        (ddump_deriving inst_info rn_binds famInsts))

        ; gbl_env <- tcExtendLocalFamInstEnv (bagToList famInsts) $
                     tcExtendLocalInstEnv (map iSpec (bagToList inst_info)) getGblEnv
        ; let all_dus = rn_dus `plusDU` usesOnly (mkFVs $ catMaybes maybe_fvs)
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
deriveDerivInfo (DerivInfo { di_rep_tc = rep_tc, di_preds = preds
                           , di_ctxt = err_ctxt })
  = addErrCtxt err_ctxt $
    concatMapM (deriveTyData tvs tc tys) preds
  where
    tvs = tyConTyVars rep_tc
    (tc, tys) = case tyConFamInstSig_maybe rep_tc of
                        -- data family:
                  Just (fam_tc, pats, _) -> (fam_tc, pats)
      -- NB: deriveTyData wants the *user-specified*
      -- name. See Note [Why we don't pass rep_tc into deriveTyData]

                  _ -> (rep_tc, mkTyVarTys tvs)     -- datatype

------------------------------------------------------------------
deriveStandalone :: LDerivDecl Name -> TcM [EarlyDerivSpec]
-- Standalone deriving declarations
--  e.g.   deriving instance Show a => Show (T a)
-- Rather like tcLocalInstDecl
deriveStandalone (L loc (DerivDecl deriv_ty overlap_mode))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do { traceTc "Standalone deriving decl for" (ppr deriv_ty)
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

       ; case tcSplitTyConApp_maybe inst_ty of
           Just (tc, tc_args)
              | className cls == typeableClassName
              -> do warnUselessTypeable
                    return []

              | isAlgTyCon tc || isDataFamilyTyCon tc  -- All other classes
              -> do { spec <- mkEqnHelp (fmap unLoc overlap_mode)
                                        tvs cls cls_tys tc tc_args
                                        (Just theta)
                    ; return [spec] }

           _  -> -- Complain about functions, primitive types, etc,
                 failWithTc $ derivingThingErr False cls cls_tys inst_ty $
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
             -> LHsSigType Name              -- The deriving predicate
             -> TcM [EarlyDerivSpec]
-- The deriving clause of a data or newtype declaration
-- I.e. not standalone deriving
deriveTyData tvs tc tc_args deriv_pred
  = setSrcSpan (getLoc (hsSigType deriv_pred)) $  -- Use loc of the 'deriving' item
    do  { (deriv_tvs, cls, cls_tys, cls_arg_kind)
                <- tcExtendTyVarEnv tvs $
                   tcHsDeriv deriv_pred
                -- Deriving preds may (now) mention
                -- the type variables for the type constructor, hence tcExtendTyVarenv
                -- The "deriv_pred" is a LHsType to take account of the fact that for
                -- newtype deriving we allow deriving (forall a. C [a]).

                -- Typeable is special, because Typeable :: forall k. k -> Constraint
                -- so the argument kind 'k' is not decomposable by splitKindFunTys
                -- as is the case for all other derivable type classes
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
              -- Use exactTyCoVarsOfTypes, not tyCoVarsOfTypes, so that we
              -- don't mistakenly grab a type variable mentioned in a type
              -- synonym that drops it.
              -- See Note [Eta-reducing type synonyms].
              dropped_tvs     = exactTyCoVarsOfTypes args_to_drop

              -- Match up the kinds, and apply the resulting kind substitution
              -- to the types.  See Note [Unify kinds in deriving]
              -- We are assuming the tycon tyvars and the class tyvars are distinct
              mb_match        = tcUnifyTy inst_ty_kind cls_arg_kind
              Just kind_subst = mb_match

              all_tkvs        = varSetElemsWellScoped $
                                mkVarSet deriv_tvs `unionVarSet`
                                tyCoVarsOfTypes tc_args_to_keep
              unmapped_tkvs   = filter (`notElemTCvSubst` kind_subst) all_tkvs
              (subst, tkvs)   = mapAccumL substTyVarBndr
                                          kind_subst unmapped_tkvs
              final_tc_args   = substTys subst tc_args_to_keep
              final_cls_tys   = substTys subst cls_tys

        ; traceTc "derivTyData1" (vcat [ pprTvBndrs tvs, ppr tc, ppr tc_args, ppr deriv_pred
                                       , pprTvBndrs (tyCoVarsOfTypesList tc_args)
                                       , ppr n_args_to_keep, ppr n_args_to_drop
                                       , ppr inst_ty_kind, ppr cls_arg_kind, ppr mb_match
                                       , ppr final_tc_args, ppr final_cls_tys ])

        -- Check that the result really is well-kinded
        ; checkTc (n_args_to_keep >= 0 && isJust mb_match)
                  (derivingKindErr tc cls cls_tys cls_arg_kind)

        ; traceTc "derivTyData2" (vcat [ ppr tkvs ])

        ; checkTc (allDistinctTyVars args_to_drop &&              -- (a) and (b)
                   not (any (`elemVarSet` dropped_tvs) tkvs))     -- (c)
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

        ; spec <- mkEqnHelp Nothing tkvs
                            cls final_cls_tys tc final_tc_args Nothing
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
where this was first noticed).

For this reason, we call exactTyCoVarsOfTypes on the eta-reduced types so that
we only consider the type variables that remain after expanding through type
synonyms.
-}

mkEqnHelp :: Maybe OverlapMode
          -> [TyVar]
          -> Class -> [Type]
          -> TyCon -> [Type]
          -> DerivContext       -- Just    => context supplied (standalone deriving)
                                -- Nothing => context inferred (deriving on data decl)
          -> TcRn EarlyDerivSpec
-- Make the EarlyDerivSpec for an instance
--      forall tvs. theta => cls (tys ++ [ty])
-- where the 'theta' is optional (that's the Maybe part)
-- Assumes that this declaration is well-kinded

mkEqnHelp overlap_mode tvs cls cls_tys tycon tc_args mtheta
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
             not_in_scope dc  = null (lookupGRE_Name rdr_env dc)

       ; addUsedDataCons rdr_env rep_tc
       ; unless (isNothing mtheta || not hidden_data_cons)
                (bale_out (derivingHiddenErr tycon))

       ; dflags <- getDynFlags
       ; if isDataTyCon rep_tc then
            mkDataTypeEqn dflags overlap_mode tvs cls cls_tys
                          tycon tc_args rep_tc rep_tc_args mtheta
         else
            mkNewTypeEqn dflags overlap_mode tvs cls cls_tys
                         tycon tc_args rep_tc rep_tc_args mtheta }
  where
     bale_out msg = failWithTc (derivingThingErr False cls cls_tys (mkTyConApp tycon tc_args) msg)

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
              -> TcRn EarlyDerivSpec    -- Return 'Nothing' if error

mkDataTypeEqn dflags overlap_mode tvs cls cls_tys
              tycon tc_args rep_tc rep_tc_args mtheta
  = case checkSideConditions dflags mtheta cls cls_tys rep_tc rep_tc_args of
        -- NB: pass the *representation* tycon to checkSideConditions
        NonDerivableClass   msg -> bale_out (nonStdErr cls $$ msg)
        DerivableClassError msg -> bale_out msg
        CanDerive               -> go_for_it
        DerivableViaInstance    -> go_for_it
  where
    go_for_it    = mk_data_eqn overlap_mode tvs cls cls_tys tycon tc_args rep_tc rep_tc_args mtheta
    bale_out msg = failWithTc (derivingThingErr False cls cls_tys (mkTyConApp tycon tc_args) msg)

mk_data_eqn :: Maybe OverlapMode -> [TyVar] -> Class -> [Type]
            -> TyCon -> [TcType] -> TyCon -> [TcType] -> DerivContext
            -> TcM EarlyDerivSpec
mk_data_eqn overlap_mode tvs cls cls_tys tycon tc_args rep_tc rep_tc_args mtheta
  = do loc                  <- getSrcSpanM
       dfun_name            <- newDFunName' cls tycon
       case mtheta of
        Nothing -> do --Infer context
            inferred_constraints <- inferConstraints cls cls_tys inst_ty rep_tc rep_tc_args
            return $ InferTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs
                   , ds_cls = cls, ds_tys = inst_tys
                   , ds_tc = rep_tc
                   , ds_theta = inferred_constraints
                   , ds_overlap = overlap_mode
                   , ds_newtype = Nothing }
        Just theta -> do -- Specified context
            return $ GivenTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs
                   , ds_cls = cls, ds_tys = inst_tys
                   , ds_tc = rep_tc
                   , ds_theta = theta
                   , ds_overlap = overlap_mode
                   , ds_newtype = Nothing }
  where
    inst_ty  = mkTyConApp tycon tc_args
    inst_tys = cls_tys ++ [inst_ty]

----------------------

inferConstraints :: Class -> [TcType] -> TcType
                 -> TyCon -> [TcType]
                 -> TcM ThetaOrigin
-- inferConstraints figures out the constraints needed for the
-- instance declaration generated by a 'deriving' clause on a
-- data type declaration.
-- See Note [Inferring the instance context]

-- e.g. inferConstraints
--        C Int (T [a])    -- Class and inst_tys
--        :RTList a        -- Rep tycon and its arg tys
-- where T [a] ~R :RTList a
--
-- Generate a sufficiently large set of constraints that typechecking the
-- generated method definitions should succeed.   This set will be simplified
-- before being used in the instance declaration
inferConstraints main_cls cls_tys inst_ty rep_tc rep_tc_args
  | main_cls `hasKey` genClassKey    -- Generic constraints are easy
  = return []

  | main_cls `hasKey` gen1ClassKey   -- Gen1 needs Functor
  = ASSERT( length rep_tc_tvs > 0 )   -- See Note [Getting base classes]
    ASSERT( null cls_tys )
    do { functorClass <- tcLookupClass functorClassName
       ; return (con_arg_constraints (get_gen1_constraints functorClass)) }

  | otherwise  -- The others are a bit more complicated
  = ASSERT2( equalLength rep_tc_tvs all_rep_tc_args
           , ppr main_cls <+> ppr rep_tc
             $$ ppr rep_tc_tvs $$ ppr all_rep_tc_args )
    do { traceTc "inferConstraints" (vcat [ppr main_cls <+> ppr inst_tys, ppr arg_constraints])
       ; return (stupid_constraints ++ extra_constraints
                 ++ sc_constraints
                 ++ arg_constraints) }
  where
    tc_binders = tyConBinders rep_tc
    choose_level bndr
      | isNamedBinder bndr = KindLevel
      | otherwise          = TypeLevel
    t_or_ks = map choose_level tc_binders ++ repeat TypeLevel
       -- want to report *kind* errors when possible

    arg_constraints = con_arg_constraints get_std_constrained_tys

       -- Constraints arising from the arguments of each constructor
    con_arg_constraints :: (CtOrigin -> TypeOrKind -> Type -> [PredOrigin])
                        -> [PredOrigin]
    con_arg_constraints get_arg_constraints
      = [ pred
        | data_con <- tyConDataCons rep_tc
        , (arg_n, arg_t_or_k, arg_ty)
            <- zip3 [1..] t_or_ks $
               dataConInstOrigArgTys data_con all_rep_tc_args
        , not (isUnliftedType arg_ty)
        , let orig = DerivOriginDC data_con arg_n
        , pred <- get_arg_constraints orig arg_t_or_k arg_ty ]

                -- No constraints for unlifted types
                -- See Note [Deriving and unboxed types]

    -- is_functor_like: see Note [Inferring the instance context]
    is_functor_like = typeKind inst_ty `tcEqKind` typeToTypeKind

    get_gen1_constraints functor_cls orig t_or_k ty
       = mk_functor_like_constraints orig t_or_k functor_cls $
         get_gen1_constrained_tys last_tv ty

    get_std_constrained_tys :: CtOrigin -> TypeOrKind -> Type -> [PredOrigin]
    get_std_constrained_tys orig t_or_k ty
        | is_functor_like = mk_functor_like_constraints orig t_or_k main_cls $
                            deepSubtypesContaining last_tv ty
        | otherwise       = [mk_cls_pred orig t_or_k main_cls ty]

    mk_functor_like_constraints :: CtOrigin -> TypeOrKind
                                -> Class -> [Type] -> [PredOrigin]
    -- 'cls' is usually main_cls (Functor or Traversable etc), but if
    -- main_cls = Generic1, then 'cls' can be Functor; see get_gen1_constraints
    --
    -- For each type, generate two constraints: (cls ty, kind(ty) ~ (*->*))
    -- The second constraint checks that the first is well-kinded.
    -- Lacking that, as Trac #10561 showed, we can just generate an
    -- ill-kinded instance.
    mk_functor_like_constraints orig t_or_k cls tys
       = [ pred_o
         | ty <- tys
         , pred_o <- [ mk_cls_pred orig t_or_k cls ty
                     , mkPredOrigin orig KindLevel
                         (mkPrimEqPred (typeKind ty) typeToTypeKind) ] ]

    rep_tc_tvs      = tyConTyVars rep_tc
    last_tv         = last rep_tc_tvs
    all_rep_tc_args | is_functor_like = rep_tc_args ++ [mkTyVarTy last_tv]
                    | otherwise       = rep_tc_args

        -- Constraints arising from superclasses
        -- See Note [Superclasses of derived instance]
    cls_tvs  = classTyVars main_cls
    inst_tys = cls_tys ++ [inst_ty]
    sc_constraints = ASSERT2( equalLength cls_tvs inst_tys, ppr main_cls <+> ppr rep_tc)
                     mkThetaOrigin DerivOrigin TypeLevel $
                     substTheta cls_subst (classSCTheta main_cls)
    cls_subst = ASSERT( equalLength cls_tvs inst_tys )
                zipTvSubst cls_tvs inst_tys

        -- Stupid constraints
    stupid_constraints = mkThetaOrigin DerivOrigin TypeLevel $
                         substTheta tc_subst (tyConStupidTheta rep_tc)
    tc_subst = ASSERT( equalLength rep_tc_tvs all_rep_tc_args )
               zipTvSubst rep_tc_tvs all_rep_tc_args

        -- Extra Data constraints
        -- The Data class (only) requires that for
        --    instance (...) => Data (T t1 t2)
        -- IF   t1:*, t2:*
        -- THEN (Data t1, Data t2) are among the (...) constraints
        -- Reason: when the IF holds, we generate a method
        --             dataCast2 f = gcast2 f
        --         and we need the Data constraints to typecheck the method
    extra_constraints
      | main_cls `hasKey` dataClassKey
      , all (isLiftedTypeKind . typeKind) rep_tc_args
      = [ mk_cls_pred DerivOrigin t_or_k main_cls ty
        | (t_or_k, ty) <- zip t_or_ks rep_tc_args]
      | otherwise
      = []

    mk_cls_pred orig t_or_k cls ty   -- Don't forget to apply to cls_tys too
                              -- In the awkward Generic1 casde, cls_tys is empty
       = mkPredOrigin orig t_or_k (mkClassPred cls (cls_tys ++ [ty]))

{- Note [Getting base classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functor and Typeable are defined in package 'base', and that is not available
when compiling 'ghc-prim'.  So we must be careful that 'deriving' for stuff in
ghc-prim does not use Functor or Typeable implicitly via these lookups.

Note [Deriving and unboxed types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have some special hacks to support things like
   data T = MkT Int# deriving ( Show )

Specifically, we use TcGenDeriv.box to box the Int# into an Int
(which we know how to show), and append a '#'. Parenthesis are not required
for unboxed values (`MkT -3#` is a valid expression).

Note [Deriving any class]
~~~~~~~~~~~~~~~~~~~~~~~~~
Classic uses of a deriving clause, or a standalone-deriving declaration, are
for:
  * a built-in class like Eq or Show, for which GHC knows how to generate
    the instance code
  * a newtype, via the mechanism enabled by GeneralizedNewtypeDeriving

The DeriveAnyClass extension adds a third way to derive instances, based on
empty instance declarations.

The canonical use case is in combination with GHC.Generics and default method
signatures. These allow us to have instance declarations being empty, but still
useful, e.g.

  data T a = ...blah..blah... deriving( Generic )
  instance C a => C (T a)  -- No 'where' clause

where C is some "random" user-defined class.

This boilerplate code can be replaced by the more compact

  data T a = ...blah..blah... deriving( Generic, C )

if DeriveAnyClass is enabled.

This is not restricted to Generics; any class can be derived, simply giving
rise to an empty instance.

Unfortunately, it is not clear how to determine the context (in case of
standard deriving; in standalone deriving, the user provides the context).
GHC uses the same heuristic for figuring out the class context that it uses for
Eq in the case of *-kinded classes, and for Functor in the case of
* -> *-kinded classes. That may not be optimal or even wrong. But in such
cases, standalone deriving can still be used.
-}

------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

data DerivStatus = CanDerive                 -- Standard class, can derive
                 | DerivableClassError SDoc  -- Standard class, but can't do it
                 | DerivableViaInstance      -- See Note [Deriving any class]
                 | NonDerivableClass SDoc    -- Non-standard class

-- A "standard" class is one defined in the Haskell report which GHC knows how
-- to generate code for, such as Eq, Ord, Ix, etc.

checkSideConditions :: DynFlags -> DerivContext -> Class -> [TcType]
                    -> TyCon -> [Type] -- tycon and its parameters
                    -> DerivStatus
checkSideConditions dflags mtheta cls cls_tys rep_tc rep_tc_args
  | Just cond <- sideConditions mtheta cls
  = case (cond (dflags, rep_tc, rep_tc_args)) of
        NotValid err -> DerivableClassError err  -- Class-specific error
        IsValid  | null cls_tys -> CanDerive     -- All derivable classes are unary, so
                                                 -- cls_tys (the type args other than last)
                                                 -- should be null
                 | otherwise    -> DerivableClassError (classArgsErr cls cls_tys)  -- e.g. deriving( Eq s )

  | Just err <- canDeriveAnyClass dflags rep_tc cls
  = NonDerivableClass err  -- DeriveAnyClass does not work

  | otherwise
  = DerivableViaInstance   -- DeriveAnyClass should work


classArgsErr :: Class -> [Type] -> SDoc
classArgsErr cls cls_tys = quotes (ppr (mkClassPred cls cls_tys)) <+> text "is not a class"

nonStdErr :: Class -> SDoc
nonStdErr cls =
      quotes (ppr cls)
  <+> text "is not a standard derivable class (Eq, Show, etc.)"

sideConditions :: DerivContext -> Class -> Maybe Condition
-- Side conditions for classes that GHC knows about,
-- that is, "deriviable classes"
-- Returns Nothing for a non-derivable class
sideConditions mtheta cls
  | cls_key == eqClassKey          = Just (cond_std `andCond` cond_args cls)
  | cls_key == ordClassKey         = Just (cond_std `andCond` cond_args cls)
  | cls_key == showClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == readClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == enumClassKey        = Just (cond_std `andCond` cond_isEnumeration)
  | cls_key == ixClassKey          = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == boundedClassKey     = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == dataClassKey        = Just (checkFlag LangExt.DeriveDataTypeable `andCond`
                                           cond_std `andCond`
                                           cond_args cls)
  | cls_key == functorClassKey     = Just (checkFlag LangExt.DeriveFunctor `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK True False)
  | cls_key == foldableClassKey    = Just (checkFlag LangExt.DeriveFoldable `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK False True)
                                           -- Functor/Fold/Trav works ok
                                           -- for rank-n types
  | cls_key == traversableClassKey = Just (checkFlag LangExt.DeriveTraversable `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK False False)
  | cls_key == genClassKey         = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                           cond_vanilla `andCond`
                                           cond_RepresentableOk)
  | cls_key == gen1ClassKey        = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                           cond_vanilla `andCond`
                                           cond_Representable1Ok)
  | cls_key == liftClassKey        = Just (checkFlag LangExt.DeriveLift `andCond`
                                           cond_vanilla `andCond`
                                           cond_args cls)
  | otherwise                      = Nothing
  where
    cls_key = getUnique cls
    cond_std     = cond_stdOK mtheta False  -- Vanilla data constructors, at least one,
                                            --    and monotype arguments
    cond_vanilla = cond_stdOK mtheta True   -- Vanilla data constructors but
                                            --   allow no data cons or polytype arguments

canDeriveAnyClass :: DynFlags -> TyCon -> Class -> Maybe SDoc
-- Nothing: we can (try to) derive it via an empty instance declaration
-- Just s:  we can't, reason s
-- Precondition: the class is not one of the standard ones
canDeriveAnyClass dflags _tycon clas
  | not (xopt LangExt.DeriveAnyClass dflags)
  = Just (text "Try enabling DeriveAnyClass")
  | not (any (target_kind `tcEqKind`) [ liftedTypeKind, typeToTypeKind ])
  = Just (text "The last argument of class" <+> quotes (ppr clas)
          <+> text "does not have kind * or (* -> *)")
  | otherwise
  = Nothing   -- OK!
  where
    -- We are making an instance  (C t1 .. tn (T s1 .. sm))
    -- and we can only do so if the kind of C's last argument
    -- is * or (* -> *).  Because only then can we make a reasonable
    -- guess at the instance context
    target_kind = tyVarKind (last (classTyVars clas))

typeToTypeKind :: Kind
typeToTypeKind = liftedTypeKind `mkFunTy` liftedTypeKind

type Condition = (DynFlags, TyCon, [Type]) -> Validity
        -- first Bool is whether or not we are allowed to derive Data and Typeable
        -- second Bool is whether or not we are allowed to derive Functor
        -- TyCon is the *representation* tycon if the data type is an indexed one
        -- [Type] are the type arguments to the (representation) TyCon
        -- Nothing => OK

orCond :: Condition -> Condition -> Condition
orCond c1 c2 tc
  = case (c1 tc, c2 tc) of
     (IsValid,    _)          -> IsValid    -- c1 succeeds
     (_,          IsValid)    -> IsValid    -- c21 succeeds
     (NotValid x, NotValid y) -> NotValid (x $$ text "  or" $$ y)
                                            -- Both fail

andCond :: Condition -> Condition -> Condition
andCond c1 c2 tc = c1 tc `andValid` c2 tc

cond_stdOK :: DerivContext -- Says whether this is standalone deriving or not;
                           --     if standalone, we just say "yes, go for it"
           -> Bool         -- True <=> permissive: allow higher rank
                           --          args and no data constructors
           -> Condition
cond_stdOK (Just _) _ _
  = IsValid     -- Don't check these conservative conditions for
                -- standalone deriving; just generate the code
                -- and let the typechecker handle the result
cond_stdOK Nothing permissive (_, rep_tc, _)
  | null data_cons
  , not permissive      = NotValid (no_cons_why rep_tc $$ suggestion)
  | not (null con_whys) = NotValid (vcat con_whys $$ suggestion)
  | otherwise           = IsValid
  where
    suggestion = text "Possible fix: use a standalone deriving declaration instead"
    data_cons  = tyConDataCons rep_tc
    con_whys   = getInvalids (map check_con data_cons)

    check_con :: DataCon -> Validity
    check_con con
      | not (isVanillaDataCon con)
      = NotValid (badCon con (text "has existentials or constraints in its type"))
      | not (permissive || all isTauTy (dataConOrigArgTys con))
      = NotValid (badCon con (text "has a higher-rank type"))
      | otherwise
      = IsValid

no_cons_why :: TyCon -> SDoc
no_cons_why rep_tc = quotes (pprSourceTyCon rep_tc) <+>
                     text "must have at least one data constructor"

cond_RepresentableOk :: Condition
cond_RepresentableOk (dflags, tc, tc_args) = canDoGenerics dflags tc tc_args

cond_Representable1Ok :: Condition
cond_Representable1Ok (dflags, tc, tc_args) = canDoGenerics1 dflags tc tc_args

cond_enumOrProduct :: Class -> Condition
cond_enumOrProduct cls = cond_isEnumeration `orCond`
                         (cond_isProduct `andCond` cond_args cls)

cond_args :: Class -> Condition
-- For some classes (eg Eq, Ord) we allow unlifted arg types
-- by generating specialised code.  For others (eg Data) we don't.
cond_args cls (_, tc, _)
  = case bad_args of
      []     -> IsValid
      (ty:_) -> NotValid (hang (text "Don't know how to derive" <+> quotes (ppr cls))
                             2 (text "for type" <+> quotes (ppr ty)))
  where
    bad_args = [ arg_ty | con <- tyConDataCons tc
                        , arg_ty <- dataConOrigArgTys con
                        , isUnliftedType arg_ty
                        , not (ok_ty arg_ty) ]

    cls_key = classKey cls
    ok_ty arg_ty
     | cls_key == eqClassKey   = check_in arg_ty ordOpTbl
     | cls_key == ordClassKey  = check_in arg_ty ordOpTbl
     | cls_key == showClassKey = check_in arg_ty boxConTbl
     | cls_key == liftClassKey = check_in arg_ty litConTbl
     | otherwise               = False    -- Read, Ix etc

    check_in :: Type -> [(Type,a)] -> Bool
    check_in arg_ty tbl = any (eqType arg_ty . fst) tbl


cond_isEnumeration :: Condition
cond_isEnumeration (_, rep_tc, _)
  | isEnumerationTyCon rep_tc = IsValid
  | otherwise                 = NotValid why
  where
    why = sep [ quotes (pprSourceTyCon rep_tc) <+>
                  text "must be an enumeration type"
              , text "(an enumeration consists of one or more nullary, non-GADT constructors)" ]
                  -- See Note [Enumeration types] in TyCon

cond_isProduct :: Condition
cond_isProduct (_, rep_tc, _)
  | isProductTyCon rep_tc = IsValid
  | otherwise             = NotValid why
  where
    why = quotes (pprSourceTyCon rep_tc) <+>
          text "must have precisely one constructor"

cond_functorOK :: Bool -> Bool -> Condition
-- OK for Functor/Foldable/Traversable class
-- Currently: (a) at least one argument
--            (b) don't use argument contravariantly
--            (c) don't use argument in the wrong place, e.g. data T a = T (X a a)
--            (d) optionally: don't use function types
--            (e) no "stupid context" on data type
cond_functorOK allowFunctions allowExQuantifiedLastTyVar (_, rep_tc, _)
  | null tc_tvs
  = NotValid (text "Data type" <+> quotes (ppr rep_tc)
              <+> text "must have some type parameters")

  | not (null bad_stupid_theta)
  = NotValid (text "Data type" <+> quotes (ppr rep_tc)
              <+> text "must not have a class context:" <+> pprTheta bad_stupid_theta)

  | otherwise
  = allValid (map check_con data_cons)
  where
    tc_tvs            = tyConTyVars rep_tc
    Just (_, last_tv) = snocView tc_tvs
    bad_stupid_theta  = filter is_bad (tyConStupidTheta rep_tc)
    is_bad pred       = last_tv `elemVarSet` tyCoVarsOfType pred

    data_cons = tyConDataCons rep_tc
    check_con con = allValid (check_universal con : foldDataConArgs (ft_check con) con)

    check_universal :: DataCon -> Validity
    check_universal con
      | allowExQuantifiedLastTyVar
      = IsValid -- See Note [DeriveFoldable with ExistentialQuantification]
                -- in TcGenDeriv
      | Just tv <- getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con)))
      , tv `elem` dataConUnivTyVars con
      , not (tv `elemVarSet` tyCoVarsOfTypes (dataConTheta con))
      = IsValid   -- See Note [Check that the type variable is truly universal]
      | otherwise
      = NotValid (badCon con existential)

    ft_check :: DataCon -> FFoldType Validity
    ft_check con = FT { ft_triv = IsValid, ft_var = IsValid
                      , ft_co_var = NotValid (badCon con covariant)
                      , ft_fun = \x y -> if allowFunctions then x `andValid` y
                                                           else NotValid (badCon con functions)
                      , ft_tup = \_ xs  -> allValid xs
                      , ft_ty_app = \_ x   -> x
                      , ft_bad_app = NotValid (badCon con wrong_arg)
                      , ft_forall = \_ x   -> x }

    existential = text "must be truly polymorphic in the last argument of the data type"
    covariant   = text "must not use the type variable in a function argument"
    functions   = text "must not contain function types"
    wrong_arg   = text "must use the type variable only as the last argument of a data type"

checkFlag :: LangExt.Extension -> Condition
checkFlag flag (dflags, _, _)
  | xopt flag dflags = IsValid
  | otherwise        = NotValid why
  where
    why = text "You need " <> text flag_str
          <+> text "to derive an instance for this class"
    flag_str = case [ flagSpecName f | f <- xFlags , flagSpecFlag f == flag ] of
                 [s]   -> s
                 other -> pprPanic "checkFlag" (ppr other)

std_class_via_coercible :: Class -> Bool
-- These standard classes can be derived for a newtype
-- using the coercible trick *even if no -XGeneralizedNewtypeDeriving
-- because giving so gives the same results as generating the boilerplate
std_class_via_coercible clas
  = classKey clas `elem` [eqClassKey, ordClassKey, ixClassKey, boundedClassKey]
        -- Not Read/Show/Lift because they respect the type
        -- Not Enum, because newtypes are never in Enum


non_coercible_class :: Class -> Bool
-- *Never* derive Read, Show, Typeable, Data, Generic, Generic1, Lift
-- by Coercible, even with -XGeneralizedNewtypeDeriving
-- Also, avoid Traversable, as the Coercible-derived instance and the "normal"-derived
-- instance behave differently if there's a non-lawful Applicative out there.
-- Besides, with roles, Coercible-deriving Traversable is ill-roled.
non_coercible_class cls
  = classKey cls `elem` ([ readClassKey, showClassKey, dataClassKey
                         , genClassKey, gen1ClassKey, typeableClassKey
                         , traversableClassKey, liftClassKey ])

badCon :: DataCon -> SDoc -> SDoc
badCon con msg = text "Constructor" <+> quotes (ppr con) <+> msg

{-
Note [Check that the type variable is truly universal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Functor and Traversable instances, we must check that the *last argument*
of the type constructor is used truly universally quantified.  Example

   data T a b where
     T1 :: a -> b -> T a b      -- Fine! Vanilla H-98
     T2 :: b -> c -> T a b      -- Fine! Existential c, but we can still map over 'b'
     T3 :: b -> T Int b         -- Fine! Constraint 'a', but 'b' is still polymorphic
     T4 :: Ord b => b -> T a b  -- No!  'b' is constrained
     T5 :: b -> T b b           -- No!  'b' is constrained
     T6 :: T a (b,b)            -- No!  'b' is constrained

Notice that only the first of these constructors is vanilla H-98. We only
need to take care about the last argument (b in this case).  See Trac #8678.
Eg. for T1-T3 we can write

     fmap f (T1 a b) = T1 a (f b)
     fmap f (T2 b c) = T2 (f b) c
     fmap f (T3 x)   = T3 (f x)

We need not perform these checks for Foldable instances, however, since
functions in Foldable can only consume existentially quantified type variables,
rather than produce them (as is the case in Functor and Traversable functions.)
As a result, T can have a derived Foldable instance:

    foldr f z (T1 a b) = f b z
    foldr f z (T2 b c) = f b z
    foldr f z (T3 x)   = f x z
    foldr f z (T4 x)   = f x z
    foldr f z (T5 x)   = f x z
    foldr _ z T6       = z

See Note [DeriveFoldable with ExistentialQuantification] in TcGenDeriv.


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


************************************************************************
*                                                                      *
                Deriving newtypes
*                                                                      *
************************************************************************
-}

mkNewTypeEqn :: DynFlags -> Maybe OverlapMode -> [TyVar] -> Class
             -> [Type] -> TyCon -> [Type] -> TyCon -> [Type]
             -> DerivContext
             -> TcRn EarlyDerivSpec
mkNewTypeEqn dflags overlap_mode tvs
             cls cls_tys tycon tc_args rep_tycon rep_tc_args mtheta
-- Want: instance (...) => cls (cls_tys ++ [tycon tc_args]) where ...
  | ASSERT( length cls_tys + 1 == classArity cls )
    might_derive_via_coercible && ((newtype_deriving && not deriveAnyClass)
                                  || std_class_via_coercible cls)
  = do traceTc "newtype deriving:" (ppr tycon <+> ppr rep_tys <+> ppr all_preds)
       dfun_name <- newDFunName' cls tycon
       loc <- getSrcSpanM
       case mtheta of
        Just theta -> return $ GivenTheta $ DS
            { ds_loc = loc
            , ds_name = dfun_name, ds_tvs = varSetElemsWellScoped dfun_tvs
            , ds_cls = cls, ds_tys = inst_tys
            , ds_tc = rep_tycon
            , ds_theta = theta
            , ds_overlap = overlap_mode
            , ds_newtype = Just rep_inst_ty }
        Nothing -> return $ InferTheta $ DS
            { ds_loc = loc
            , ds_name = dfun_name, ds_tvs = varSetElemsWellScoped dfun_tvs
            , ds_cls = cls, ds_tys = inst_tys
            , ds_tc = rep_tycon
            , ds_theta = all_preds
            , ds_overlap = overlap_mode
            , ds_newtype = Just rep_inst_ty }
  | otherwise
  = case checkSideConditions dflags mtheta cls cls_tys rep_tycon rep_tc_args of
      -- Error with standard class
      DerivableClassError msg
        | might_derive_via_coercible -> bale_out (msg $$ suggest_gnd)
        | otherwise                  -> bale_out msg

      -- Must use newtype deriving or DeriveAnyClass
      NonDerivableClass _msg
        -- Too hard, even with newtype deriving
        | newtype_deriving           -> bale_out cant_derive_err
        -- Try newtype deriving!
        -- Here we suggest GeneralizedNewtypeDeriving even in cases where it may
        -- not be applicable. See Trac #9600.
        | otherwise                  -> bale_out (non_std $$ suggest_gnd)

      -- CanDerive/DerivableViaInstance
      _ -> do when (newtype_deriving && deriveAnyClass) $
                addWarnTc NoReason
                          (sep [ text "Both DeriveAnyClass and GeneralizedNewtypeDeriving are enabled"
                               , text "Defaulting to the DeriveAnyClass strategy for instantiating" <+> ppr cls ])
              go_for_it
  where
        newtype_deriving  = xopt LangExt.GeneralizedNewtypeDeriving dflags
        deriveAnyClass    = xopt LangExt.DeriveAnyClass             dflags
        go_for_it         = mk_data_eqn overlap_mode tvs cls cls_tys tycon tc_args
                              rep_tycon rep_tc_args mtheta
        bale_out    = bale_out' newtype_deriving
        bale_out' b = failWithTc . derivingThingErr b cls cls_tys inst_ty

        non_std     = nonStdErr cls
        suggest_gnd = text "Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension"

        -- Here is the plan for newtype derivings.  We see
        --        newtype T a1...an = MkT (t ak+1...an) deriving (.., C s1 .. sm, ...)
        -- where t is a type,
        --       ak+1...an is a suffix of a1..an, and are all tyars
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
        dfun_tvs   = tyCoVarsOfTypes inst_tys
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
            [ let (Pair t1 t2) = mkCoerceClassMethEqn cls (varSetElemsWellScoped dfun_tvs) inst_tys rep_inst_ty meth
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
           && eta_ok
           && ats_ok
--         && not (isRecursiveTyCon tycon)      -- Note [Recursive newtypes]

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

Note that newtype deriving might fail, even after we commit to it. This
is because the derived instance uses `coerce`, which must satisfy its
`Coercible` constraint. This is different than other deriving scenarios,
where we're sure that the resulting instance will type-check.

************************************************************************
*                                                                      *
         Finding the fixed point of deriving equations
*                                                                      *
************************************************************************

Note [Simplifying the instance context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

        data T a b = C1 (Foo a) (Bar b)
                   | C2 Int (T b a)
                   | C3 (T a a)
                   deriving (Eq)

We want to come up with an instance declaration of the form

        instance (Ping a, Pong b, ...) => Eq (T a b) where
                x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely Ping, Pong and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

        Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the data decl.  This part
is done by inferConstraints.

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3


Foo and Bar may have explicit instances for Eq, in which case we can
just substitute for them.  Alternatively, either or both may have
their Eq instances given by deriving clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  This is done by simplifyInstanceConstraints.
Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

        Eq (T a b) = {}         -- The empty set

Next iteration:
        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b u {} u {} u {}
                   = Eq a u Ping b

Next iteration:

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

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

-}


simplifyInstanceContexts :: [DerivSpec ThetaOrigin] -> TcM [DerivSpec ThetaType]
-- Used only for deriving clauses (InferTheta)
-- not for standalone deriving
-- See Note [Simplifying the instance context]

simplifyInstanceContexts [] = return []

simplifyInstanceContexts infer_specs
  = do  { traceTc "simplifyInstanceContexts" $ vcat (map pprDerivSpec infer_specs)
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
    iterate_deriv :: Int -> [ThetaType] -> TcM [DerivSpec ThetaType]
    iterate_deriv n current_solns
      | n > 20  -- Looks as if we are in an infinite loop
                -- This can happen if we have -XUndecidableInstances
                -- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop"
                 (vcat (map pprDerivSpec infer_specs) $$ ppr current_solns)
      | otherwise
      = do {      -- Extend the inst info from the explicit instance decls
                  -- with the current set of solutions, and simplify each RHS
             inst_specs <- zipWithM newDerivClsInst current_solns infer_specs
           ; new_solns <- checkNoErrs $
                          extendLocalInstEnv inst_specs $
                          mapM gen_soln infer_specs

           ; if (current_solns `eqSolution` new_solns) then
                return [ spec { ds_theta = soln }
                       | (spec, soln) <- zip infer_specs current_solns ]
             else
                iterate_deriv (n+1) new_solns }

    eqSolution = eqListBy (eqListBy eqType)

    ------------------------------------------------------------------
    gen_soln :: DerivSpec ThetaOrigin -> TcM ThetaType
    gen_soln (DS { ds_loc = loc, ds_tvs = tyvars
                 , ds_cls = clas, ds_tys = inst_tys, ds_theta = deriv_rhs })
      = setSrcSpan loc  $
        addErrCtxt (derivInstCtxt the_pred) $
        do { theta <- simplifyDeriv the_pred tyvars deriv_rhs
                -- checkValidInstance tyvars theta clas inst_tys
                -- Not necessary; see Note [Exotic derived instance contexts]

           ; traceTc "TcDeriv" (ppr deriv_rhs $$ ppr theta)
                -- Claim: the result instance declaration is guaranteed valid
                -- Hence no need to call:
                --   checkValidInstance tyvars theta clas inst_tys
           ; return (sortBy cmpType theta) }    -- Canonicalise before returning the solution
      where
        the_pred = mkClassPred clas inst_tys

------------------------------------------------------------------
newDerivClsInst :: ThetaType -> DerivSpec theta -> TcM ClsInst
newDerivClsInst theta (DS { ds_name = dfun_name, ds_overlap = overlap_mode
                          , ds_tvs = tvs, ds_cls = clas, ds_tys = tys })
  = newClsInst overlap_mode dfun_name tvs theta clas tys

extendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
-- Add new locally-defined instances; don't bother to check
-- for functional dependency errors -- that'll happen in TcInstDcls
extendLocalInstEnv dfuns thing_inside
 = do { env <- getGblEnv
      ; let  inst_env' = extendInstEnvList (tcg_inst_env env) dfuns
             env'      = env { tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

{-
***********************************************************************************
*                                                                                 *
*            Simplify derived constraints
*                                                                                 *
***********************************************************************************
-}

simplifyDeriv :: PredType
              -> [TyVar]
              -> ThetaOrigin      -- Wanted
              -> TcM ThetaType  -- Needed
-- Given  instance (wanted) => C inst_ty
-- Simplify 'wanted' as much as possibles
-- Fail if not possible
simplifyDeriv pred tvs theta
  = do { (skol_subst, tvs_skols) <- tcInstSkolTyVars tvs -- Skolemize
                -- The constraint solving machinery
                -- expects *TcTyVars* not TyVars.
                -- We use *non-overlappable* (vanilla) skolems
                -- See Note [Overlap and deriving]

       ; let skol_set  = mkVarSet tvs_skols
             skol_info = DerivSkol pred
             doc = text "deriving" <+> parens (ppr pred)
             mk_ct (PredOrigin t o t_or_k)
                 = newWanted o (Just t_or_k) (substTy skol_subst t)

       ; (wanted, tclvl) <- pushTcLevelM (mapM mk_ct theta)

       ; traceTc "simplifyDeriv" $
         vcat [ pprTvBndrs tvs $$ ppr theta $$ ppr wanted, doc ]
       ; residual_wanted <- simplifyWantedsTcM wanted
            -- Result is zonked

       ; let residual_simple = wc_simple residual_wanted
             (good, bad) = partitionBagWith get_good residual_simple
             unsolved    = residual_wanted { wc_simple = bad }

                         -- See Note [Exotic derived instance contexts]

             get_good :: Ct -> Either PredType Ct
             get_good ct | validDerivPred skol_set p
                         , isWantedCt ct
                         = Left p
                          -- NB: residual_wanted may contain unsolved
                          -- Derived and we stick them into the bad set
                          -- so that reportUnsolved may decide what to do with them
                         | otherwise
                         = Right ct
                           where p = ctPred ct

       ; traceTc "simplifyDeriv 2" $
         vcat [ ppr tvs_skols, ppr residual_simple, ppr good, ppr bad ]

       -- If we are deferring type errors, simply ignore any insoluble
       -- constraints.  They'll come up again when we typecheck the
       -- generated instance declaration
       ; defer <- goptM Opt_DeferTypeErrors
       ; (implic, _) <- buildImplicationFor tclvl skol_info tvs_skols [] unsolved
                   -- The buildImplicationFor is just to bind the skolems,
                   -- in case they are mentioned in error messages
                   -- See Trac #11347
       ; unless defer (reportAllUnsolved (mkImplicWC implic))


       ; let min_theta  = mkMinimalBySCs (bagToList good)
             subst_skol = zipTvSubst tvs_skols $ mkTyVarTys tvs
                          -- The reverse substitution (sigh)
       ; return (substTheta subst_skol min_theta) }

{-
Note [Overlap and deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider some overlapping instances:
  data Show a => Show [a] where ..
  data Show [Char] where ...

Now a data type with deriving:
  data T a = MkT [a] deriving( Show )

We want to get the derived instance
  instance Show [a] => Show (T a) where...
and NOT
  instance Show a => Show (T a) where...
so that the (Show (T Char)) instance does the Right Thing

It's very like the situation when we're inferring the type
of a function
   f x = show [x]
and we want to infer
   f :: Show [a] => a -> String

BOTTOM LINE: use vanilla, non-overlappable skolems when inferring
             the context for the derived instance.
             Hence tcInstSkolTyVars not tcInstSuperSkolTyVars

Note [Exotic derived instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a 'derived' instance declaration, we *infer* the context.  It's a
bit unclear what rules we should apply for this; the Haskell report is
silent.  Obviously, constraints like (Eq a) are fine, but what about
        data T f a = MkT (f a) deriving( Eq )
where we'd get an Eq (f a) constraint.  That's probably fine too.

One could go further: consider
        data T a b c = MkT (Foo a b c) deriving( Eq )
        instance (C Int a, Eq b, Eq c) => Eq (Foo a b c)

Notice that this instance (just) satisfies the Paterson termination
conditions.  Then we *could* derive an instance decl like this:

        instance (C Int a, Eq b, Eq c) => Eq (T a b c)
even though there is no instance for (C Int a), because there just
*might* be an instance for, say, (C Int Bool) at a site where we
need the equality instance for T's.

However, this seems pretty exotic, and it's quite tricky to allow
this, and yet give sensible error messages in the (much more common)
case where we really want that instance decl for C.

So for now we simply require that the derived instance context
should have only type-variable constraints.

Here is another example:
        data Fix f = In (f (Fix f)) deriving( Eq )
Here, if we are prepared to allow -XUndecidableInstances we
could derive the instance
        instance Eq (f (Fix f)) => Eq (Fix f)
but this is so delicate that I don't think it should happen inside
'deriving'. If you want this, write it yourself!

NB: if you want to lift this condition, make sure you still meet the
termination conditions!  If not, the deriving mechanism generates
larger and larger constraints.  Example:
  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate
  instance (Show (Succ a), Show a) => Show (Seq a)
and then
  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)
and so on.  Instead we want to complain of no instance for (Show (Succ a)).

The bottom line
~~~~~~~~~~~~~~~
Allow constraints which consist only of type variables, with no repeats.


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
                 , ds_theta = theta, ds_newtype = is_newtype, ds_tys = tys
                 , ds_name = dfun_name, ds_cls = clas, ds_loc = loc })
  | Just rhs_ty <- is_newtype   -- See Note [Bindings for Generalised Newtype Deriving]
  = do { inst_spec <- newDerivClsInst theta spec
       ; traceTc "genInst/is_newtype" (vcat [ppr loc, ppr clas, ppr tvs, ppr tys, ppr rhs_ty])
       ; return ( InstInfo
                    { iSpec   = inst_spec
                    , iBinds  = InstBindings
                        { ib_binds      = gen_Newtype_binds loc clas tvs tys rhs_ty
                        , ib_tyvars     = map Var.varName tvs   -- Scope over bindings
                        , ib_pragmas    = []
                        , ib_extensions = [ LangExt.ImpredicativeTypes
                                          , LangExt.RankNTypes ]
                        , ib_derived    = True } }
                , emptyBag
                , Just $ getName $ head $ tyConDataCons rep_tycon ) }
              -- See Note [Newtype deriving and unused constructors]

  | otherwise
  = do { (meth_binds, deriv_stuff) <- genDerivStuff loc clas
                                        dfun_name rep_tycon
                                        tys tvs
       ; inst_spec <- newDerivClsInst theta spec
       ; traceTc "newder" (ppr inst_spec)
       ; let inst_info = InstInfo { iSpec   = inst_spec
                                  , iBinds  = InstBindings
                                                { ib_binds = meth_binds
                                                , ib_tyvars = map Var.varName tvs
                                                , ib_pragmas = []
                                                , ib_extensions = []
                                                , ib_derived = True } }
       ; return ( inst_info, deriv_stuff, Nothing ) }

-- Generate the bindings needed for a derived class that isn't handled by
-- -XGeneralizedNewtypeDeriving.
genDerivStuff :: SrcSpan -> Class -> Name -> TyCon -> [Type] -> [TyVar]
              -> TcM (LHsBinds RdrName, BagDerivStuff)
genDerivStuff loc clas dfun_name tycon inst_tys tyvars
  -- Special case for DeriveGeneric
  | let ck = classKey clas
  , ck `elem` [genClassKey, gen1ClassKey]
  = let gk = if ck == genClassKey then Gen0 else Gen1
        -- TODO NSF: correctly identify when we're building Both instead of One
    in do
      (binds, faminst) <- gen_Generic_binds gk tycon (nameModule dfun_name)
      return (binds, unitBag (DerivFamInst faminst))

  -- Not deriving Generic(1), so we first check if the compiler has built-in
  -- support for deriving the class in question.
  | otherwise
  = do { dflags <- getDynFlags
       ; fix_env <- getDataConFixityFun tycon
       ; case hasBuiltinDeriving dflags fix_env clas of
              Just gen_fn -> return (gen_fn loc tycon)
              Nothing -> genDerivAnyClass dflags }

  where
    genDerivAnyClass :: DynFlags -> TcM (LHsBinds RdrName, BagDerivStuff)
    genDerivAnyClass dflags =
      do { -- If there isn't compiler support for deriving the class, our last
           -- resort is -XDeriveAnyClass (since -XGeneralizedNewtypeDeriving
           -- fell through).
          let mini_env   = mkVarEnv (classTyVars clas `zip` inst_tys)
              mini_subst = mkTvSubst (mkInScopeSet (mkVarSet tyvars)) mini_env

         ; tyfam_insts <-
             ASSERT2( isNothing (canDeriveAnyClass dflags tycon clas)
                    , ppr "genDerivStuff: bad derived class" <+> ppr clas )
             mapM (tcATDefault False loc mini_subst emptyNameSet)
                  (classATItems clas)
         ; return ( emptyBag -- No method bindings are needed...
                  , listToBag (map DerivFamInst (concat tyfam_insts))
                  -- ...but we may need to generate binding for associated type
                  -- family default instances.
                  -- See Note [DeriveAnyClass and default family instances]
                  ) }

getDataConFixityFun :: TyCon -> TcM (Name -> Fixity)
-- If the TyCon is locally defined, we want the local fixity env;
-- but if it is imported (which happens for standalone deriving)
-- we need to get the fixity env from the interface file
-- c.f. RnEnv.lookupFixity, and Trac #9830
getDataConFixityFun tc
  = do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name
         then do { fix_env <- getFixityEnv
                 ; return (lookupFixity fix_env) }
         else do { iface <- loadInterfaceForName doc name
                            -- Should already be loaded!
                 ; return (mi_fix iface . nameOccName) } }
  where
    name = tyConName tc
    doc = text "Data con fixities for" <+> ppr name

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

See the paper "Safe zero-cost coercions for Hsakell".

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

************************************************************************
*                                                                      *
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
*                                                                      *
************************************************************************
-}

derivingNullaryErr :: MsgDoc
derivingNullaryErr = text "Cannot derive instances for nullary classes"

derivingKindErr :: TyCon -> Class -> [Type] -> Kind -> MsgDoc
derivingKindErr tc cls cls_tys cls_kind
  = hang (text "Cannot derive well-kinded instance of form"
                <+> quotes (pprClassPred cls cls_tys <+> parens (ppr tc <+> text "...")))
       2 (text "Class" <+> quotes (ppr cls)
            <+> text "expects an argument of kind" <+> quotes (pprKind cls_kind))

derivingEtaErr :: Class -> [Type] -> Type -> MsgDoc
derivingEtaErr cls cls_tys inst_ty
  = sep [text "Cannot eta-reduce to an instance of form",
         nest 2 (text "instance (...) =>"
                <+> pprClassPred cls (cls_tys ++ [inst_ty]))]

derivingThingErr :: Bool -> Class -> [Type] -> Type -> MsgDoc -> MsgDoc
derivingThingErr newtype_deriving clas tys ty why
  = sep [(hang (text "Can't make a derived instance of")
             2 (quotes (ppr pred))
          $$ nest 2 extra) <> colon,
         nest 2 why]
  where
    extra | newtype_deriving = text "(even with cunning GeneralizedNewtypeDeriving)"
          | otherwise        = Outputable.empty
    pred = mkClassPred clas (tys ++ [ty])

derivingHiddenErr :: TyCon -> SDoc
derivingHiddenErr tc
  = hang (text "The data constructors of" <+> quotes (ppr tc) <+> ptext (sLit "are not all in scope"))
       2 (text "so you cannot derive an instance for it")

standaloneCtxt :: LHsSigType Name -> SDoc
standaloneCtxt ty = hang (text "In the stand-alone deriving instance for")
                       2 (quotes (ppr ty))

derivInstCtxt :: PredType -> MsgDoc
derivInstCtxt pred
  = text "When deriving the instance for" <+> parens (ppr pred)
