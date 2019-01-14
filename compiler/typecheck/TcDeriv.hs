{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Handles @deriving@ clauses on @data@ declarations.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module TcDeriv ( tcDeriving, DerivInfo(..), mkDerivInfos ) where

#include "HsVersions.h"

import GhcPrelude

import HsSyn
import DynFlags

import TcRnMonad
import FamInst
import TcDerivInfer
import TcDerivUtils
import TcValidity( allDistinctTyVars )
import TcClassDcl( instDeclCtxt3, tcATDefault, tcMkDeclCtxt )
import TcEnv
import TcGenDeriv                       -- Deriv stuff
import TcValidity( checkValidInstHead )
import InstEnv
import Inst
import FamInstEnv
import TcHsType
import TyCoRep

import RnNames( extendGlobalRdrEnvRn )
import RnBinds
import RnEnv
import RnUtils    ( bindLocalNamesFV )
import RnSource   ( addTcgDUs )
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
import SrcLoc
import Util
import Outputable
import FastString
import Bag
import Pair
import FV (fvVarList, unionFV, mkFVs)
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
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

data EarlyDerivSpec = InferTheta (DerivSpec [ThetaOrigin])
                    | GivenTheta (DerivSpec ThetaType)
        -- InferTheta ds => the context for the instance should be inferred
        --      In this case ds_theta is the list of all the sets of
        --      constraints needed, such as (Eq [a], Eq a), together with a
        --      suitable CtLoc to get good error messages.
        --      The inference process is to reduce this to a
        --      simpler form (e.g. Eq a)
        --
        -- GivenTheta ds => the exact context for the instance is supplied
        --                  by the programmer; it is ds_theta
        -- See Note [Inferring the instance context] in TcDerivInfer

earlyDSLoc :: EarlyDerivSpec -> SrcSpan
earlyDSLoc (InferTheta spec) = ds_loc spec
earlyDSLoc (GivenTheta spec) = ds_loc spec

splitEarlyDerivSpec :: [EarlyDerivSpec]
                    -> ([DerivSpec [ThetaOrigin]], [DerivSpec ThetaType])
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
                           , di_clauses :: [LHsDerivingClause GhcRn]
                           , di_ctxt    :: SDoc -- ^ error context
                           }

-- | Extract `deriving` clauses of proper data type (skips data families)
mkDerivInfos :: [LTyClDecl GhcRn] -> TcM [DerivInfo]
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
            -> [LDerivDecl GhcRn] -- All stand-alone deriving declarations
            -> TcM (TcGblEnv, Bag (InstInfo GhcRn), HsValBinds GhcRn)
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
        ; insts2 <- mapM genInst infer_specs

        ; dflags <- getDynFlags

        ; let (_, deriv_stuff, fvs) = unzip3 (insts1 ++ insts2)
        ; loc <- getSrcSpanM
        ; let (binds, famInsts) = genAuxBinds dflags loc
                                    (unionManyBags deriv_stuff)

        ; let mk_inst_infos1 = map fstOf3 insts1
        ; inst_infos1 <- apply_inst_infos mk_inst_infos1 given_specs

          -- We must put all the derived type family instances (from both
          -- infer_specs and given_specs) in the local instance environment
          -- before proceeding, or else simplifyInstanceContexts might
          -- get stuck if it has to reason about any of those family instances.
          -- See Note [Staging of tcDeriving]
        ; tcExtendLocalFamInstEnv (bagToList famInsts) $
          -- NB: only call tcExtendLocalFamInstEnv once, as it performs
          -- validity checking for all of the family instances you give it.
          -- If the family instances have errors, calling it twice will result
          -- in duplicate error messages!

     do {
        -- the stand-alone derived instances (@inst_infos1@) are used when
        -- inferring the contexts for "deriving" clauses' instances
        -- (@infer_specs@)
        ; final_specs <- extendLocalInstEnv (map iSpec inst_infos1) $
                         simplifyInstanceContexts infer_specs

        ; let mk_inst_infos2 = map fstOf3 insts2
        ; inst_infos2 <- apply_inst_infos mk_inst_infos2 final_specs
        ; let inst_infos = inst_infos1 ++ inst_infos2

        ; (inst_info, rn_binds, rn_dus) <-
            renameDeriv is_boot inst_infos binds

        ; unless (isEmptyBag inst_info) $
             liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances"
                        (ddump_deriving inst_info rn_binds famInsts))

        ; gbl_env <- tcExtendLocalInstEnv (map iSpec (bagToList inst_info))
                                          getGblEnv
        ; let all_dus = rn_dus `plusDU` usesOnly (NameSet.mkFVs $ concat fvs)
        ; return (addTcgDUs gbl_env all_dus, inst_info, rn_binds) } }
  where
    ddump_deriving :: Bag (InstInfo GhcRn) -> HsValBinds GhcRn
                   -> Bag FamInst             -- ^ Rep type family instances
                   -> SDoc
    ddump_deriving inst_infos extra_binds repFamInsts
      =    hang (text "Derived class instances:")
              2 (vcat (map (\i -> pprInstInfoDetails i $$ text "") (bagToList inst_infos))
                 $$ ppr extra_binds)
        $$ hangP "Derived type family instances:"
             (vcat (map pprRepTy (bagToList repFamInsts)))

    hangP s x = text "" $$ hang (ptext (sLit s)) 2 x

    -- Apply the suspended computations given by genInst calls.
    -- See Note [Staging of tcDeriving]
    apply_inst_infos :: [ThetaType -> TcM (InstInfo GhcPs)]
                     -> [DerivSpec ThetaType] -> TcM [InstInfo GhcPs]
    apply_inst_infos = zipWithM (\f ds -> f (ds_theta ds))

-- Prints the representable type family instance
pprRepTy :: FamInst -> SDoc
pprRepTy fi@(FamInst { fi_tys = lhs })
  = text "type" <+> ppr (mkTyConApp (famInstTyCon fi) lhs) <+>
      equals <+> ppr rhs
  where rhs = famInstRHS fi

renameDeriv :: Bool
            -> [InstInfo GhcPs]
            -> Bag (LHsBind GhcPs, LSig GhcPs)
            -> TcM (Bag (InstInfo GhcRn), HsValBinds GhcRn, DefUses)
renameDeriv is_boot inst_infos bagBinds
  | is_boot     -- If we are compiling a hs-boot file, don't generate any derived bindings
                -- The inst-info bindings will all be empty, but it's easier to
                -- just use rn_inst_info to change the type appropriately
  = do  { (rn_inst_infos, fvs) <- mapAndUnzipM rn_inst_info inst_infos
        ; return ( listToBag rn_inst_infos
                 , emptyValBindsOut, usesOnly (plusFVs fvs)) }

  | otherwise
  = discardWarnings $
    -- Discard warnings about unused bindings etc
    setXOptM LangExt.EmptyCase $
    -- Derived decls (for empty types) can have
    --    case x of {}
    setXOptM LangExt.ScopedTypeVariables $
    setXOptM LangExt.KindSignatures $
    -- Derived decls (for newtype-deriving) can use ScopedTypeVariables &
    -- KindSignatures
    setXOptM LangExt.TypeApplications $
    -- GND/DerivingVia uses TypeApplications in generated code
    -- (See Note [Newtype-deriving instances] in TcGenDeriv)
    unsetXOptM LangExt.RebindableSyntax $
    -- See Note [Avoid RebindableSyntax when deriving]
    do  {
        -- Bring the extra deriving stuff into scope
        -- before renaming the instances themselves
        ; traceTc "rnd" (vcat (map (\i -> pprInstInfoDetails i $$ text "") inst_infos))
        ; (aux_binds, aux_sigs) <- mapAndUnzipBagM return bagBinds
        ; let aux_val_binds = ValBinds noExt aux_binds (bagToList aux_sigs)
        ; rn_aux_lhs <- rnTopBindsLHS emptyFsEnv aux_val_binds
        ; let bndrs = collectHsValBinders rn_aux_lhs
        ; envs <- extendGlobalRdrEnvRn (map avail bndrs) emptyFsEnv ;
        ; setEnvs envs $
    do  { (rn_aux, dus_aux) <- rnValBindsRHS (TopSigCtxt (mkNameSet bndrs)) rn_aux_lhs
        ; (rn_inst_infos, fvs_insts) <- mapAndUnzipM rn_inst_info inst_infos
        ; return (listToBag rn_inst_infos, rn_aux,
                  dus_aux `plusDU` usesOnly (plusFVs fvs_insts)) } }

  where
    rn_inst_info :: InstInfo GhcPs -> TcM (InstInfo GhcRn, FreeVars)
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
"Defined but not used: data constructor MkP". Yet the newtype deriving
code does not explicitly mention MkP, but it should behave as if you
had written
  instance Monad P where
     return x = MkP (return x)
     ...etc...

So we want to signal a user of the data constructor 'MkP'.
This is the reason behind the [Name] part of the return type
of genInst.

Note [Staging of tcDeriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here's a tricky corner case for deriving (adapted from Trac #2721):

    class C a where
      type T a
      foo :: a -> T a

    instance C Int where
      type T Int = Int
      foo = id

    newtype N = N Int deriving C

This will produce an instance something like this:

    instance C N where
      type T N = T Int
      foo = coerce (foo :: Int -> T Int) :: N -> T N

We must be careful in order to typecheck this code. When determining the
context for the instance (in simplifyInstanceContexts), we need to determine
that T N and T Int have the same representation, but to do that, the T N
instance must be in the local family instance environment. Otherwise, GHC
would be unable to conclude that T Int is representationally equivalent to
T Int, and simplifyInstanceContexts would get stuck.

Previously, tcDeriving would defer adding any derived type family instances to
the instance environment until the very end, which meant that
simplifyInstanceContexts would get called without all the type family instances
it needed in the environment in order to properly simplify instance like
the C N instance above.

To avoid this scenario, we carefully structure the order of events in
tcDeriving. We first call genInst on the standalone derived instance specs and
the instance specs obtained from deriving clauses. Note that the return type of
genInst is a triple:

    TcM (ThetaType -> TcM (InstInfo RdrName), BagDerivStuff, Maybe Name)

The type family instances are in the BagDerivStuff. The first field of the
triple is a suspended computation which, given an instance context, produces
the rest of the instance. The fact that it is suspended is important, because
right now, we don't have ThetaTypes for the instances that use deriving clauses
(only the standalone-derived ones).

Now we can can collect the type family instances and extend the local instance
environment. At this point, it is safe to run simplifyInstanceContexts on the
deriving-clause instance specs, which gives us the ThetaTypes for the
deriving-clause instances. Now we can feed all the ThetaTypes to the
suspended computations and obtain our InstInfos, at which point
tcDeriving is done.

An alternative design would be to split up genInst so that the
family instances are generated separately from the InstInfos. But this would
require carving up a lot of the GHC deriving internals to accommodate the
change. On the other hand, we can keep all of the InstInfo and type family
instance logic together in genInst simply by converting genInst to
continuation-returning style, so we opt for that route.

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

Note [Avoid RebindableSyntax when deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The RebindableSyntax extension interacts awkwardly with the derivation of
any stock class whose methods require the use of string literals. The Show
class is a simple example (see Trac #12688):

  {-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
  newtype Text = Text String
  fromString :: String -> Text
  fromString = Text

  data Foo = Foo deriving Show

This will generate code to the effect of:

  instance Show Foo where
    showsPrec _ Foo = showString "Foo"

But because RebindableSyntax and OverloadedStrings are enabled, the "Foo"
string literal is now of type Text, not String, which showString doesn't
accept! This causes the generated Show instance to fail to typecheck.

To avoid this kind of scenario, we simply turn off RebindableSyntax entirely
in derived code.

************************************************************************
*                                                                      *
                From HsSyn to DerivSpec
*                                                                      *
************************************************************************

@makeDerivSpecs@ fishes around to find the info about needed derived instances.
-}

makeDerivSpecs :: Bool
               -> [DerivInfo]
               -> [LDerivDecl GhcRn]
               -> TcM [EarlyDerivSpec]
makeDerivSpecs is_boot deriv_infos deriv_decls
  = do  { -- We carefully set up uses of recoverM to minimize error message
          -- cascades. See Note [Flattening deriving clauses].
        ; eqns1 <- sequenceA
                     [ recoverM (pure Nothing)
                                (deriveClause rep_tc (fmap unLoc dcs)
                                                      pred err_ctxt)
                     | DerivInfo { di_rep_tc = rep_tc, di_clauses = clauses
                                 , di_ctxt = err_ctxt } <- deriv_infos
                     , L _ (HsDerivingClause { deriv_clause_strategy = dcs
                                             , deriv_clause_tys = L _ preds })
                         <- clauses
                     , pred <- preds
                     ]
        ; eqns2 <- mapM (recoverM (pure Nothing) . deriveStandalone) deriv_decls
        ; let eqns = catMaybes (eqns1 ++ eqns2)

        ; if is_boot then   -- No 'deriving' at all in hs-boot files
              do { unless (null eqns) (add_deriv_err (head eqns))
                 ; return [] }
          else return eqns }
  where
    add_deriv_err eqn
       = setSrcSpan (earlyDSLoc eqn) $
         addErr (hang (text "Deriving not permitted in hs-boot file")
                    2 (text "Use an instance declaration instead"))

{-
Note [Flattening deriving clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider what happens if you run this program (from Trac #10684) without
DeriveGeneric enabled:

    data A = A deriving (Show, Generic)
    data B = B A deriving (Show)

Naturally, you'd expect GHC to give an error to the effect of:

    Can't make a derived instance of `Generic A':
      You need -XDeriveGeneric to derive an instance for this class

And *only* that error, since the other two derived Show instances appear to be
independent of this derived Generic instance. Yet GHC also used to give this
additional error on the program above:

    No instance for (Show A)
      arising from the 'deriving' clause of a data type declaration
    When deriving the instance for (Show B)

This was happening because when GHC encountered any error within a single
data type's set of deriving clauses, it would call recoverM and move on
to the next data type's deriving clauses. One unfortunate consequence of
this design is that if A's derived Generic instance failed, so its derived
Show instance would be skipped entirely, leading to the "No instance for
(Show A)" error cascade.

The solution to this problem is to "flatten" the set of classes that are
derived for a particular data type via deriving clauses. That is, if
you have:

    newtype C = C D
      deriving (E, F, G)
      deriving anyclass (H, I, J)
      deriving newtype  (K, L, M)

Then instead of processing instances E through M under the scope of a single
recoverM, we flatten these deriving clauses into the list:

    [ E (Nothing)
    , F (Nothing)
    , G (Nothing)
    , H (Just anyclass)
    , I (Just anyclass)
    , J (Just anyclass)
    , K (Just newtype)
    , L (Just newtype)
    , M (Just newtype) ]

And then process each class individually, under its own recoverM scope. That
way, failure to derive one class doesn't cancel out other classes in the
same set of clause-derived classes.
-}

------------------------------------------------------------------
-- | Process a single class in a `deriving` clause.
deriveClause :: TyCon -> Maybe (DerivStrategy GhcRn)
             -> LHsSigType GhcRn -> SDoc
             -> TcM (Maybe EarlyDerivSpec)
deriveClause rep_tc mb_strat pred err_ctxt
  = addErrCtxt err_ctxt $
    deriveTyData tvs tc tys mb_strat pred
  where
    tvs = tyConTyVars rep_tc
    (tc, tys) = case tyConFamInstSig_maybe rep_tc of
                        -- data family:
                  Just (fam_tc, pats, _) -> (fam_tc, pats)
      -- NB: deriveTyData wants the *user-specified*
      -- name. See Note [Why we don't pass rep_tc into deriveTyData]

                  _ -> (rep_tc, mkTyVarTys tvs)     -- datatype

------------------------------------------------------------------
deriveStandalone :: LDerivDecl GhcRn -> TcM (Maybe EarlyDerivSpec)
-- Process a single standalone deriving declaration
--  e.g.   deriving instance Show a => Show (T a)
-- Rather like tcLocalInstDecl
--
-- This returns a Maybe because the user might try to derive Typeable, which is
-- a no-op nowadays.
deriveStandalone (L loc (DerivDecl _ deriv_ty mbl_deriv_strat overlap_mode))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do { traceTc "Standalone deriving decl for" (ppr deriv_ty)
       ; let mb_deriv_strat = fmap unLoc mbl_deriv_strat
             ctxt           = TcType.InstDeclCtxt True
       ; traceTc "Deriving strategy (standalone deriving)" $
           vcat [ppr mb_deriv_strat, ppr deriv_ty]
       ; (mb_deriv_strat', tvs', (deriv_ctxt', cls, inst_tys'))
           <- tcDerivStrategy mb_deriv_strat $ do
                (tvs, deriv_ctxt, cls, inst_tys)
                  <- tcStandaloneDerivInstType ctxt deriv_ty
                pure (tvs, (deriv_ctxt, cls, inst_tys))
       ; checkTc (not (null inst_tys')) derivingNullaryErr
       ; let inst_ty' = last inst_tys'
         -- See Note [Unify kinds in deriving]
       ; (tvs, deriv_ctxt, inst_tys) <-
           case mb_deriv_strat' of
             -- Perform an additional unification with the kind of the `via`
             -- type and the result of the previous kind unification.
             Just (ViaStrategy via_ty) -> do
               let via_kind     = tcTypeKind via_ty
                   inst_ty_kind = tcTypeKind inst_ty'
                   mb_match     = tcUnifyTy inst_ty_kind via_kind

               checkTc (isJust mb_match)
                       (derivingViaKindErr cls inst_ty_kind
                                           via_ty via_kind)

               let Just kind_subst = mb_match
                   ki_subst_range  = getTCvSubstRangeFVs kind_subst
                   -- See Note [Unification of two kind variables in deriving]
                   unmapped_tkvs = filter (\v -> v `notElemTCvSubst` kind_subst
                                        && not (v `elemVarSet` ki_subst_range))
                                          tvs'
                   (subst, _)    = substTyVarBndrs kind_subst unmapped_tkvs
                   (final_deriv_ctxt, final_deriv_ctxt_tys)
                     = case deriv_ctxt' of
                         InferContext wc -> (InferContext wc, [])
                         SupplyContext theta ->
                           let final_theta = substTheta subst theta
                           in (SupplyContext final_theta, final_theta)
                   final_inst_tys   = substTys subst inst_tys'
                   final_tvs        = tyCoVarsOfTypesWellScoped $
                                      final_deriv_ctxt_tys ++ final_inst_tys
               pure (final_tvs, final_deriv_ctxt, final_inst_tys)

             _ -> pure (tvs', deriv_ctxt', inst_tys')
       ; let cls_tys = take (length inst_tys - 1) inst_tys
             inst_ty = last inst_tys
       ; traceTc "Standalone deriving;" $ vcat
              [ text "tvs:" <+> ppr tvs
              , text "mb_deriv_strat:" <+> ppr mb_deriv_strat'
              , text "deriv_ctxt:" <+> ppr deriv_ctxt
              , text "cls:" <+> ppr cls
              , text "tys:" <+> ppr inst_tys ]
                -- C.f. TcInstDcls.tcLocalInstDecl1
       ; traceTc "Standalone deriving:" $ vcat
              [ text "class:" <+> ppr cls
              , text "class types:" <+> ppr cls_tys
              , text "type:" <+> ppr inst_ty ]

       ; let bale_out msg = failWithTc (derivingThingErr False cls cls_tys
                              inst_ty mb_deriv_strat' msg)

       ; case tcSplitTyConApp_maybe inst_ty of
           Just (tc, tc_args)
              | className cls == typeableClassName
              -> do warnUselessTypeable
                    return Nothing

              | otherwise
              -> Just <$> mkEqnHelp (fmap unLoc overlap_mode)
                                    tvs cls cls_tys tc tc_args
                                    deriv_ctxt mb_deriv_strat'

           _  -> -- Complain about functions, primitive types, etc,
                 bale_out $
                 text "The last argument of the instance must be a data or newtype application"
        }
deriveStandalone (L _ (XDerivDecl _)) = panic "deriveStandalone"

-- Typecheck the type in a standalone deriving declaration.
--
-- This may appear dense, but it's mostly huffing and puffing to recognize
-- the special case of a type with an extra-constraints wildcard context, e.g.,
--
--   deriving instance _ => Eq (Foo a)
--
-- If there is such a wildcard, we typecheck this as if we had written
-- @deriving instance Eq (Foo a)@, and return @'InferContext' ('Just' loc)@,
-- as the 'DerivContext', where loc is the location of the wildcard used for
-- error reporting. This indicates that we should infer the context as if we
-- were deriving Eq via a deriving clause
-- (see Note [Inferring the instance context] in TcDerivInfer).
--
-- If there is no wildcard, then proceed as normal, and instead return
-- @'SupplyContext' theta@, where theta is the typechecked context.
--
-- Note that this will never return @'InferContext' 'Nothing'@, as that can
-- only happen with @deriving@ clauses.
tcStandaloneDerivInstType
  :: UserTypeCtxt -> LHsSigWcType GhcRn
  -> TcM ([TyVar], DerivContext, Class, [Type])
tcStandaloneDerivInstType ctxt
    (HsWC { hswc_body = deriv_ty@(HsIB { hsib_ext = vars
                                       , hsib_body   = deriv_ty_body })})
  | (tvs, theta, rho) <- splitLHsSigmaTy deriv_ty_body
  , L _ [wc_pred] <- theta
  , L wc_span (HsWildCardTy _) <- ignoreParens wc_pred
  = do dfun_ty <- tcHsClsInstType ctxt $
                  HsIB { hsib_ext = vars
                       , hsib_body
                           = L (getLoc deriv_ty_body) $
                             HsForAllTy { hst_bndrs = tvs
                                        , hst_xforall = noExt
                                        , hst_body  = rho }}
       let (tvs, _theta, cls, inst_tys) = tcSplitDFunTy dfun_ty
       pure (tvs, InferContext (Just wc_span), cls, inst_tys)
  | otherwise
  = do dfun_ty <- tcHsClsInstType ctxt deriv_ty
       let (tvs, theta, cls, inst_tys) = tcSplitDFunTy dfun_ty
       pure (tvs, SupplyContext theta, cls, inst_tys)

tcStandaloneDerivInstType _ (HsWC _ (XHsImplicitBndrs _))
  = panic "tcStandaloneDerivInstType"
tcStandaloneDerivInstType _ (XHsWildCardBndrs _)
  = panic "tcStandaloneDerivInstType"

warnUselessTypeable :: TcM ()
warnUselessTypeable
  = do { warn <- woptM Opt_WarnDerivingTypeable
       ; when warn $ addWarnTc (Reason Opt_WarnDerivingTypeable)
                   $ text "Deriving" <+> quotes (ppr typeableClassName) <+>
                     text "has no effect: all types now auto-derive Typeable" }

------------------------------------------------------------------
deriveTyData :: [TyVar] -> TyCon -> [Type]   -- LHS of data or data instance
                    -- Can be a data instance, hence [Type] args
                    -- and in that case the TyCon is the /family/ tycon
             -> Maybe (DerivStrategy GhcRn)  -- The optional deriving strategy
             -> LHsSigType GhcRn             -- The deriving predicate
             -> TcM (Maybe EarlyDerivSpec)
-- The deriving clause of a data or newtype declaration
-- I.e. not standalone deriving
--
-- This returns a Maybe because the user might try to derive Typeable, which is
-- a no-op nowadays.
deriveTyData tvs tc tc_args mb_deriv_strat deriv_pred
  = setSrcSpan (getLoc (hsSigType deriv_pred)) $
    -- Use loc of the 'deriving' item
    do  { (mb_deriv_strat', deriv_tvs, (cls, cls_tys, cls_arg_kinds))
                <- tcExtendTyVarEnv tvs $
                -- Deriving preds may (now) mention
                -- the type variables for the type constructor, hence tcExtendTyVarenv
                -- The "deriv_pred" is a LHsType to take account of the fact that for
                -- newtype deriving we allow deriving (forall a. C [a]).

                -- Typeable is special, because Typeable :: forall k. k -> Constraint
                -- so the argument kind 'k' is not decomposable by splitKindFunTys
                -- as is the case for all other derivable type classes
                     tcDerivStrategy mb_deriv_strat $
                     tcHsDeriv deriv_pred

        ; when (cls_arg_kinds `lengthIsNot` 1) $
            failWithTc (nonUnaryErr deriv_pred)
        ; let [cls_arg_kind] = cls_arg_kinds
        ; if className cls == typeableClassName
          then do warnUselessTypeable
                  return Nothing
          else

     do {  -- Given data T a b c = ... deriving( C d ),
           -- we want to drop type variables from T so that (C d (T a)) is well-kinded
          let (arg_kinds, _)  = splitFunTys cls_arg_kind
              n_args_to_drop  = length arg_kinds
              n_args_to_keep  = length tc_args - n_args_to_drop
                                -- See Note [tc_args and tycon arity]
              (tc_args_to_keep, args_to_drop)
                              = splitAt n_args_to_keep tc_args
              inst_ty_kind    = tcTypeKind (mkTyConApp tc tc_args_to_keep)

              -- Match up the kinds, and apply the resulting kind substitution
              -- to the types.  See Note [Unify kinds in deriving]
              -- We are assuming the tycon tyvars and the class tyvars are distinct
              mb_match        = tcUnifyTy inst_ty_kind cls_arg_kind
              enough_args     = n_args_to_keep >= 0

        -- Check that the result really is well-kinded
        ; checkTc (enough_args && isJust mb_match)
                  (derivingKindErr tc cls cls_tys cls_arg_kind enough_args)

        ; let propagate_subst kind_subst tkvs' cls_tys' tc_args'
                = (final_tkvs, final_cls_tys, final_tc_args)
                where
                  ki_subst_range  = getTCvSubstRangeFVs kind_subst
                  -- See Note [Unification of two kind variables in deriving]
                  unmapped_tkvs   = filter (\v -> v `notElemTCvSubst` kind_subst
                                         && not (v `elemVarSet` ki_subst_range))
                                           tkvs'
                  (subst, _)      = substTyVarBndrs kind_subst unmapped_tkvs
                  final_tc_args   = substTys subst tc_args'
                  final_cls_tys   = substTys subst cls_tys'
                  final_tkvs      = tyCoVarsOfTypesWellScoped $
                                    final_cls_tys ++ final_tc_args

        ; let tkvs = scopedSort $ fvVarList $
                     unionFV (tyCoFVsOfTypes tc_args_to_keep)
                             (FV.mkFVs deriv_tvs)
              Just kind_subst = mb_match
              (tkvs', final_cls_tys', final_tc_args')
                = propagate_subst kind_subst tkvs cls_tys tc_args_to_keep

          -- See Note [Unify kinds in deriving]
        ; (tkvs, final_cls_tys, final_tc_args, final_mb_deriv_strat) <-
            case mb_deriv_strat' of
              -- Perform an additional unification with the kind of the `via`
              -- type and the result of the previous kind unification.
              Just (ViaStrategy via_ty) -> do
                let final_via_ty   = via_ty
                    final_via_kind = tcTypeKind final_via_ty
                    final_inst_ty_kind
                              = tcTypeKind (mkTyConApp tc final_tc_args')
                    via_match = tcUnifyTy final_inst_ty_kind final_via_kind

                checkTc (isJust via_match)
                        (derivingViaKindErr cls final_inst_ty_kind
                                            final_via_ty final_via_kind)

                let Just via_subst = via_match
                    (final_tkvs, final_cls_tys, final_tc_args)
                      = propagate_subst via_subst tkvs'
                                        final_cls_tys' final_tc_args'
                pure ( final_tkvs, final_cls_tys, final_tc_args
                     , Just $ ViaStrategy $ substTy via_subst via_ty
                     )

              _ -> pure ( tkvs', final_cls_tys', final_tc_args'
                        , mb_deriv_strat' )

        ; traceTc "Deriving strategy (deriving clause)" $
            vcat [ppr final_mb_deriv_strat, ppr deriv_pred]

        ; traceTc "derivTyData1" (vcat [ pprTyVars tvs, ppr tc, ppr tc_args
                                       , ppr deriv_pred
                                       , pprTyVars (tyCoVarsOfTypesList tc_args)
                                       , ppr n_args_to_keep, ppr n_args_to_drop
                                       , ppr inst_ty_kind, ppr cls_arg_kind, ppr mb_match
                                       , ppr final_tc_args, ppr final_cls_tys ])

        ; traceTc "derivTyData2" (vcat [ ppr tkvs ])

        ; let final_tc_app = mkTyConApp tc final_tc_args
        ; checkTc (allDistinctTyVars (mkVarSet tkvs) args_to_drop)     -- (a, b, c)
                  (derivingEtaErr cls final_cls_tys final_tc_app)
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

        ; checkValidInstHead DerivClauseCtxt cls $
                             final_cls_tys ++ [final_tc_app]
                -- Check that we aren't deriving an instance of a magical
                -- type like (~) or Coercible (#14916).

        ; spec <- mkEqnHelp Nothing tkvs
                            cls final_cls_tys tc final_tc_args
                            (InferContext Nothing) final_mb_deriv_strat
        ; traceTc "derivTyData" (ppr spec)
        ; return $ Just spec } }


{- Note [tc_args and tycon arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might wonder if we could use (tyConArity tc) at this point, rather
than (length tc_args).  But for data families the two can differ!  The
tc and tc_args passed into 'deriveTyData' come from 'deriveClause' which
in turn gets them from 'tyConFamInstSig_maybe' which in turn gets them
from DataFamInstTyCon:

| DataFamInstTyCon          -- See Note [Data type families]
      (CoAxiom Unbranched)
      TyCon   -- The family TyCon
      [Type]  -- Argument types (mentions the tyConTyVars of this TyCon)
              -- No shorter in length than the tyConTyVars of the family TyCon
              -- How could it be longer? See [Arity of data families] in FamInstEnv

Notice that the arg tys might not be the same as the family tycon arity
(= length tyConTyVars).

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

But this does not typecheck by design: kind equalities are not allowed to be
bound in types, only terms. But in essence, the two instance declarations are
entirely equivalent, since even though (T k) matches any kind k, the only
possibly value for k is *, since anything else is ill-typed. As a result, we can
just as comfortably use (T *).

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

Even though we requested a derived instance of the form (Cat k Fun), the
kind unification will actually generate (Cat * Fun) (i.e., the same thing as if
the user wrote deriving (Cat *)).

What happens with DerivingVia, when you have yet another type? Consider:

  newtype Foo (a :: Type) = MkFoo (Proxy a)
    deriving Functor via Proxy

As before, we unify the kind of Foo (* -> *) with the kind of the argument to
Functor (* -> *). But that's not enough: the `via` type, Proxy, has the kind
(k -> *), which is more general than what we want. So we must additionally
unify (k -> *) with (* -> *).

Currently, all of this unification is implemented kludgily with the pure
unifier, which is rather tiresome. Trac #14331 lays out a plan for how this
might be made cleaner.

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
    (subst, _)    = substTyVarBndrs kind_subst unmapped_tkvs

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

It is also possible to define kind synonyms, and they can mention other types in
a datatype declaration. For example,

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
          -> DerivContext
               -- SupplyContext => context supplied (standalone deriving)
               -- InferContext  => context inferred (deriving on data decl, or
               --                  standalone deriving decl with a wildcard)
          -> Maybe (DerivStrategy GhcTc)
          -> TcRn EarlyDerivSpec
-- Make the EarlyDerivSpec for an instance
--      forall tvs. theta => cls (tys ++ [ty])
-- where the 'theta' is optional (that's the Maybe part)
-- Assumes that this declaration is well-kinded

mkEqnHelp overlap_mode tvs cls cls_tys tycon tc_args deriv_ctxt deriv_strat
  = do {      -- Find the instance of a data family
              -- Note [Looking up family instances for deriving]
         fam_envs <- tcGetFamInstEnvs
       ; let (rep_tc, rep_tc_args, _co) = tcLookupDataFamInst fam_envs tycon tc_args
              -- If it's still a data family, the lookup failed; i.e no instance exists
       ; when (isDataFamilyTyCon rep_tc)
              (bale_out (text "No family instance for" <+> quotes (pprTypeApp tycon tc_args)))
       ; is_boot <- tcIsHsBootOrSig
       ; when is_boot $
              bale_out (text "Cannot derive instances in hs-boot files"
                    $+$ text "Write an instance declaration instead")

       ; let deriv_env = DerivEnv
                         { denv_overlap_mode = overlap_mode
                         , denv_tvs          = tvs
                         , denv_cls          = cls
                         , denv_cls_tys      = cls_tys
                         , denv_tc           = tycon
                         , denv_tc_args      = tc_args
                         , denv_rep_tc       = rep_tc
                         , denv_rep_tc_args  = rep_tc_args
                         , denv_ctxt         = deriv_ctxt
                         , denv_strat        = deriv_strat }
       ; flip runReaderT deriv_env $
         if isNewTyCon rep_tc then mkNewTypeEqn else mkDataTypeEqn }
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

-- | Derive an instance for a data type (i.e., non-newtype).
mkDataTypeEqn :: DerivM EarlyDerivSpec
mkDataTypeEqn
  = do mb_strat <- asks denv_strat
       let bale_out msg = do err <- derivingThingErrM False msg
                             lift $ failWithTc err
       case mb_strat of
         Just StockStrategy    -> mk_eqn_stock    mk_originative_eqn bale_out
         Just AnyclassStrategy -> mk_eqn_anyclass mk_originative_eqn bale_out
         Just (ViaStrategy ty) -> mk_eqn_via ty
         -- GeneralizedNewtypeDeriving makes no sense for non-newtypes
         Just NewtypeStrategy  -> bale_out gndNonNewtypeErr
         -- Lacking a user-requested deriving strategy, we will try to pick
         -- between the stock or anyclass strategies
         Nothing -> mk_eqn_no_mechanism mk_originative_eqn bale_out

-- Derive an instance by way of an originative deriving strategy
-- (stock or anyclass).
--
-- See Note [Deriving strategies]
mk_originative_eqn
  :: DerivSpecMechanism -- Invariant: This will be DerivSpecStock or
                        -- DerivSpecAnyclass
  -> DerivM EarlyDerivSpec
mk_originative_eqn mechanism
  = do DerivEnv { denv_overlap_mode = overlap_mode
                , denv_tvs          = tvs
                , denv_tc           = tc
                , denv_tc_args      = tc_args
                , denv_rep_tc       = rep_tc
                , denv_cls          = cls
                , denv_cls_tys      = cls_tys
                , denv_ctxt         = deriv_ctxt } <- ask
       let inst_ty  = mkTyConApp tc tc_args
           inst_tys = cls_tys ++ [inst_ty]
       doDerivInstErrorChecks1 mechanism
       loc       <- lift getSrcSpanM
       dfun_name <- lift $ newDFunName' cls tc
       case deriv_ctxt of
        InferContext wildcard ->
          do { (inferred_constraints, tvs', inst_tys')
                 <- inferConstraints mechanism
             ; return $ InferTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs'
                   , ds_cls = cls, ds_tys = inst_tys'
                   , ds_tc = rep_tc
                   , ds_theta = inferred_constraints
                   , ds_overlap = overlap_mode
                   , ds_standalone_wildcard = wildcard
                   , ds_mechanism = mechanism } }

        SupplyContext theta ->
            return $ GivenTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs
                   , ds_cls = cls, ds_tys = inst_tys
                   , ds_tc = rep_tc
                   , ds_theta = theta
                   , ds_overlap = overlap_mode
                   , ds_standalone_wildcard = Nothing
                   , ds_mechanism = mechanism }

-- Derive an instance by way of a coerce-based deriving strategy
-- (newtype or via).
--
-- See Note [Deriving strategies]
mk_coerce_based_eqn
  :: (Type -> DerivSpecMechanism) -- Invariant: This will be DerivSpecNewtype
                                  -- or DerivSpecVia
  -> Type -- The type to coerce
  -> DerivM EarlyDerivSpec
mk_coerce_based_eqn mk_mechanism coerced_ty
  = do DerivEnv { denv_overlap_mode = overlap_mode
                , denv_tvs          = tvs
                , denv_tc           = tycon
                , denv_tc_args      = tc_args
                , denv_rep_tc       = rep_tycon
                , denv_cls          = cls
                , denv_cls_tys      = cls_tys
                , denv_ctxt         = deriv_ctxt } <- ask
       sa_wildcard <- isStandaloneWildcardDeriv
       let -- The following functions are polymorphic over the representation
           -- type, since we might either give it the underlying type of a
           -- newtype (for GeneralizedNewtypeDeriving) or a @via@ type
           -- (for DerivingVia).
           rep_tys ty  = cls_tys ++ [ty]
           rep_pred ty = mkClassPred cls (rep_tys ty)
           rep_pred_o ty = mkPredOrigin deriv_origin TypeLevel (rep_pred ty)
                   -- rep_pred is the representation dictionary, from where
                   -- we are going to get all the methods for the final
                   -- dictionary

           -- Next we figure out what superclass dictionaries to use
           -- See Note [Newtype deriving superclasses] above
           sc_preds   :: [PredOrigin]
           cls_tyvars = classTyVars cls
           inst_ty    = mkTyConApp tycon tc_args
           inst_tys   = cls_tys ++ [inst_ty]
           sc_preds   = map (mkPredOrigin deriv_origin TypeLevel) $
                        substTheta (zipTvSubst cls_tyvars inst_tys) $
                        classSCTheta cls
           deriv_origin = mkDerivOrigin sa_wildcard

           -- Next we collect constraints for the class methods
           -- If there are no methods, we don't need any constraints
           -- Otherwise we need (C rep_ty), for the representation methods,
           -- and constraints to coerce each individual method
           meth_preds :: Type -> [PredOrigin]
           meths = classMethods cls
           meth_preds ty
             | null meths = [] -- No methods => no constraints
                               -- (Trac #12814)
             | otherwise = rep_pred_o ty : coercible_constraints ty
           coercible_constraints ty
             = [ mkPredOrigin (DerivOriginCoerce meth t1 t2 sa_wildcard)
                              TypeLevel (mkReprPrimEqPred t1 t2)
               | meth <- meths
               , let (Pair t1 t2) = mkCoerceClassMethEqn cls tvs
                                            inst_tys ty meth ]

           all_thetas :: Type -> [ThetaOrigin]
           all_thetas ty = [mkThetaOriginFromPreds $ meth_preds ty ++ sc_preds]

           inferred_thetas = all_thetas coerced_ty
       lift $ traceTc "newtype deriving:" $
         ppr tycon <+> ppr (rep_tys coerced_ty) <+> ppr inferred_thetas
       let mechanism = mk_mechanism coerced_ty
           bale_out msg = do err <- derivingThingErrMechanism mechanism msg
                             lift $ failWithTc err
       atf_coerce_based_error_checks cls bale_out
       doDerivInstErrorChecks1 mechanism
       dfun_name <- lift $ newDFunName' cls tycon
       loc       <- lift getSrcSpanM
       case deriv_ctxt of
        SupplyContext theta -> return $ GivenTheta $ DS
            { ds_loc = loc
            , ds_name = dfun_name, ds_tvs = tvs
            , ds_cls = cls, ds_tys = inst_tys
            , ds_tc = rep_tycon
            , ds_theta = theta
            , ds_overlap = overlap_mode
            , ds_standalone_wildcard = Nothing
            , ds_mechanism = mechanism }
        InferContext wildcard -> return $ InferTheta $ DS
            { ds_loc = loc
            , ds_name = dfun_name, ds_tvs = tvs
            , ds_cls = cls, ds_tys = inst_tys
            , ds_tc = rep_tycon
            , ds_theta = inferred_thetas
            , ds_overlap = overlap_mode
            , ds_standalone_wildcard = wildcard
            , ds_mechanism = mechanism }

-- Ensure that a class's associated type variables are suitable for
-- GeneralizedNewtypeDeriving or DerivingVia.
--
-- See Note [GND and associated type families]
atf_coerce_based_error_checks
  :: Class
  -> (SDoc -> DerivM ())
  -> DerivM ()
atf_coerce_based_error_checks cls bale_out
  = let cls_tyvars = classTyVars cls

        ats_look_sensible
           =  -- Check (a) from Note [GND and associated type families]
              no_adfs
              -- Check (b) from Note [GND and associated type families]
           && isNothing at_without_last_cls_tv
              -- Check (d) from Note [GND and associated type families]
           && isNothing at_last_cls_tv_in_kinds

        (adf_tcs, atf_tcs) = partition isDataFamilyTyCon at_tcs
        no_adfs            = null adf_tcs
               -- We cannot newtype-derive data family instances

        at_without_last_cls_tv
          = find (\tc -> last_cls_tv `notElem` tyConTyVars tc) atf_tcs
        at_last_cls_tv_in_kinds
          = find (\tc -> any (at_last_cls_tv_in_kind . tyVarKind)
                             (tyConTyVars tc)
                      || at_last_cls_tv_in_kind (tyConResKind tc)) atf_tcs
        at_last_cls_tv_in_kind kind
          = last_cls_tv `elemVarSet` exactTyCoVarsOfType kind
        at_tcs = classATs cls
        last_cls_tv = ASSERT( notNull cls_tyvars )
                      last cls_tyvars

        cant_derive_err
           = vcat [ ppUnless no_adfs adfs_msg
                  , maybe empty at_without_last_cls_tv_msg
                          at_without_last_cls_tv
                  , maybe empty at_last_cls_tv_in_kinds_msg
                          at_last_cls_tv_in_kinds
                  ]
        adfs_msg  = text "the class has associated data types"
        at_without_last_cls_tv_msg at_tc = hang
          (text "the associated type" <+> quotes (ppr at_tc)
           <+> text "is not parameterized over the last type variable")
          2 (text "of the class" <+> quotes (ppr cls))
        at_last_cls_tv_in_kinds_msg at_tc = hang
          (text "the associated type" <+> quotes (ppr at_tc)
           <+> text "contains the last type variable")
         2 (text "of the class" <+> quotes (ppr cls)
           <+> text "in a kind, which is not (yet) allowed")
    in unless ats_look_sensible $ bale_out cant_derive_err

mk_eqn_stock :: (DerivSpecMechanism -> DerivM EarlyDerivSpec)
             -> (SDoc -> DerivM EarlyDerivSpec)
             -> DerivM EarlyDerivSpec
mk_eqn_stock go_for_it bale_out
  = do DerivEnv { denv_tc      = tc
                , denv_rep_tc  = rep_tc
                , denv_cls     = cls
                , denv_cls_tys = cls_tys
                , denv_ctxt    = deriv_ctxt } <- ask
       dflags <- getDynFlags
       case checkOriginativeSideConditions dflags deriv_ctxt cls cls_tys
                                           tc rep_tc of
         CanDeriveStock gen_fn -> go_for_it $ DerivSpecStock gen_fn
         StockClassError msg   -> bale_out msg
         _                     -> bale_out (nonStdErr cls)

mk_eqn_anyclass :: (DerivSpecMechanism -> DerivM EarlyDerivSpec)
                -> (SDoc -> DerivM EarlyDerivSpec)
                -> DerivM EarlyDerivSpec
mk_eqn_anyclass go_for_it bale_out
  = do dflags <- getDynFlags
       case canDeriveAnyClass dflags of
         IsValid      -> go_for_it DerivSpecAnyClass
         NotValid msg -> bale_out msg

mk_eqn_newtype :: Type -- The newtype's representation type
               -> DerivM EarlyDerivSpec
mk_eqn_newtype = mk_coerce_based_eqn DerivSpecNewtype

mk_eqn_via :: Type -- The @via@ type
           -> DerivM EarlyDerivSpec
mk_eqn_via = mk_coerce_based_eqn DerivSpecVia

mk_eqn_no_mechanism :: (DerivSpecMechanism -> DerivM EarlyDerivSpec)
                    -> (SDoc -> DerivM EarlyDerivSpec)
                    -> DerivM EarlyDerivSpec
mk_eqn_no_mechanism go_for_it bale_out
  = do DerivEnv { denv_tc      = tc
                , denv_rep_tc  = rep_tc
                , denv_cls     = cls
                , denv_cls_tys = cls_tys
                , denv_ctxt    = deriv_ctxt } <- ask
       dflags <- getDynFlags

           -- See Note [Deriving instances for classes themselves]
       let dac_error msg
             | isClassTyCon rep_tc
             = quotes (ppr tc) <+> text "is a type class,"
                               <+> text "and can only have a derived instance"
                               $+$ text "if DeriveAnyClass is enabled"
             | otherwise
             = nonStdErr cls $$ msg

       case checkOriginativeSideConditions dflags deriv_ctxt cls cls_tys
                                           tc rep_tc of
           -- NB: pass the *representation* tycon to
           -- checkOriginativeSideConditions
           NonDerivableClass   msg -> bale_out (dac_error msg)
           StockClassError msg     -> bale_out msg
           CanDeriveStock gen_fn   -> go_for_it $ DerivSpecStock gen_fn
           CanDeriveAnyClass       -> go_for_it DerivSpecAnyClass

{-
************************************************************************
*                                                                      *
            GeneralizedNewtypeDeriving and DerivingVia
*                                                                      *
************************************************************************
-}

-- | Derive an instance for a newtype.
mkNewTypeEqn :: DerivM EarlyDerivSpec
mkNewTypeEqn
-- Want: instance (...) => cls (cls_tys ++ [tycon tc_args]) where ...
  = do DerivEnv { denv_tc           = tycon
                , denv_rep_tc       = rep_tycon
                , denv_rep_tc_args  = rep_tc_args
                , denv_cls          = cls
                , denv_cls_tys      = cls_tys
                , denv_ctxt         = deriv_ctxt
                , denv_strat        = mb_strat } <- ask
       dflags <- getDynFlags

       let newtype_deriving  = xopt LangExt.GeneralizedNewtypeDeriving dflags
           deriveAnyClass    = xopt LangExt.DeriveAnyClass             dflags
           bale_out        = bale_out' newtype_deriving
           bale_out' b msg = do err <- derivingThingErrM b msg
                                lift $ failWithTc err

           non_std     = nonStdErr cls
           suggest_gnd = text "Try GeneralizedNewtypeDeriving for GHC's"
                     <+> text "newtype-deriving extension"

           -- Here is the plan for newtype derivings.  We see
           --        newtype T a1...an = MkT (t ak+1...an)
           --          deriving (.., C s1 .. sm, ...)
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
                   -- For newtype T a b = MkT (S a a b), the TyCon
                   -- machinery already eta-reduces the representation type, so
                   -- we know that
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

           -------------------------------------------------------------------
           --  Figuring out whether we can only do this newtype-deriving thing

           -- See Note [Determining whether newtype-deriving is appropriate]
           might_be_newtype_derivable
              =  not (non_coercible_class cls)
              && eta_ok
--            && not (isRecursiveTyCon tycon)      -- Note [Recursive newtypes]

           -- Check that eta reduction is OK
           eta_ok = rep_tc_args `lengthAtLeast` nt_eta_arity
             -- The newtype can be eta-reduced to match the number
             --     of type argument actually supplied
             --        newtype T a b = MkT (S [a] b) deriving( Monad )
             --     Here the 'b' must be the same in the rep type (S [a] b)
             --     And the [a] must not mention 'b'.  That's all handled
             --     by nt_eta_rity.

           cant_derive_err = ppUnless eta_ok  eta_msg
           eta_msg = text "cannot eta-reduce the representation type enough"

       MASSERT( cls_tys `lengthIs` (classArity cls - 1) )
       case mb_strat of
         Just StockStrategy    -> mk_eqn_stock    mk_originative_eqn bale_out
         Just AnyclassStrategy -> mk_eqn_anyclass mk_originative_eqn bale_out
         Just NewtypeStrategy  ->
           -- Since the user explicitly asked for GeneralizedNewtypeDeriving,
           -- we don't need to perform all of the checks we normally would,
           -- such as if the class being derived is known to produce ill-roled
           -- coercions (e.g., Traversable), since we can just derive the
           -- instance and let it error if need be.
           -- See Note [Determining whether newtype-deriving is appropriate]
           if eta_ok && newtype_deriving
             then mk_eqn_newtype rep_inst_ty
             else bale_out (cant_derive_err $$
                            if newtype_deriving then empty else suggest_gnd)
         Just (ViaStrategy via_ty) ->
           -- NB: For DerivingVia, we don't need to any eta-reduction checking,
           -- since the @via@ type is already "eta-reduced".
           mk_eqn_via via_ty
         Nothing
           | might_be_newtype_derivable
             && ((newtype_deriving && not deriveAnyClass)
                  || std_class_via_coercible cls)
          -> mk_eqn_newtype rep_inst_ty
           | otherwise
          -> case checkOriginativeSideConditions dflags deriv_ctxt cls cls_tys
                                                 tycon rep_tycon of
               StockClassError msg
                 -- There's a particular corner case where
                 --
                 -- 1. -XGeneralizedNewtypeDeriving and -XDeriveAnyClass are
                 --    both enabled at the same time
                 -- 2. We're deriving a particular stock derivable class
                 --    (such as Functor)
                 --
                 -- and the previous cases won't catch it. This fixes the bug
                 -- reported in Trac #10598.
                 | might_be_newtype_derivable && newtype_deriving
                -> mk_eqn_newtype rep_inst_ty
                 -- Otherwise, throw an error for a stock class
                 | might_be_newtype_derivable && not newtype_deriving
                -> bale_out (msg $$ suggest_gnd)
                 | otherwise
                -> bale_out msg

               -- Must use newtype deriving or DeriveAnyClass
               NonDerivableClass _msg
                 -- Too hard, even with newtype deriving
                 | newtype_deriving           -> bale_out cant_derive_err
                 -- Try newtype deriving!
                 -- Here we suggest GeneralizedNewtypeDeriving even in cases
                 -- where it may not be applicable. See Trac #9600.
                 | otherwise                  -> bale_out (non_std $$ suggest_gnd)

               -- DeriveAnyClass
               CanDeriveAnyClass -> do
                 -- If both DeriveAnyClass and GeneralizedNewtypeDeriving are
                 -- enabled, we take the diplomatic approach of defaulting to
                 -- DeriveAnyClass, but emitting a warning about the choice.
                 -- See Note [Deriving strategies]
                 when (newtype_deriving && deriveAnyClass) $
                   lift $ addWarnTc NoReason $ sep
                     [ text "Both DeriveAnyClass and"
                       <+> text "GeneralizedNewtypeDeriving are enabled"
                     , text "Defaulting to the DeriveAnyClass strategy"
                       <+> text "for instantiating" <+> ppr cls
                     , text "Use DerivingStrategies to pick"
                       <+> text "a different strategy"
                      ]
                 mk_originative_eqn DerivSpecAnyClass
               -- CanDeriveStock
               CanDeriveStock gen_fn -> mk_originative_eqn $
                                        DerivSpecStock gen_fn

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

Note [GND and associated type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's possible to use GeneralizedNewtypeDeriving (GND) to derive instances for
classes with associated type families. A general recipe is:

    class C x y z where
      type T y z x
      op :: x -> [y] -> z

    newtype N a = MkN <rep-type> deriving( C )

    =====>

    instance C x y <rep-type> => C x y (N a) where
      type T y (N a) x = T y <rep-type> x
      op = coerce (op :: x -> [y] -> <rep-type>)

However, we must watch out for three things:

(a) The class must not contain any data families. If it did, we'd have to
    generate a fresh data constructor name for the derived data family
    instance, and it's not clear how to do this.

(b) Each associated type family's type variables must mention the last type
    variable of the class. As an example, you wouldn't be able to use GND to
    derive an instance of this class:

      class C a b where
        type T a

    But you would be able to derive an instance of this class:

      class C a b where
        type T b

    The difference is that in the latter T mentions the last parameter of C
    (i.e., it mentions b), but the former T does not. If you tried, e.g.,

      newtype Foo x = Foo x deriving (C a)

    with the former definition of C, you'd end up with something like this:

      instance C a (Foo x) where
        type T a = T ???

    This T family instance doesn't mention the newtype (or its representation
    type) at all, so we disallow such constructions with GND.

(c) UndecidableInstances might need to be enabled. Here's a case where it is
    most definitely necessary:

      class C a where
        type T a
      newtype Loop = Loop MkLoop deriving C

      =====>

      instance C Loop where
        type T Loop = T Loop

    Obviously, T Loop would send the typechecker into a loop. Unfortunately,
    you might even need UndecidableInstances even in cases where the
    typechecker would be guaranteed to terminate. For example:

      instance C Int where
        type C Int = Int
      newtype MyInt = MyInt Int deriving C

      =====>

      instance C MyInt where
        type T MyInt = T Int

    GHC's termination checker isn't sophisticated enough to conclude that the
    definition of T MyInt terminates, so UndecidableInstances is required.

(d) For the time being, we do not allow the last type variable of the class to
    appear in a /kind/ of an associated type family definition. For instance:

    class C a where
      type T1 a        -- OK
      type T2 (x :: a) -- Illegal: a appears in the kind of x
      type T3 y :: a   -- Illegal: a appears in the kind of (T3 y)

    The reason we disallow this is because our current approach to deriving
    associated type family instances—i.e., by unwrapping the newtype's type
    constructor as shown above—is ill-equipped to handle the scenario when
    the last type variable appears as an implicit argument. In the worst case,
    allowing the last variable to appear in a kind can result in improper Core
    being generated (see #14728).

    There is hope for this feature being added some day, as one could
    conceivably take a newtype axiom (which witnesses a coercion between a
    newtype and its representation type) at lift that through each associated
    type at the Core level. See #14728, comment:3 for a sketch of how this
    might work. Until then, we disallow this featurette wholesale.

The same criteria apply to DerivingVia.

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
genInst :: DerivSpec theta
        -> TcM (ThetaType -> TcM (InstInfo GhcPs), BagDerivStuff, [Name])
-- We must use continuation-returning style here to get the order in which we
-- typecheck family instances and derived instances right.
-- See Note [Staging of tcDeriving]
genInst spec@(DS { ds_tvs = tvs, ds_tc = rep_tycon
                 , ds_mechanism = mechanism, ds_tys = tys
                 , ds_cls = clas, ds_loc = loc
                 , ds_standalone_wildcard = wildcard })
  = do (meth_binds, deriv_stuff, unusedNames)
         <- set_span_and_ctxt $
            genDerivStuff mechanism loc clas rep_tycon tys tvs
       let mk_inst_info theta = set_span_and_ctxt $ do
             inst_spec <- newDerivClsInst theta spec
             doDerivInstErrorChecks2 clas inst_spec theta wildcard mechanism
             traceTc "newder" (ppr inst_spec)
             return $ InstInfo
                       { iSpec   = inst_spec
                       , iBinds  = InstBindings
                                     { ib_binds = meth_binds
                                     , ib_tyvars = map Var.varName tvs
                                     , ib_pragmas = []
                                     , ib_extensions = extensions
                                     , ib_derived = True } }
       return (mk_inst_info, deriv_stuff, unusedNames)
  where
    extensions :: [LangExt.Extension]
    extensions
      | isDerivSpecNewtype mechanism || isDerivSpecVia mechanism
        -- Both these flags are needed for higher-rank uses of coerce
        -- See Note [Newtype-deriving instances] in TcGenDeriv
      = [LangExt.ImpredicativeTypes, LangExt.RankNTypes]
      | otherwise
      = []

    set_span_and_ctxt :: TcM a -> TcM a
    set_span_and_ctxt = setSrcSpan loc . addErrCtxt (instDeclCtxt3 clas tys)

doDerivInstErrorChecks1 :: DerivSpecMechanism -> DerivM ()
doDerivInstErrorChecks1 mechanism = do
    DerivEnv { denv_tc      = tc
             , denv_rep_tc  = rep_tc } <- ask
    standalone <- isStandaloneDeriv
    let anyclass_strategy = isDerivSpecAnyClass mechanism
        via_strategy      = isDerivSpecVia mechanism
        bale_out msg = do err <- derivingThingErrMechanism mechanism msg
                          lift $ failWithTc err

    -- For standalone deriving, check that all the data constructors are in
    -- scope...
    rdr_env <- lift getGlobalRdrEnv
    let data_con_names = map dataConName (tyConDataCons rep_tc)
        hidden_data_cons = not (isWiredInName (tyConName rep_tc)) &&
                           (isAbstractTyCon rep_tc ||
                            any not_in_scope data_con_names)
        not_in_scope dc  = isNothing (lookupGRE_Name rdr_env dc)

    lift $ addUsedDataCons rdr_env rep_tc

    -- ...however, we don't perform this check if we're using DeriveAnyClass,
    -- since it doesn't generate any code that requires use of a data
    -- constructor. Nor do we perform this check with @deriving via@, as it
    -- doesn't explicitly require the constructors to be in scope.
    unless (anyclass_strategy || via_strategy
            || not standalone || not hidden_data_cons) $
           bale_out $ derivingHiddenErr tc

doDerivInstErrorChecks2 :: Class -> ClsInst -> ThetaType -> Maybe SrcSpan
                        -> DerivSpecMechanism -> TcM ()
doDerivInstErrorChecks2 clas clas_inst theta wildcard mechanism
  = do { traceTc "doDerivInstErrorChecks2" (ppr clas_inst)
       ; dflags <- getDynFlags
       ; xpartial_sigs <- xoptM LangExt.PartialTypeSignatures
       ; wpartial_sigs <- woptM Opt_WarnPartialTypeSignatures

         -- Error if PartialTypeSignatures isn't enabled when a user tries
         -- to write @deriving instance _ => Eq (Foo a)@. Or, if that
         -- extension is enabled, give a warning if -Wpartial-type-signatures
         -- is enabled.
       ; case wildcard of
           Nothing -> pure ()
           Just span -> setSrcSpan span $ do
             checkTc xpartial_sigs (hang partial_sig_msg 2 pts_suggestion)
             warnTc (Reason Opt_WarnPartialTypeSignatures)
                    wpartial_sigs partial_sig_msg

         -- Check for Generic instances that are derived with an exotic
         -- deriving strategy like DAC
         -- See Note [Deriving strategies]
       ; when (exotic_mechanism && className clas `elem` genericClassNames) $
         do { failIfTc (safeLanguageOn dflags) gen_inst_err
            ; when (safeInferOn dflags) (recordUnsafeInfer emptyBag) } }
  where
    exotic_mechanism = not $ isDerivSpecStock mechanism

    partial_sig_msg = text "Found type wildcard" <+> quotes (char '_')
                  <+> text "standing for" <+> quotes (pprTheta theta)

    pts_suggestion
      = text "To use the inferred type, enable PartialTypeSignatures"

    gen_inst_err = text "Generic instances can only be derived in"
               <+> text "Safe Haskell using the stock strategy."

genDerivStuff :: DerivSpecMechanism -> SrcSpan -> Class
              -> TyCon -> [Type] -> [TyVar]
              -> TcM (LHsBinds GhcPs, BagDerivStuff, [Name])
genDerivStuff mechanism loc clas tycon inst_tys tyvars
  = case mechanism of
      -- See Note [Bindings for Generalised Newtype Deriving]
      DerivSpecNewtype rhs_ty -> gen_newtype_or_via rhs_ty

      -- Try a stock deriver
      DerivSpecStock gen_fn -> gen_fn loc tycon inst_tys

      -- Try DeriveAnyClass
      DerivSpecAnyClass -> do
        let mini_env   = mkVarEnv (classTyVars clas `zip` inst_tys)
            mini_subst = mkTvSubst (mkInScopeSet (mkVarSet tyvars)) mini_env
        dflags <- getDynFlags
        tyfam_insts <-
          -- canDeriveAnyClass should ensure that this code can't be reached
          -- unless -XDeriveAnyClass is enabled.
          ASSERT2( isValid (canDeriveAnyClass dflags)
                 , ppr "genDerivStuff: bad derived class" <+> ppr clas )
          mapM (tcATDefault loc mini_subst emptyNameSet)
               (classATItems clas)
        return ( emptyBag -- No method bindings are needed...
               , listToBag (map DerivFamInst (concat tyfam_insts))
               -- ...but we may need to generate binding for associated type
               -- family default instances.
               -- See Note [DeriveAnyClass and default family instances]
               , [] )

      -- Try DerivingVia
      DerivSpecVia via_ty -> gen_newtype_or_via via_ty
  where
    gen_newtype_or_via ty = do
      (binds, faminsts) <- gen_Newtype_binds loc clas tyvars inst_tys ty
      return (binds, faminsts, maybeToList unusedConName)

    unusedConName :: Maybe Name
    unusedConName
      | isDerivSpecNewtype mechanism
        -- See Note [Newtype deriving and unused constructors]
      = Just $ getName $ head $ tyConDataCons tycon
      | otherwise
      = Nothing

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
tcATDefault function from TcClassDcl (which is also used in TcInstDcls to
handle the empty instance declaration case).

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

* via: Use -XDerivingVia

The latter two strategies (newtype and via) are referred to as the
"coerce-based" strategies, since they generate code that relies on the `coerce`
function. The former two strategies (stock and anyclass), in contrast, are
referred to as the "originative" strategies, since they create "original"
instances instead of "reusing" old instances (by way of `coerce`).

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

Note [Deriving instances for classes themselves]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Much of the code in TcDeriv assumes that deriving only works on data types.
But this assumption doesn't hold true for DeriveAnyClass, since it's perfectly
reasonable to do something like this:

  {-# LANGUAGE DeriveAnyClass #-}
  class C1 (a :: Constraint) where
  class C2 where
  deriving instance C1 C2
    -- This is equivalent to `instance C1 C2`

If DeriveAnyClass isn't enabled in the code above (i.e., it defaults to stock
deriving), we throw a special error message indicating that DeriveAnyClass is
the only way to go. We don't bother throwing this error if an explicit 'stock'
or 'newtype' keyword is used, since both options have their own perfectly
sensible error messages in the case of the above code (as C1 isn't a stock
derivable class, and C2 isn't a newtype).

************************************************************************
*                                                                      *
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
*                                                                      *
************************************************************************
-}

nonUnaryErr :: LHsSigType GhcRn -> SDoc
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

derivingViaKindErr :: Class -> Kind -> Type -> Kind -> MsgDoc
derivingViaKindErr cls cls_kind via_ty via_kind
  = hang (text "Cannot derive instance via" <+> quotes (pprType via_ty))
       2 (text "Class" <+> quotes (ppr cls)
               <+> text "expects an argument of kind"
               <+> quotes (pprKind cls_kind) <> char ','
      $+$ text "but" <+> quotes (pprType via_ty)
               <+> text "has kind" <+> quotes (pprKind via_kind))

derivingEtaErr :: Class -> [Type] -> Type -> MsgDoc
derivingEtaErr cls cls_tys inst_ty
  = sep [text "Cannot eta-reduce to an instance of form",
         nest 2 (text "instance (...) =>"
                <+> pprClassPred cls (cls_tys ++ [inst_ty]))]

derivingThingErr :: Bool -> Class -> [Type] -> Type
                 -> Maybe (DerivStrategy GhcTc) -> MsgDoc -> MsgDoc
derivingThingErr newtype_deriving cls cls_tys inst_ty mb_strat why
  = derivingThingErr' newtype_deriving cls cls_tys inst_ty mb_strat
                      (maybe empty derivStrategyName mb_strat) why

derivingThingErrM :: Bool -> MsgDoc -> DerivM MsgDoc
derivingThingErrM newtype_deriving why
  = do DerivEnv { denv_tc      = tc
                , denv_tc_args = tc_args
                , denv_cls     = cls
                , denv_cls_tys = cls_tys
                , denv_strat   = mb_strat } <- ask
       pure $ derivingThingErr newtype_deriving cls cls_tys
                               (mkTyConApp tc tc_args) mb_strat why

derivingThingErrMechanism :: DerivSpecMechanism -> MsgDoc -> DerivM MsgDoc
derivingThingErrMechanism mechanism why
  = do DerivEnv { denv_tc      = tc
                , denv_tc_args = tc_args
                , denv_cls     = cls
                , denv_cls_tys = cls_tys
                , denv_strat   = mb_strat } <- ask
       pure $ derivingThingErr' (isDerivSpecNewtype mechanism) cls cls_tys
                (mkTyConApp tc tc_args) mb_strat
                (derivStrategyName $ derivSpecMechanismToStrategy mechanism)
                why

derivingThingErr' :: Bool -> Class -> [Type] -> Type
                  -> Maybe (DerivStrategy GhcTc) -> MsgDoc -> MsgDoc -> MsgDoc
derivingThingErr' newtype_deriving cls cls_tys inst_ty mb_strat strat_msg why
  = sep [(hang (text "Can't make a derived instance of")
             2 (quotes (ppr pred) <+> via_mechanism)
          $$ nest 2 extra) <> colon,
         nest 2 why]
  where
    strat_used = isJust mb_strat
    extra | not strat_used, newtype_deriving
          = text "(even with cunning GeneralizedNewtypeDeriving)"
          | otherwise = empty
    pred = mkClassPred cls (cls_tys ++ [inst_ty])
    via_mechanism | strat_used
                  = text "with the" <+> strat_msg <+> text "strategy"
                  | otherwise
                  = empty

derivingHiddenErr :: TyCon -> SDoc
derivingHiddenErr tc
  = hang (text "The data constructors of" <+> quotes (ppr tc) <+> ptext (sLit "are not all in scope"))
       2 (text "so you cannot derive an instance for it")

standaloneCtxt :: LHsSigWcType GhcRn -> SDoc
standaloneCtxt ty = hang (text "In the stand-alone deriving instance for")
                       2 (quotes (ppr ty))
