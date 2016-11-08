{-
(c) The University of Glasgow 2011


The deriving code for the Generic class
(equivalent to the code in TcGenDeriv, for other classes)
-}

{-# LANGUAGE CPP, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module TcGenGenerics (canDoGenerics, canDoGenerics1,
                      GenericKind(..),
                      gen_Generic_binds, get_gen1_constrained_tys) where

import HsSyn
import Type
import TcType
import TcGenDeriv
import TcGenFunctor
import DataCon
import TyCon
import FamInstEnv       ( FamInst, FamFlavor(..), mkSingleCoAxiom )
import FamInst
import Module           ( moduleName, moduleNameFS
                        , moduleUnitId, unitIdFS, getModule )
import IfaceEnv         ( newGlobalBinder )
import Name      hiding ( varName )
import RdrName
import BasicTypes
import TysPrim
import TysWiredIn
import PrelNames
import TcEnv
import TcRnMonad
import HscTypes
import ErrUtils( Validity(..), andValid )
import SrcLoc
import Bag
import VarEnv
import VarSet (elemVarSet)
import Outputable
import FastString
import Util

import Control.Monad (mplus)
import Data.List (zip4, partition)
import Data.Maybe (isJust)

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
\subsection{Bindings for the new generic deriving mechanism}
*                                                                      *
************************************************************************

For the generic representation we need to generate:
\begin{itemize}
\item A Generic instance
\item A Rep type instance
\item Many auxiliary datatypes and instances for them (for the meta-information)
\end{itemize}
-}

gen_Generic_binds :: GenericKind -> TyCon -> [Type]
                 -> TcM (LHsBinds RdrName, FamInst)
gen_Generic_binds gk tc inst_tys = do
  repTyInsts <- tc_mkRepFamInsts gk tc inst_tys
  return (mkBindsRep gk tc, repTyInsts)

{-
************************************************************************
*                                                                      *
\subsection{Generating representation types}
*                                                                      *
************************************************************************
-}

get_gen1_constrained_tys :: TyVar -> Type -> [Type]
-- called by TcDeriv.inferConstraints; generates a list of types, each of which
-- must be a Functor in order for the Generic1 instance to work.
get_gen1_constrained_tys argVar
  = argTyFold argVar $ ArgTyAlg { ata_rec0 = const []
                                , ata_par1 = [], ata_rec1 = const []
                                , ata_comp = (:) }

{-

Note [Requirements for deriving Generic and Rep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the following, T, Tfun, and Targ are "meta-variables" ranging over type
expressions.

(Generic T) and (Rep T) are derivable for some type expression T if the
following constraints are satisfied.

  (a) D is a type constructor *value*. In other words, D is either a type
      constructor or it is equivalent to the head of a data family instance (up to
      alpha-renaming).

  (b) D cannot have a "stupid context".

  (c) The right-hand side of D cannot include existential types, universally
      quantified types, or "exotic" unlifted types. An exotic unlifted type
      is one which is not listed in the definition of allowedUnliftedTy
      (i.e., one for which we have no representation type).
      See Note [Generics and unlifted types]

  (d) T :: *.

(Generic1 T) and (Rep1 T) are derivable for some type expression T if the
following constraints are satisfied.

  (a),(b),(c) As above.

  (d) T must expect arguments, and its last parameter must have kind *.

      We use `a' to denote the parameter of D that corresponds to the last
      parameter of T.

  (e) For any type-level application (Tfun Targ) in the right-hand side of D
      where the head of Tfun is not a tuple constructor:

      (b1) `a' must not occur in Tfun.

      (b2) If `a' occurs in Targ, then Tfun :: * -> *.

-}

canDoGenerics :: TyCon -> Validity
-- canDoGenerics determines if Generic/Rep can be derived.
--
-- Check (a) from Note [Requirements for deriving Generic and Rep] is taken
-- care of because canDoGenerics is applied to rep tycons.
--
-- It returns IsValid if deriving is possible. It returns (NotValid reason)
-- if not.
canDoGenerics tc
  = mergeErrors (
          -- Check (b) from Note [Requirements for deriving Generic and Rep].
              (if (not (null (tyConStupidTheta tc)))
                then (NotValid (tc_name <+> text "must not have a datatype context"))
                else IsValid)
          -- See comment below
            : (map bad_con (tyConDataCons tc)))
  where
    -- The tc can be a representation tycon. When we want to display it to the
    -- user (in an error message) we should print its parent
    tc_name = ppr $ case tyConFamInst_maybe tc of
        Just (ptc, _) -> ptc
        _             -> tc

        -- Check (c) from Note [Requirements for deriving Generic and Rep].
        --
        -- If any of the constructors has an exotic unlifted type as argument,
        -- then we can't build the embedding-projection pair, because
        -- it relies on instantiating *polymorphic* sum and product types
        -- at the argument types of the constructors
    bad_con dc = if (any bad_arg_type (dataConOrigArgTys dc))
                  then (NotValid (ppr dc <+> text
                    "must not have exotic unlifted or polymorphic arguments"))
                  else (if (not (isVanillaDataCon dc))
                          then (NotValid (ppr dc <+> text "must be a vanilla data constructor"))
                          else IsValid)

        -- Nor can we do the job if it's an existential data constructor,
        -- Nor if the args are polymorphic types (I don't think)
    bad_arg_type ty = (isUnliftedType ty && not (allowedUnliftedTy ty))
                      || not (isTauTy ty)

-- Returns True the Type argument is an unlifted type which has a
-- corresponding generic representation type. For example,
-- (allowedUnliftedTy Int#) would return True since there is the UInt
-- representation type.
allowedUnliftedTy :: Type -> Bool
allowedUnliftedTy = isJust . unboxedRepRDRs

mergeErrors :: [Validity] -> Validity
mergeErrors []             = IsValid
mergeErrors (NotValid s:t) = case mergeErrors t of
  IsValid     -> NotValid s
  NotValid s' -> NotValid (s <> text ", and" $$ s')
mergeErrors (IsValid : t) = mergeErrors t

-- A datatype used only inside of canDoGenerics1. It's the result of analysing
-- a type term.
data Check_for_CanDoGenerics1 = CCDG1
  { _ccdg1_hasParam :: Bool       -- does the parameter of interest occurs in
                                  -- this type?
  , _ccdg1_errors   :: Validity   -- errors generated by this type
  }

{-

Note [degenerate use of FFoldType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We use foldDataConArgs here only for its ability to treat tuples
specially. foldDataConArgs also tracks covariance (though it assumes all
higher-order type parameters are covariant) and has hooks for special handling
of functions and polytypes, but we do *not* use those.

The key issue is that Generic1 deriving currently offers no sophisticated
support for functions. For example, we cannot handle

  data F a = F ((a -> Int) -> Int)

even though a is occurring covariantly.

In fact, our rule is harsh: a is simply not allowed to occur within the first
argument of (->). We treat (->) the same as any other non-tuple tycon.

Unfortunately, this means we have to track "the parameter occurs in this type"
explicitly, even though foldDataConArgs is also doing this internally.

-}

-- canDoGenerics1 determines if a Generic1/Rep1 can be derived.
--
-- Checks (a) through (c) from Note [Requirements for deriving Generic and Rep]
-- are taken care of by the call to canDoGenerics.
--
-- It returns IsValid if deriving is possible. It returns (NotValid reason)
-- if not.
canDoGenerics1 :: TyCon -> Validity
canDoGenerics1 rep_tc =
  canDoGenerics rep_tc `andValid` additionalChecks
  where
    additionalChecks
        -- check (d) from Note [Requirements for deriving Generic and Rep]
      | null (tyConTyVars rep_tc) = NotValid $
          text "Data type" <+> quotes (ppr rep_tc)
      <+> text "must have some type parameters"

      | otherwise = mergeErrors $ concatMap check_con data_cons

    data_cons = tyConDataCons rep_tc
    check_con con = case check_vanilla con of
      j@(NotValid {}) -> [j]
      IsValid -> _ccdg1_errors `map` foldDataConArgs (ft_check con) con

    bad :: DataCon -> SDoc -> SDoc
    bad con msg = text "Constructor" <+> quotes (ppr con) <+> msg

    check_vanilla :: DataCon -> Validity
    check_vanilla con | isVanillaDataCon con = IsValid
                      | otherwise            = NotValid (bad con existential)

    bmzero      = CCDG1 False IsValid
    bmbad con s = CCDG1 True $ NotValid $ bad con s
    bmplus (CCDG1 b1 m1) (CCDG1 b2 m2) = CCDG1 (b1 || b2) (m1 `andValid` m2)

    -- check (e) from Note [Requirements for deriving Generic and Rep]
    -- See also Note [degenerate use of FFoldType]
    ft_check :: DataCon -> FFoldType Check_for_CanDoGenerics1
    ft_check con = FT
      { ft_triv = bmzero

      , ft_var = caseVar, ft_co_var = caseVar

      -- (component_0,component_1,...,component_n)
      , ft_tup = \_ components -> if any _ccdg1_hasParam (init components)
                                  then bmbad con wrong_arg
                                  else foldr bmplus bmzero components

      -- (dom -> rng), where the head of ty is not a tuple tycon
      , ft_fun = \dom rng -> -- cf #8516
          if _ccdg1_hasParam dom
          then bmbad con wrong_arg
          else bmplus dom rng

      -- (ty arg), where head of ty is neither (->) nor a tuple constructor and
      -- the parameter of interest does not occur in ty
      , ft_ty_app = \_ arg -> arg

      , ft_bad_app = bmbad con wrong_arg
      , ft_forall  = \_ body -> body -- polytypes are handled elsewhere
      }
      where
        caseVar = CCDG1 True IsValid


    existential = text "must not have existential arguments"
    wrong_arg   = text "applies a type to an argument involving the last parameter"
               $$ text "but the applied type is not of kind * -> *"

{-
************************************************************************
*                                                                      *
\subsection{Generating the RHS of a generic default method}
*                                                                      *
************************************************************************
-}

type US = Int   -- Local unique supply, just a plain Int
type Alt = (LPat RdrName, LHsExpr RdrName)

-- GenericKind serves to mark if a datatype derives Generic (Gen0) or
-- Generic1 (Gen1).
data GenericKind = Gen0 | Gen1

-- as above, but with a payload of the TyCon's name for "the" parameter
data GenericKind_ = Gen0_ | Gen1_ TyVar

-- as above, but using a single datacon's name for "the" parameter
data GenericKind_DC = Gen0_DC | Gen1_DC TyVar

forgetArgVar :: GenericKind_DC -> GenericKind
forgetArgVar Gen0_DC   = Gen0
forgetArgVar Gen1_DC{} = Gen1

-- When working only within a single datacon, "the" parameter's name should
-- match that datacon's name for it.
gk2gkDC :: GenericKind_ -> DataCon -> GenericKind_DC
gk2gkDC Gen0_   _ = Gen0_DC
gk2gkDC Gen1_{} d = Gen1_DC $ last $ dataConUnivTyVars d


-- Bindings for the Generic instance
mkBindsRep :: GenericKind -> TyCon -> LHsBinds RdrName
mkBindsRep gk tycon =
    unitBag (mkRdrFunBind (L loc from01_RDR) [from_eqn])
  `unionBags`
    unitBag (mkRdrFunBind (L loc to01_RDR) [to_eqn])
      where
        -- The topmost M1 (the datatype metadata) has the exact same type
        -- across all cases of a from/to definition, and can be factored out
        -- to save some allocations during typechecking.
        -- See Note [Generics compilation speed tricks]
        from_eqn = mkHsCaseAlt x_Pat $ mkM1_E $ nlHsCase x_Expr from_matches
        to_eqn   = mkHsCaseAlt (mkM1_P x_Pat) $ nlHsCase x_Expr to_matches

        from_matches  = [mkHsCaseAlt pat rhs | (pat,rhs) <- from_alts]
        to_matches    = [mkHsCaseAlt pat rhs | (pat,rhs) <- to_alts  ]
        loc           = srcLocSpan (getSrcLoc tycon)
        datacons      = tyConDataCons tycon

        (from01_RDR, to01_RDR) = case gk of
                                   Gen0 -> (from_RDR,  to_RDR)
                                   Gen1 -> (from1_RDR, to1_RDR)

        -- Recurse over the sum first
        from_alts, to_alts :: [Alt]
        (from_alts, to_alts) = mkSum gk_ (1 :: US) tycon datacons
          where gk_ = case gk of
                  Gen0 -> Gen0_
                  Gen1 -> ASSERT(length tyvars >= 1)
                          Gen1_ (last tyvars)
                    where tyvars = tyConTyVars tycon

--------------------------------------------------------------------------------
-- The type synonym instance and synonym
--       type instance Rep (D a b) = Rep_D a b
--       type Rep_D a b = ...representation type for D ...
--------------------------------------------------------------------------------

tc_mkRepFamInsts :: GenericKind   -- Gen0 or Gen1
                 -> TyCon         -- The type to generate representation for
                 -> [Type]        -- The type(s) to which Generic(1) is applied
                                  -- in the generated instance
                 -> TcM FamInst   -- Generated representation0 coercion
tc_mkRepFamInsts gk tycon inst_tys =
       -- Consider the example input tycon `D`, where data D a b = D_ a
       -- Also consider `R:DInt`, where { data family D x y :: * -> *
       --                               ; data instance D Int a b = D_ a }
  do { -- `rep` = GHC.Generics.Rep or GHC.Generics.Rep1 (type family)
       fam_tc <- case gk of
         Gen0 -> tcLookupTyCon repTyConName
         Gen1 -> tcLookupTyCon rep1TyConName

     ; fam_envs <- tcGetFamInstEnvs

     ; let -- If the derived instance is
           --   instance Generic (Foo x)
           -- then:
           --   `arg_ki` = *, `inst_ty` = Foo x :: *
           --
           -- If the derived instance is
           --   instance Generic1 (Bar x :: k -> *)
           -- then:
           --   `arg_k` = k, `inst_ty` = Bar x :: k -> *
           (arg_ki, inst_ty) = case (gk, inst_tys) of
             (Gen0, [inst_t])        -> (liftedTypeKind, inst_t)
             (Gen1, [arg_k, inst_t]) -> (arg_k,          inst_t)
             _ -> pprPanic "tc_mkRepFamInsts" (ppr inst_tys)

     ; let mbFamInst         = tyConFamInst_maybe tycon
           -- If we're examining a data family instance, we grab the parent
           -- TyCon (ptc) and use it to determine the type arguments
           -- (inst_args) for the data family *instance*'s type variables.
           ptc               = maybe tycon fst mbFamInst
           (_, inst_args, _) = tcLookupDataFamInst fam_envs ptc $ snd
                                 $ tcSplitTyConApp inst_ty

     ; let -- `tyvars` = [a,b]
           (tyvars, gk_) = case gk of
             Gen0 -> (all_tyvars, Gen0_)
             Gen1 -> ASSERT(not $ null all_tyvars)
                     (init all_tyvars, Gen1_ $ last all_tyvars)
             where all_tyvars = tyConTyVars tycon

       -- `repTy` = D1 ... (C1 ... (S1 ... (Rec0 a))) :: * -> *
     ; repTy <- tc_mkRepTy gk_ tycon arg_ki

       -- `rep_name` is a name we generate for the synonym
     ; mod <- getModule
     ; loc <- getSrcSpanM
     ; let tc_occ  = nameOccName (tyConName tycon)
           rep_occ = case gk of Gen0 -> mkGenR tc_occ; Gen1 -> mkGen1R tc_occ
     ; rep_name <- newGlobalBinder mod rep_occ loc

       -- We make sure to substitute the tyvars with their user-supplied
       -- type arguments before generating the Rep/Rep1 instance, since some
       -- of the tyvars might have been instantiated when deriving.
       -- See Note [Generating a correctly typed Rep instance].
     ; let env        = zipTyEnv tyvars inst_args
           in_scope   = mkInScopeSet (tyCoVarsOfTypes inst_tys)
           subst      = mkTvSubst in_scope env
           repTy'     = substTy  subst repTy
           tcv'       = tyCoVarsOfTypeList inst_ty
           (tv', cv') = partition isTyVar tcv'
           tvs'       = toposortTyVars tv'
           cvs'       = toposortTyVars cv'
           axiom      = mkSingleCoAxiom Nominal rep_name tvs' cvs'
                                        fam_tc inst_tys repTy'

     ; newFamInst SynFamilyInst axiom  }

--------------------------------------------------------------------------------
-- Type representation
--------------------------------------------------------------------------------

-- | See documentation of 'argTyFold'; that function uses the fields of this
-- type to interpret the structure of a type when that type is considered as an
-- argument to a constructor that is being represented with 'Rep1'.
data ArgTyAlg a = ArgTyAlg
  { ata_rec0 :: (Type -> a)
  , ata_par1 :: a, ata_rec1 :: (Type -> a)
  , ata_comp :: (Type -> a -> a)
  }

-- | @argTyFold@ implements a generalised and safer variant of the @arg@
-- function from Figure 3 in <http://dreixel.net/research/pdf/gdmh.pdf>. @arg@
-- is conceptually equivalent to:
--
-- > arg t = case t of
-- >   _ | isTyVar t         -> if (t == argVar) then Par1 else Par0 t
-- >   App f [t'] |
-- >     representable1 f &&
-- >     t' == argVar        -> Rec1 f
-- >   App f [t'] |
-- >     representable1 f &&
-- >     t' has tyvars       -> f :.: (arg t')
-- >   _                     -> Rec0 t
--
-- where @argVar@ is the last type variable in the data type declaration we are
-- finding the representation for.
--
-- @argTyFold@ is more general than @arg@ because it uses 'ArgTyAlg' to
-- abstract out the concrete invocations of @Par0@, @Rec0@, @Par1@, @Rec1@, and
-- @:.:@.
--
-- @argTyFold@ is safer than @arg@ because @arg@ would lead to a GHC panic for
-- some data types. The problematic case is when @t@ is an application of a
-- non-representable type @f@ to @argVar@: @App f [argVar]@ is caught by the
-- @_@ pattern, and ends up represented as @Rec0 t@. This type occurs /free/ in
-- the RHS of the eventual @Rep1@ instance, which is therefore ill-formed. Some
-- representable1 checks have been relaxed, and others were moved to
-- @canDoGenerics1@.
argTyFold :: forall a. TyVar -> ArgTyAlg a -> Type -> a
argTyFold argVar (ArgTyAlg {ata_rec0 = mkRec0,
                            ata_par1 = mkPar1, ata_rec1 = mkRec1,
                            ata_comp = mkComp}) =
  -- mkRec0 is the default; use it if there is no interesting structure
  -- (e.g. occurrences of parameters or recursive occurrences)
  \t -> maybe (mkRec0 t) id $ go t where
  go :: Type -> -- type to fold through
        Maybe a -- the result (e.g. representation type), unless it's trivial
  go t = isParam `mplus` isApp where

    isParam = do -- handles parameters
      t' <- getTyVar_maybe t
      Just $ if t' == argVar then mkPar1 -- moreover, it is "the" parameter
             else mkRec0 t -- NB mkRec0 instead of the conventional mkPar0

    isApp = do -- handles applications
      (phi, beta) <- tcSplitAppTy_maybe t

      let interesting = argVar `elemVarSet` exactTyCoVarsOfType beta

      -- Does it have no interesting structure to represent?
      if not interesting then Nothing
        else -- Is the argument the parameter? Special case for mkRec1.
          if Just argVar == getTyVar_maybe beta then Just $ mkRec1 phi
            else mkComp phi `fmap` go beta -- It must be a composition.


tc_mkRepTy ::  -- Gen0_ or Gen1_, for Rep or Rep1
               GenericKind_
              -- The type to generate representation for
            -> TyCon
              -- The kind of the representation type's argument
              -- See Note [Handling kinds in a Rep instance]
            -> Kind
               -- Generated representation0 type
            -> TcM Type
tc_mkRepTy gk_ tycon k =
  do
    d1      <- tcLookupTyCon d1TyConName
    c1      <- tcLookupTyCon c1TyConName
    s1      <- tcLookupTyCon s1TyConName
    rec0    <- tcLookupTyCon rec0TyConName
    rec1    <- tcLookupTyCon rec1TyConName
    par1    <- tcLookupTyCon par1TyConName
    u1      <- tcLookupTyCon u1TyConName
    v1      <- tcLookupTyCon v1TyConName
    plus    <- tcLookupTyCon sumTyConName
    times   <- tcLookupTyCon prodTyConName
    comp    <- tcLookupTyCon compTyConName
    uAddr   <- tcLookupTyCon uAddrTyConName
    uChar   <- tcLookupTyCon uCharTyConName
    uDouble <- tcLookupTyCon uDoubleTyConName
    uFloat  <- tcLookupTyCon uFloatTyConName
    uInt    <- tcLookupTyCon uIntTyConName
    uWord   <- tcLookupTyCon uWordTyConName

    let tcLookupPromDataCon = fmap promoteDataCon . tcLookupDataCon

    md         <- tcLookupPromDataCon metaDataDataConName
    mc         <- tcLookupPromDataCon metaConsDataConName
    ms         <- tcLookupPromDataCon metaSelDataConName
    pPrefix    <- tcLookupPromDataCon prefixIDataConName
    pInfix     <- tcLookupPromDataCon infixIDataConName
    pLA        <- tcLookupPromDataCon leftAssociativeDataConName
    pRA        <- tcLookupPromDataCon rightAssociativeDataConName
    pNA        <- tcLookupPromDataCon notAssociativeDataConName
    pSUpk      <- tcLookupPromDataCon sourceUnpackDataConName
    pSNUpk     <- tcLookupPromDataCon sourceNoUnpackDataConName
    pNSUpkness <- tcLookupPromDataCon noSourceUnpackednessDataConName
    pSLzy      <- tcLookupPromDataCon sourceLazyDataConName
    pSStr      <- tcLookupPromDataCon sourceStrictDataConName
    pNSStrness <- tcLookupPromDataCon noSourceStrictnessDataConName
    pDLzy      <- tcLookupPromDataCon decidedLazyDataConName
    pDStr      <- tcLookupPromDataCon decidedStrictDataConName
    pDUpk      <- tcLookupPromDataCon decidedUnpackDataConName

    fix_env <- getFixityEnv

    let mkSum' a b = mkTyConApp plus  [k,a,b]
        mkProd a b = mkTyConApp times [k,a,b]
        mkRec0 a   = mkBoxTy uAddr uChar uDouble uFloat uInt uWord rec0 k a
        mkRec1 a   = mkTyConApp rec1  [k,a]
        mkPar1     = mkTyConTy  par1
        mkD    a   = mkTyConApp d1 [ k, metaDataTy, sumP (tyConDataCons a) ]
        mkC      a = mkTyConApp c1 [ k
                                   , metaConsTy a
                                   , prod (dataConInstOrigArgTys a
                                            . mkTyVarTys . tyConTyVars $ tycon)
                                          (dataConSrcBangs    a)
                                          (dataConImplBangs   a)
                                          (dataConFieldLabels a)]
        mkS mlbl su ss ib a = mkTyConApp s1 [k, metaSelTy mlbl su ss ib, a]

        -- Sums and products are done in the same way for both Rep and Rep1
        sumP [] = mkTyConApp v1 [k]
        sumP l  = foldBal mkSum' . map mkC  $ l
        -- The Bool is True if this constructor has labelled fields
        prod :: [Type] -> [HsSrcBang] -> [HsImplBang] -> [FieldLabel] -> Type
        prod [] _  _  _  = mkTyConApp u1 [k]
        prod l  sb ib fl = foldBal mkProd
                                   [ ASSERT(null fl || length fl > j)
                                     arg t sb' ib' (if null fl
                                                       then Nothing
                                                       else Just (fl !! j))
                                   | (t,sb',ib',j) <- zip4 l sb ib [0..] ]

        arg :: Type -> HsSrcBang -> HsImplBang -> Maybe FieldLabel -> Type
        arg t (HsSrcBang _ su ss) ib fl = mkS fl su ss ib $ case gk_ of
            -- Here we previously used Par0 if t was a type variable, but we
            -- realized that we can't always guarantee that we are wrapping-up
            -- all type variables in Par0. So we decided to stop using Par0
            -- altogether, and use Rec0 all the time.
                      Gen0_        -> mkRec0 t
                      Gen1_ argVar -> argPar argVar t
          where
            -- Builds argument representation for Rep1 (more complicated due to
            -- the presence of composition).
            argPar argVar = argTyFold argVar $ ArgTyAlg
              {ata_rec0 = mkRec0, ata_par1 = mkPar1,
               ata_rec1 = mkRec1, ata_comp = mkComp comp k}

        tyConName_user = case tyConFamInst_maybe tycon of
                           Just (ptycon, _) -> tyConName ptycon
                           Nothing          -> tyConName tycon

        dtName  = mkStrLitTy . occNameFS . nameOccName $ tyConName_user
        mdName  = mkStrLitTy . moduleNameFS . moduleName
                . nameModule . tyConName $ tycon
        pkgName = mkStrLitTy . unitIdFS . moduleUnitId
                . nameModule . tyConName $ tycon
        isNT    = mkTyConTy $ if isNewTyCon tycon
                              then promotedTrueDataCon
                              else promotedFalseDataCon

        ctName = mkStrLitTy . occNameFS . nameOccName . dataConName
        ctFix c
            | dataConIsInfix c
            = case lookupFixity fix_env (dataConName c) of
                   Fixity _ n InfixL -> buildFix n pLA
                   Fixity _ n InfixR -> buildFix n pRA
                   Fixity _ n InfixN -> buildFix n pNA
            | otherwise = mkTyConTy pPrefix
        buildFix n assoc = mkTyConApp pInfix [ mkTyConTy assoc
                                             , mkNumLitTy (fromIntegral n)]

        isRec c = mkTyConTy $ if length (dataConFieldLabels c) > 0
                              then promotedTrueDataCon
                              else promotedFalseDataCon

        selName = mkStrLitTy . flLabel

        mbSel Nothing  = mkTyConApp promotedNothingDataCon [typeSymbolKind]
        mbSel (Just s) = mkTyConApp promotedJustDataCon
                                    [typeSymbolKind, selName s]

        metaDataTy   = mkTyConApp md [dtName, mdName, pkgName, isNT]
        metaConsTy c = mkTyConApp mc [ctName c, ctFix c, isRec c]
        metaSelTy mlbl su ss ib =
            mkTyConApp ms [mbSel mlbl, pSUpkness, pSStrness, pDStrness]
          where
            pSUpkness = mkTyConTy $ case su of
                                         SrcUnpack   -> pSUpk
                                         SrcNoUnpack -> pSNUpk
                                         NoSrcUnpack -> pNSUpkness

            pSStrness = mkTyConTy $ case ss of
                                         SrcLazy     -> pSLzy
                                         SrcStrict   -> pSStr
                                         NoSrcStrict -> pNSStrness

            pDStrness = mkTyConTy $ case ib of
                                         HsLazy      -> pDLzy
                                         HsStrict    -> pDStr
                                         HsUnpack{}  -> pDUpk

    return (mkD tycon)

mkComp :: TyCon -> Kind -> Type -> Type -> Type
mkComp comp k f g
  | k1_first  = mkTyConApp comp  [k,liftedTypeKind,f,g]
  | otherwise = mkTyConApp comp  [liftedTypeKind,k,f,g]
  where
    -- Which of these is the case?
    --     newtype (:.:) {k1} {k2} (f :: k2->*) (g :: k1->k2) (p :: k1) = ...
    -- or  newtype (:.:) {k2} {k1} (f :: k2->*) (g :: k1->k2) (p :: k1) = ...
    -- We want to instantiate with k1=k, and k2=*
    --    Reason for k2=*: see Note [Handling kinds in a Rep instance]
    -- But we need to know which way round!
    k1_first = k_first == p_kind_var
    [k_first,_,_,_,p] = tyConTyVars comp
    Just p_kind_var = getTyVar_maybe (tyVarKind p)

-- Given the TyCons for each URec-related type synonym, check to see if the
-- given type is an unlifted type that generics understands. If so, return
-- its representation type. Otherwise, return Rec0.
-- See Note [Generics and unlifted types]
mkBoxTy :: TyCon -- UAddr
        -> TyCon -- UChar
        -> TyCon -- UDouble
        -> TyCon -- UFloat
        -> TyCon -- UInt
        -> TyCon -- UWord
        -> TyCon -- Rec0
        -> Kind  -- What to instantiate Rec0's kind variable with
        -> Type
        -> Type
mkBoxTy uAddr uChar uDouble uFloat uInt uWord rec0 k ty
  | ty `eqType` addrPrimTy   = mkTyConApp uAddr   [k]
  | ty `eqType` charPrimTy   = mkTyConApp uChar   [k]
  | ty `eqType` doublePrimTy = mkTyConApp uDouble [k]
  | ty `eqType` floatPrimTy  = mkTyConApp uFloat  [k]
  | ty `eqType` intPrimTy    = mkTyConApp uInt    [k]
  | ty `eqType` wordPrimTy   = mkTyConApp uWord   [k]
  | otherwise                = mkTyConApp rec0    [k,ty]

--------------------------------------------------------------------------------
-- Dealing with sums
--------------------------------------------------------------------------------

mkSum :: GenericKind_ -- Generic or Generic1?
      -> US          -- Base for generating unique names
      -> TyCon       -- The type constructor
      -> [DataCon]   -- The data constructors
      -> ([Alt],     -- Alternatives for the T->Trep "from" function
          [Alt])     -- Alternatives for the Trep->T "to" function

-- Datatype without any constructors
mkSum _ _ tycon [] = ([from_alt], [to_alt])
  where
    from_alt = (nlWildPat, makeError errMsgFrom)
    to_alt   = (nlWildPat, makeError errMsgTo)
               -- These M1s are meta-information for the datatype
    makeError s = nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString s))
    tyConStr   = occNameString (nameOccName (tyConName tycon))
    errMsgFrom = "No generic representation for empty datatype " ++ tyConStr
    errMsgTo   = "No values for empty datatype " ++ tyConStr

-- Datatype with at least one constructor
mkSum gk_ us _ datacons =
  -- switch the payload of gk_ to be datacon-centric instead of tycon-centric
 unzip [ mk1Sum (gk2gkDC gk_ d) us i (length datacons) d
           | (d,i) <- zip datacons [1..] ]

-- Build the sum for a particular constructor
mk1Sum :: GenericKind_DC -- Generic or Generic1?
       -> US        -- Base for generating unique names
       -> Int       -- The index of this constructor
       -> Int       -- Total number of constructors
       -> DataCon   -- The data constructor
       -> (Alt,     -- Alternative for the T->Trep "from" function
           Alt)     -- Alternative for the Trep->T "to" function
mk1Sum gk_ us i n datacon = (from_alt, to_alt)
  where
    gk = forgetArgVar gk_

    -- Existentials already excluded
    argTys = dataConOrigArgTys datacon
    n_args = dataConSourceArity datacon

    datacon_varTys = zip (map mkGenericLocal [us .. us+n_args-1]) argTys
    datacon_vars = map fst datacon_varTys
    us'          = us + n_args

    datacon_rdr  = getRdrName datacon

    from_alt     = (nlConVarPat datacon_rdr datacon_vars, from_alt_rhs)
    from_alt_rhs = genLR_E i n (mkProd_E gk_ us' datacon_varTys)

    to_alt     = ( genLR_P i n (mkProd_P gk us' datacon_varTys)
                 , to_alt_rhs
                 ) -- These M1s are meta-information for the datatype
    to_alt_rhs = case gk_ of
      Gen0_DC        -> nlHsVarApps datacon_rdr datacon_vars
      Gen1_DC argVar -> nlHsApps datacon_rdr $ map argTo datacon_varTys
        where
          argTo (var, ty) = converter ty `nlHsApp` nlHsVar var where
            converter = argTyFold argVar $ ArgTyAlg
              {ata_rec0 = nlHsVar . unboxRepRDR,
               ata_par1 = nlHsVar unPar1_RDR,
               ata_rec1 = const $ nlHsVar unRec1_RDR,
               ata_comp = \_ cnv -> (nlHsVar fmap_RDR `nlHsApp` cnv)
                                    `nlHsCompose` nlHsVar unComp1_RDR}


-- Generates the L1/R1 sum pattern
genLR_P :: Int -> Int -> LPat RdrName -> LPat RdrName
genLR_P i n p
  | n == 0       = error "impossible"
  | n == 1       = p
  | i <= div n 2 = nlParPat $ nlConPat l1DataCon_RDR [genLR_P i     (div n 2) p]
  | otherwise    = nlParPat $ nlConPat r1DataCon_RDR [genLR_P (i-m) (n-m)     p]
                     where m = div n 2

-- Generates the L1/R1 sum expression
genLR_E :: Int -> Int -> LHsExpr RdrName -> LHsExpr RdrName
genLR_E i n e
  | n == 0       = error "impossible"
  | n == 1       = e
  | i <= div n 2 = nlHsVar l1DataCon_RDR `nlHsApp` genLR_E i     (div n 2) e
  | otherwise    = nlHsVar r1DataCon_RDR `nlHsApp` genLR_E (i-m) (n-m)     e
                     where m = div n 2

--------------------------------------------------------------------------------
-- Dealing with products
--------------------------------------------------------------------------------

-- Build a product expression
mkProd_E :: GenericKind_DC    -- Generic or Generic1?
         -> US                -- Base for unique names
         -> [(RdrName, Type)] -- List of variables matched on the lhs and their types
         -> LHsExpr RdrName   -- Resulting product expression
mkProd_E _   _ []     = mkM1_E (nlHsVar u1DataCon_RDR)
mkProd_E gk_ _ varTys = mkM1_E (foldBal prod appVars)
                     -- These M1s are meta-information for the constructor
  where
    appVars = map (wrapArg_E gk_) varTys
    prod a b = prodDataCon_RDR `nlHsApps` [a,b]

wrapArg_E :: GenericKind_DC -> (RdrName, Type) -> LHsExpr RdrName
wrapArg_E Gen0_DC          (var, ty) = mkM1_E $
                            boxRepRDR ty `nlHsVarApps` [var]
                         -- This M1 is meta-information for the selector
wrapArg_E (Gen1_DC argVar) (var, ty) = mkM1_E $
                            converter ty `nlHsApp` nlHsVar var
                         -- This M1 is meta-information for the selector
  where converter = argTyFold argVar $ ArgTyAlg
          {ata_rec0 = nlHsVar . boxRepRDR,
           ata_par1 = nlHsVar par1DataCon_RDR,
           ata_rec1 = const $ nlHsVar rec1DataCon_RDR,
           ata_comp = \_ cnv -> nlHsVar comp1DataCon_RDR `nlHsCompose`
                                  (nlHsVar fmap_RDR `nlHsApp` cnv)}

boxRepRDR :: Type -> RdrName
boxRepRDR = maybe k1DataCon_RDR fst . unboxedRepRDRs

unboxRepRDR :: Type -> RdrName
unboxRepRDR = maybe unK1_RDR snd . unboxedRepRDRs

-- Retrieve the RDRs associated with each URec data family instance
-- constructor. See Note [Generics and unlifted types]
unboxedRepRDRs :: Type -> Maybe (RdrName, RdrName)
unboxedRepRDRs ty
  | ty `eqType` addrPrimTy   = Just (uAddrDataCon_RDR,   uAddrHash_RDR)
  | ty `eqType` charPrimTy   = Just (uCharDataCon_RDR,   uCharHash_RDR)
  | ty `eqType` doublePrimTy = Just (uDoubleDataCon_RDR, uDoubleHash_RDR)
  | ty `eqType` floatPrimTy  = Just (uFloatDataCon_RDR,  uFloatHash_RDR)
  | ty `eqType` intPrimTy    = Just (uIntDataCon_RDR,    uIntHash_RDR)
  | ty `eqType` wordPrimTy   = Just (uWordDataCon_RDR,   uWordHash_RDR)
  | otherwise          = Nothing

-- Build a product pattern
mkProd_P :: GenericKind       -- Gen0 or Gen1
         -> US                -- Base for unique names
         -> [(RdrName, Type)] -- List of variables to match,
                              --   along with their types
         -> LPat RdrName      -- Resulting product pattern
mkProd_P _  _ []     = mkM1_P (nlNullaryConPat u1DataCon_RDR)
mkProd_P gk _ varTys = mkM1_P (foldBal prod appVars)
                     -- These M1s are meta-information for the constructor
  where
    appVars = unzipWith (wrapArg_P gk) varTys
    prod a b = nlParPat $ prodDataCon_RDR `nlConPat` [a,b]

wrapArg_P :: GenericKind -> RdrName -> Type -> LPat RdrName
wrapArg_P Gen0 v ty = mkM1_P (nlParPat $ boxRepRDR ty `nlConVarPat` [v])
                   -- This M1 is meta-information for the selector
wrapArg_P Gen1 v _  = nlParPat $ m1DataCon_RDR `nlConVarPat` [v]

mkGenericLocal :: US -> RdrName
mkGenericLocal u = mkVarUnqual (mkFastString ("g" ++ show u))

x_RDR :: RdrName
x_RDR = mkVarUnqual (fsLit "x")

x_Expr :: LHsExpr RdrName
x_Expr = nlHsVar x_RDR

x_Pat :: LPat RdrName
x_Pat = nlVarPat x_RDR

mkM1_E :: LHsExpr RdrName -> LHsExpr RdrName
mkM1_E e = nlHsVar m1DataCon_RDR `nlHsApp` e

mkM1_P :: LPat RdrName -> LPat RdrName
mkM1_P p = nlParPat $ m1DataCon_RDR `nlConPat` [p]

nlHsCompose :: LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
nlHsCompose x y = compose_RDR `nlHsApps` [x, y]

-- | Variant of foldr1 for producing balanced lists
foldBal :: (a -> a -> a) -> [a] -> a
foldBal op = foldBal' op (error "foldBal: empty list")

foldBal' :: (a -> a -> a) -> a -> [a] -> a
foldBal' _  x []  = x
foldBal' _  _ [y] = y
foldBal' op x l   = let (a,b) = splitAt (length l `div` 2) l
                    in foldBal' op x a `op` foldBal' op x b

{-
Note [Generics and unlifted types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, all constants are marked with K1/Rec0. The exception to this rule is
when a data constructor has an unlifted argument (e.g., Int#, Char#, etc.). In
that case, we must use a data family instance of URec (from GHC.Generics) to
mark it. As a result, before we can generate K1 or unK1, we must first check
to see if the type is actually one of the unlifted types for which URec has a
data family instance; if so, we generate that instead.

See wiki:Commentary/Compiler/GenericDeriving#Handlingunliftedtypes for more
details on why URec is implemented the way it is.

Note [Generating a correctly typed Rep instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tc_mkRepTy derives the RHS of the Rep(1) type family instance when deriving
Generic(1). That is, it derives the ellipsis in the following:

    instance Generic Foo where
      type Rep Foo = ...

However, tc_mkRepTy only has knowledge of the *TyCon* of the type for which
a Generic(1) instance is being derived, not the fully instantiated type. As a
result, tc_mkRepTy builds the most generalized Rep(1) instance possible using
the type variables it learns from the TyCon (i.e., it uses tyConTyVars). This
can cause problems when the instance has instantiated type variables
(see Trac #11732). As an example:

    data T a = MkT a
    deriving instance Generic (T Int)
    ==>
    instance Generic (T Int) where
      type Rep (T Int) = (... (Rec0 a)) -- wrong!

-XStandaloneDeriving is one way for the type variables to become instantiated.
Another way is when Generic1 is being derived for a datatype with a visible
kind binder, e.g.,

   data P k (a :: k) = MkP k deriving Generic1
   ==>
   instance Generic1 (P *) where
     type Rep1 (P *) = (... (Rec0 k)) -- wrong!

See Note [Unify kinds in deriving] in TcDeriv.

In any such scenario, we must prevent a discrepancy between the LHS and RHS of
a Rep(1) instance. To do so, we create a type variable substitution that maps
the tyConTyVars of the TyCon to their counterparts in the fully instantiated
type. (For example, using T above as example, you'd map a :-> Int.) We then
apply the substitution to the RHS before generating the instance.

Note [Handling kinds in a Rep instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because Generic1 is poly-kinded, the representation types were generalized to
be kind-polymorphic as well. As a result, tc_mkRepTy must explicitly apply
the kind of the instance being derived to all the representation type
constructors. For instance, if you have

    data Empty (a :: k) = Empty deriving Generic1

Then the generated code is now approximately (with -fprint-explicit-kinds
syntax):

    instance Generic1 k (Empty k) where
      type Rep1 k (Empty k) = U1 k

Most representation types have only one kind variable, making them easy to deal
with. The only non-trivial case is (:.:), which is only used in Generic1
instances:

    newtype (:.:) (f :: k2 -> *) (g :: k1 -> k2) (p :: k1) =
        Comp1 { unComp1 :: f (g p) }

Here, we do something a bit counter-intuitive: we make k1 be the kind of the
instance being derived, and we always make k2 be *. Why *? It's because
the code that GHC generates using (:.:) is always of the form x :.: Rec1 y
for some types x and y. In other words, the second type to which (:.:) is
applied always has kind k -> *, for some kind k, so k2 cannot possibly be
anything other than * in a generated Generic1 instance.

Note [Generics compilation speed tricks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deriving Generic(1) is known to have a large constant factor during
compilation, which contributes to noticeable compilation slowdowns when
deriving Generic(1) for large datatypes (see Trac #5642).

To ease the pain, there is a trick one can play when generating definitions for
to(1) and from(1). If you have a datatype like:

  data Letter = A | B | C | D

then a naïve Generic instance for Letter would be:

  instance Generic Letter where
    type Rep Letter = D1 ('MetaData ...) ...

    to (M1 (L1 (L1 (M1 U1)))) = A
    to (M1 (L1 (R1 (M1 U1)))) = B
    to (M1 (R1 (L1 (M1 U1)))) = C
    to (M1 (R1 (R1 (M1 U1)))) = D

    from A = M1 (L1 (L1 (M1 U1)))
    from B = M1 (L1 (R1 (M1 U1)))
    from C = M1 (R1 (L1 (M1 U1)))
    from D = M1 (R1 (R1 (M1 U1)))

Notice that in every LHS pattern-match of the 'to' definition, and in every RHS
expression in the 'from' definition, the topmost constructor is M1. This
corresponds to the datatype-specific metadata (the D1 in the Rep Letter
instance). But this is wasteful from a typechecking perspective, since this
definition requires GHC to typecheck an application of M1 in every single case,
leading to an O(n) increase in the number of coercions the typechecker has to
solve, which in turn increases allocations and degrades compilation speed.

Luckily, since the topmost M1 has the exact same type across every case, we can
factor it out reduce the typechecker's burden:

  instance Generic Letter where
    type Rep Letter = D1 ('MetaData ...) ...

    to (M1 x) = case x of
      L1 (L1 (M1 U1)) -> A
      L1 (R1 (M1 U1)) -> B
      R1 (L1 (M1 U1)) -> C
      R1 (R1 (M1 U1)) -> D

    from x = M1 (case x of
      A -> L1 (L1 (M1 U1))
      B -> L1 (R1 (M1 U1))
      C -> R1 (L1 (M1 U1))
      D -> R1 (R1 (M1 U1)))

A simple change, but one that pays off, since it goes turns an O(n) amount of
coercions to an O(1) amount.
-}
