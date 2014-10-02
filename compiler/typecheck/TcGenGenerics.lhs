%
% (c) The University of Glasgow 2011
%

The deriving code for the Generic class
(equivalent to the code in TcGenDeriv, for other classes)

\begin{code}
{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TcGenGenerics (canDoGenerics, canDoGenerics1,
                      GenericKind(..),
                      gen_Generic_binds, get_gen1_constrained_tys) where

import HsSyn
import Type
import Kind             ( isKind )
import TcType
import TcGenDeriv
import DataCon
import TyCon
import FamInstEnv       ( FamInst, FamFlavor(..), mkSingleCoAxiom )
import FamInst
import Module           ( Module, moduleName, moduleNameFS )
import IfaceEnv         ( newGlobalBinder )
import Name      hiding ( varName )
import NameEnv ( lookupNameEnv )
import RdrName
import BasicTypes
import TysWiredIn
import PrelNames
import TcEnv
import TcRnMonad
import HscTypes
import ErrUtils( Validity(..), andValid )
import SrcLoc
import Bag
import VarSet (elemVarSet)
import Outputable
import FastString
import Util

import Control.Monad ( mplus )

#include "HsVersions.h"
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Bindings for the new generic deriving mechanism}
%*                                                                      *
%************************************************************************

For the generic representation we need to generate:
\begin{itemize}
\item A Generic instance
\item A Rep type instance
\item Many auxiliary datatypes and instances for them (for the meta-information)
\end{itemize}

\begin{code}
gen_Generic_binds :: GenericKind -> TyCon -> Module
                 -> TcM (LHsBinds RdrName, FamInst)
gen_Generic_binds gk tc mod = do
  repTyInsts <- tc_mkRepFamInsts gk tc mod
  return (mkBindsRep gk tc, repTyInsts)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Generating representation types}
%*                                                                      *
%************************************************************************

\begin{code}
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

  (a) T = (D v1 ... vn) with free variables v1, v2, ..., vn where n >= 0 v1
      ... vn are distinct type variables. Cf #5939.

  (b) D is a type constructor *value*. In other words, D is either a type
      constructor or it is equivalent to the head of a data family instance (up to
      alpha-renaming).

  (c) D cannot have a "stupid context".

  (d) The right-hand side of D cannot include unboxed types, existential types,
      or universally quantified types.

  (e) T :: *.

(Generic1 T) and (Rep1 T) are derivable for some type expression T if the
following constraints are satisfied.

  (a),(b),(c),(d) As above.

  (f) T must expect arguments, and its last parameter must have kind *.

      We use `a' to denote the parameter of D that corresponds to the last
      parameter of T.

  (g) For any type-level application (Tfun Targ) in the right-hand side of D
      where the head of Tfun is not a tuple constructor:

      (b1) `a' must not occur in Tfun.

      (b2) If `a' occurs in Targ, then Tfun :: * -> *.

-}

canDoGenerics :: TyCon -> [Type] -> Validity
-- canDoGenerics rep_tc tc_args determines if Generic/Rep can be derived for a
-- type expression (rep_tc tc_arg0 tc_arg1 ... tc_argn).
--
-- Check (b) from Note [Requirements for deriving Generic and Rep] is taken
-- care of because canDoGenerics is applied to rep tycons.
--
-- It returns Nothing if deriving is possible. It returns (Just reason) if not.
canDoGenerics tc tc_args
  = mergeErrors (
          -- Check (c) from Note [Requirements for deriving Generic and Rep].
              (if (not (null (tyConStupidTheta tc)))
                then (NotValid (tc_name <+> text "must not have a datatype context"))
                else IsValid) :
          -- Check (a) from Note [Requirements for deriving Generic and Rep].
          --
          -- Data family indices can be instantiated; the `tc_args` here are
          -- the representation tycon args
              (if (all isTyVarTy (filterOut isKind tc_args))
                then IsValid
                else NotValid (tc_name <+> text "must not be instantiated;" <+>
                               text "try deriving `" <> tc_name <+> tc_tys <>
                               text "' instead"))
          -- See comment below
            : (map bad_con (tyConDataCons tc)))
  where
    -- The tc can be a representation tycon. When we want to display it to the
    -- user (in an error message) we should print its parent
    (tc_name, tc_tys) = case tyConParent tc of
        FamInstTyCon _ ptc tys -> (ppr ptc, hsep (map ppr
                                            (tys ++ drop (length tys) tc_args)))
        _                      -> (ppr tc, hsep (map ppr (tyConTyVars tc)))

        -- Check (d) from Note [Requirements for deriving Generic and Rep].
        --
        -- If any of the constructors has an unboxed type as argument,
        -- then we can't build the embedding-projection pair, because
        -- it relies on instantiating *polymorphic* sum and product types
        -- at the argument types of the constructors
    bad_con dc = if (any bad_arg_type (dataConOrigArgTys dc))
                  then (NotValid (ppr dc <+> text "must not have unlifted or polymorphic arguments"))
                  else (if (not (isVanillaDataCon dc))
                          then (NotValid (ppr dc <+> text "must be a vanilla data constructor"))
                          else IsValid)

        -- Nor can we do the job if it's an existential data constructor,
        -- Nor if the args are polymorphic types (I don't think)
    bad_arg_type ty = isUnLiftedType ty || not (isTauTy ty)

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

-- canDoGenerics1 rep_tc tc_args determines if a Generic1/Rep1 can be derived
-- for a type expression (rep_tc tc_arg0 tc_arg1 ... tc_argn).
--
-- Checks (a) through (d) from Note [Requirements for deriving Generic and Rep]
-- are taken care of by the call to canDoGenerics.
--
-- It returns Nothing if deriving is possible. It returns (Just reason) if not.
canDoGenerics1 :: TyCon -> [Type] -> Validity
canDoGenerics1 rep_tc tc_args =
  canDoGenerics rep_tc tc_args `andValid` additionalChecks
  where
    additionalChecks
        -- check (f) from Note [Requirements for deriving Generic and Rep]
      | null (tyConTyVars rep_tc) = NotValid $
          ptext (sLit "Data type") <+> quotes (ppr rep_tc)
      <+> ptext (sLit "must have some type parameters")

      | otherwise = mergeErrors $ concatMap check_con data_cons

    data_cons = tyConDataCons rep_tc
    check_con con = case check_vanilla con of
      j@(NotValid {}) -> [j]
      IsValid -> _ccdg1_errors `map` foldDataConArgs (ft_check con) con

    bad :: DataCon -> SDoc -> SDoc
    bad con msg = ptext (sLit "Constructor") <+> quotes (ppr con) <+> msg

    check_vanilla :: DataCon -> Validity
    check_vanilla con | isVanillaDataCon con = IsValid
                      | otherwise            = NotValid (bad con existential)

    bmzero      = CCDG1 False IsValid
    bmbad con s = CCDG1 True $ NotValid $ bad con s
    bmplus (CCDG1 b1 m1) (CCDG1 b2 m2) = CCDG1 (b1 || b2) (m1 `andValid` m2)

    -- check (g) from Note [degenerate use of FFoldType]
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

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Generating the RHS of a generic default method}
%*                                                                      *
%************************************************************************

\begin{code}
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
    unitBag (mkRdrFunBind (L loc from01_RDR) from_matches)
  `unionBags`
    unitBag (mkRdrFunBind (L loc to01_RDR) to_matches)
      where
        from_matches  = [mkSimpleHsAlt pat rhs | (pat,rhs) <- from_alts]
        to_matches    = [mkSimpleHsAlt pat rhs | (pat,rhs) <- to_alts  ]
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

tc_mkRepFamInsts :: GenericKind     -- Gen0 or Gen1
               -> TyCon           -- The type to generate representation for
               -> Module          -- Used as the location of the new RepTy
               -> TcM (FamInst)   -- Generated representation0 coercion
tc_mkRepFamInsts gk tycon mod =
       -- Consider the example input tycon `D`, where data D a b = D_ a
       -- Also consider `R:DInt`, where { data family D x y :: * -> *
       --                               ; data instance D Int a b = D_ a }
  do { -- `rep` = GHC.Generics.Rep or GHC.Generics.Rep1 (type family)
       fam_tc <- case gk of
         Gen0 -> tcLookupTyCon repTyConName
         Gen1 -> tcLookupTyCon rep1TyConName

     ; let -- `tyvars` = [a,b]
           (tyvars, gk_) = case gk of
             Gen0 -> (all_tyvars, Gen0_)
             Gen1 -> ASSERT(not $ null all_tyvars)
                     (init all_tyvars, Gen1_ $ last all_tyvars)
             where all_tyvars = tyConTyVars tycon

           tyvar_args = mkTyVarTys tyvars

           appT :: [Type]
           appT = case tyConFamInst_maybe tycon of
                     -- `appT` = D Int a b (data families case)
                     Just (famtycon, apps) ->
                       -- `fam` = D
                       -- `apps` = [Int, a, b]
                       let allApps = case gk of
                                       Gen0 -> apps
                                       Gen1 -> ASSERT(not $ null apps)
                                               init apps
                       in [mkTyConApp famtycon allApps]
                     -- `appT` = D a b (normal case)
                     Nothing -> [mkTyConApp tycon tyvar_args]

       -- `repTy` = D1 ... (C1 ... (S1 ... (Rec0 a))) :: * -> *
     ; repTy <- tc_mkRepTy gk_ tycon

       -- `rep_name` is a name we generate for the synonym
     ; rep_name <- let mkGen = case gk of Gen0 -> mkGenR; Gen1 -> mkGen1R
                   in newGlobalBinder mod (mkGen (nameOccName (tyConName tycon)))
                        (nameSrcSpan (tyConName tycon))

     ; let axiom = mkSingleCoAxiom rep_name tyvars fam_tc appT repTy
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

      let interesting = argVar `elemVarSet` exactTyVarsOfType beta

      -- Does it have no interesting structure to represent?
      if not interesting then Nothing
        else -- Is the argument the parameter? Special case for mkRec1.
          if Just argVar == getTyVar_maybe beta then Just $ mkRec1 phi
            else mkComp phi `fmap` go beta -- It must be a composition.


tc_mkRepTy ::  -- Gen0_ or Gen1_, for Rep or Rep1
               GenericKind_
              -- The type to generate representation for
            -> TyCon
               -- Generated representation0 type
            -> TcM Type
tc_mkRepTy gk_ tycon =
  do
    d1    <- tcLookupTyCon d1TyConName
    c1    <- tcLookupTyCon c1TyConName
    s1    <- tcLookupTyCon s1TyConName
    rec0  <- tcLookupTyCon rec0TyConName
    rec1  <- tcLookupTyCon rec1TyConName
    par1  <- tcLookupTyCon par1TyConName
    u1    <- tcLookupTyCon u1TyConName
    v1    <- tcLookupTyCon v1TyConName
    plus  <- tcLookupTyCon sumTyConName
    times <- tcLookupTyCon prodTyConName
    comp  <- tcLookupTyCon compTyConName

    let tcLookupPromDataCon = fmap promoteDataCon . tcLookupDataCon

    md      <- tcLookupPromDataCon metaDataDataConName
    mc      <- tcLookupPromDataCon metaConsDataConName
    ms      <- tcLookupPromDataCon metaSelDataConName
    pPrefix <- tcLookupPromDataCon prefixIDataConName
    pInfix  <- tcLookupPromDataCon infixIDataConName
    pLA     <- tcLookupPromDataCon leftAssociativeDataConName
    pRA     <- tcLookupPromDataCon rightAssociativeDataConName
    pNA     <- tcLookupPromDataCon notAssociativeDataConName

    fix_env <- getFixityEnv

    let mkSum' a b = mkTyConApp plus  [a,b]
        mkProd a b = mkTyConApp times [a,b]
        mkComp a b = mkTyConApp comp  [a,b]
        mkRec0 a   = mkTyConApp rec0  [a]
        mkRec1 a   = mkTyConApp rec1  [a]
        mkPar1     = mkTyConTy  par1
        mkD    a   = mkTyConApp d1 [ metaDataTy, sumP (tyConDataCons a) ]
        mkC      a = mkTyConApp c1 [ metaConsTy a
                                   , prod (dataConInstOrigArgTys a
                                            . mkTyVarTys . tyConTyVars $ tycon)
                                          (dataConFieldLabels a)]
        mkS mlbl a = mkTyConApp s1 [metaSelTy mlbl, a]

        -- Sums and products are done in the same way for both Rep and Rep1
        sumP [] = mkTyConTy v1
        sumP l  = foldBal mkSum' . map mkC  $ l
        -- The Bool is True if this constructor has labelled fields
        prod :: [Type] -> [FieldLabel] -> Type
        prod [] _ = mkTyConTy u1
        prod l fl = foldBal mkProd [ ASSERT(null fl || length fl > j)
                                     arg t (if null fl then Nothing
                                                       else Just (fl !! j))
                                   | (t,j) <- zip l [0..] ]

        arg :: Type -> Maybe FieldLabel -> Type
        arg t fl = mkS fl $ case gk_ of
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
               ata_rec1 = mkRec1, ata_comp = mkComp}

        tyConName_user = case tyConFamInst_maybe tycon of
                           Just (ptycon, _) -> tyConName ptycon
                           Nothing          -> tyConName tycon

        dtName = mkStrLitTy . occNameFS . nameOccName $ tyConName_user
        mdName = mkStrLitTy . moduleNameFS . moduleName . nameModule . tyConName
               $ tycon
        isNT   = mkTyConTy $ if isNewTyCon tycon
                             then promotedTrueDataCon
                             else promotedFalseDataCon

        ctName = mkStrLitTy . occNameFS . nameOccName . dataConName
        ctFix c = case myLookupFixity fix_env (dataConName c) of
                    Just (Fixity n InfixL) -> buildFix n pLA
                    Just (Fixity n InfixR) -> buildFix n pRA
                    Just (Fixity n InfixN) -> buildFix n pNA
                    Nothing                -> mkTyConTy pPrefix
        buildFix n assoc = mkTyConApp pInfix [ mkTyConTy assoc
                                             , mkNumLitTy (fromIntegral n)]

        myLookupFixity :: FixityEnv -> Name -> Maybe Fixity
        myLookupFixity env n = case lookupNameEnv env n of
                                 Just (FixItem _ fix) -> Just fix
                                 Nothing              -> Nothing

        isRec c = mkTyConTy $ if length (dataConFieldLabels c) > 0
                              then promotedTrueDataCon
                              else promotedFalseDataCon

        selName = mkStrLitTy . occNameFS . nameOccName

        metaDataTy   = mkTyConApp md [dtName, mdName, isNT]
        metaConsTy c = mkTyConApp mc [ctName c, ctFix c, isRec c]
        metaSelTy ml = mkTyConApp ms
                         [maybe (mkStrLitTy (mkFastString "")) selName ml]

    return (mkD tycon)

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
    from_alt = (nlWildPat, mkM1_E (makeError errMsgFrom))
    to_alt   = (mkM1_P nlWildPat, makeError errMsgTo)
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
    from_alt_rhs = mkM1_E (genLR_E i n (mkProd_E gk_ us' datacon_varTys))

    to_alt     = (mkM1_P (genLR_P i n (mkProd_P gk us' datacon_vars)), to_alt_rhs)
                 -- These M1s are meta-information for the datatype
    to_alt_rhs = case gk_ of
      Gen0_DC        -> nlHsVarApps datacon_rdr datacon_vars
      Gen1_DC argVar -> nlHsApps datacon_rdr $ map argTo datacon_varTys
        where
          argTo (var, ty) = converter ty `nlHsApp` nlHsVar var where
            converter = argTyFold argVar $ ArgTyAlg
              {ata_rec0 = const $ nlHsVar unK1_RDR,
               ata_par1 = nlHsVar unPar1_RDR,
               ata_rec1 = const $ nlHsVar unRec1_RDR,
               ata_comp = \_ cnv -> (nlHsVar fmap_RDR `nlHsApp` cnv)
                                    `nlHsCompose` nlHsVar unComp1_RDR}



-- Generates the L1/R1 sum pattern
genLR_P :: Int -> Int -> LPat RdrName -> LPat RdrName
genLR_P i n p
  | n == 0       = error "impossible"
  | n == 1       = p
  | i <= div n 2 = nlConPat l1DataCon_RDR [genLR_P i     (div n 2) p]
  | otherwise    = nlConPat r1DataCon_RDR [genLR_P (i-m) (n-m)     p]
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
wrapArg_E Gen0_DC          (var, _)  = mkM1_E (k1DataCon_RDR `nlHsVarApps` [var])
                         -- This M1 is meta-information for the selector
wrapArg_E (Gen1_DC argVar) (var, ty) = mkM1_E $ converter ty `nlHsApp` nlHsVar var
                         -- This M1 is meta-information for the selector
  where converter = argTyFold argVar $ ArgTyAlg
          {ata_rec0 = const $ nlHsVar k1DataCon_RDR,
           ata_par1 = nlHsVar par1DataCon_RDR,
           ata_rec1 = const $ nlHsVar rec1DataCon_RDR,
           ata_comp = \_ cnv -> nlHsVar comp1DataCon_RDR `nlHsCompose`
                                  (nlHsVar fmap_RDR `nlHsApp` cnv)}

-- Build a product pattern
mkProd_P :: GenericKind   -- Gen0 or Gen1
         -> US                  -- Base for unique names
               -> [RdrName]     -- List of variables to match
               -> LPat RdrName  -- Resulting product pattern
mkProd_P _  _ []   = mkM1_P (nlNullaryConPat u1DataCon_RDR)
mkProd_P gk _ vars = mkM1_P (foldBal prod appVars)
                     -- These M1s are meta-information for the constructor
  where
    appVars = map (wrapArg_P gk) vars
    prod a b = prodDataCon_RDR `nlConPat` [a,b]

wrapArg_P :: GenericKind -> RdrName -> LPat RdrName
wrapArg_P Gen0 v = mkM1_P (k1DataCon_RDR `nlConVarPat` [v])
                   -- This M1 is meta-information for the selector
wrapArg_P Gen1 v = m1DataCon_RDR `nlConVarPat` [v]

mkGenericLocal :: US -> RdrName
mkGenericLocal u = mkVarUnqual (mkFastString ("g" ++ show u))

mkM1_E :: LHsExpr RdrName -> LHsExpr RdrName
mkM1_E e = nlHsVar m1DataCon_RDR `nlHsApp` e

mkM1_P :: LPat RdrName -> LPat RdrName
mkM1_P p = m1DataCon_RDR `nlConPat` [p]

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

\end{code}
