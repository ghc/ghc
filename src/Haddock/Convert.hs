{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Convert
-- Copyright   :  (c) Isaac Dupree 2009,
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion between TyThing and HsDecl. This functionality may be moved into
-- GHC at some point.
-----------------------------------------------------------------------------
module Haddock.Convert where
-- Some other functions turned out to be useful for converting
-- instance heads, which aren't TyThings, so just export everything.


import HsSyn
import TcType ( tcSplitTyConApp_maybe, tcSplitSigmaTy )
import TypeRep
import Kind ( liftedTypeKind, constraintKind )
import Coercion ( splitKindFunTys, synTyConResKind )
import Name
import Var
import Class
import TyCon
import DataCon
import BasicTypes ( TupleSort(..) )
import TysPrim ( alphaTyVars )
import TysWiredIn ( listTyConName, eqTyCon )
import Bag ( emptyBag )
import SrcLoc ( Located, noLoc, unLoc )


-- the main function here! yay!
tyThingToLHsDecl :: TyThing -> LHsDecl Name
tyThingToLHsDecl t = noLoc $ case t of
  -- ids (functions and zero-argument a.k.a. CAFs) get a type signature.
  -- Including built-in functions like seq.
  -- foreign-imported functions could be represented with ForD
  -- instead of SigD if we wanted...
  --
  -- in a future code version we could turn idVarDetails = foreign-call
  -- into a ForD instead of a SigD if we wanted.  Haddock doesn't
  -- need to care.
  AnId i -> SigD (synifyIdSig ImplicitizeForAll i)

  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ATyCon tc
    | Just cl <- tyConClass_maybe tc -- classes are just a little tedious
    -> TyClD $ ClassDecl
         (synifyCtx (classSCTheta cl))
         (synifyName cl)
         (synifyTyVars (classTyVars cl))
         (map (\ (l,r) -> noLoc
                    (map getName l, map getName r) ) $
            snd $ classTvsFds cl)
         (map (noLoc . synifyIdSig DeleteTopLevelQuantification)
              (classMethods cl))
         emptyBag --ignore default method definitions, they don't affect signature
         -- class associated-types are a subset of TyCon:
         [noLoc (synifyTyCon at_tc) | (at_tc, _) <- classATItems cl]
         [] --ignore associated type defaults
         [] --we don't have any docs at this point
    | otherwise
    -> TyClD (synifyTyCon tc)

  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ACoAxiom ax -> TyClD (synifyAxiom ax)

  -- a data-constructor alone just gets rendered as a function:
  ADataCon dc -> SigD (TypeSig [synifyName dc]
    (synifyType ImplicitizeForAll (dataConUserType dc)))

synifyATDefault :: TyCon -> LTyClDecl Name
synifyATDefault tc = noLoc (synifyAxiom ax)
  where Just ax = tyConFamilyCoercion_maybe tc

synifyAxiom :: CoAxiom -> TyClDecl Name
synifyAxiom (CoAxiom { co_ax_tvs = tvs, co_ax_lhs = lhs, co_ax_rhs = rhs })
  | Just (tc, args) <- tcSplitTyConApp_maybe lhs
  = let name      = synifyName tc
        tyvars    = synifyTyVars tvs
        typats    = map (synifyType WithinType) args
        hs_rhs_ty = synifyType WithinType rhs
    in TySynonym name tyvars (Just typats) hs_rhs_ty
  | otherwise
  = error "synifyAxiom" 

synifyTyCon :: TyCon -> TyClDecl Name
synifyTyCon tc
  | isFunTyCon tc || isPrimTyCon tc =
    TyData
      -- arbitrary lie, they are neither algebraic data nor newtype:
      DataType
      -- no built-in type has any stupidTheta:
      (noLoc [])
      (synifyName tc)
      -- tyConTyVars doesn't work on fun/prim, but we can make them up:
      (zipWith
         (\fakeTyVar realKind -> noLoc $
             KindedTyVar (getName fakeTyVar) realKind)
         alphaTyVars --a, b, c... which are unfortunately all kind *
         (fst . splitKindFunTys $ tyConKind tc)
      )
      -- assume primitive types aren't members of data/newtype families:
      Nothing
      -- we have their kind accurately:
      (Just (tyConKind tc))
      -- no algebraic constructors:
      []
      -- "deriving" needn't be specified:
      Nothing
  | isSynFamilyTyCon tc =
      case synTyConRhs tc of
        SynFamilyTyCon ->
          TyFamily TypeFamily (synifyName tc) (synifyTyVars (tyConTyVars tc))
               (Just (synTyConResKind tc))
        _ -> error "synifyTyCon: impossible open type synonym?"
  | isDataFamilyTyCon tc = --(why no "isOpenAlgTyCon"?)
      case algTyConRhs tc of
        DataFamilyTyCon ->
          TyFamily DataFamily (synifyName tc) (synifyTyVars (tyConTyVars tc))
               Nothing --always kind '*'
        _ -> error "synifyTyCon: impossible open data type?"
  | otherwise =
  -- (closed) type, newtype, and data
  let
  -- alg_ only applies to newtype/data
  -- syn_ only applies to type
  -- others apply to both
  alg_nd = if isNewTyCon tc then NewType else DataType
  alg_ctx = synifyCtx (tyConStupidTheta tc)
  name = synifyName tc
  tyvars = synifyTyVars (tyConTyVars tc)
  typats = case tyConFamInst_maybe tc of
     Nothing -> Nothing
     Just (_, indexes) -> Just (map (synifyType WithinType) indexes)
  alg_kindSig = Just (tyConKind tc)
  -- The data constructors.
  --
  -- Any data-constructors not exported from the module that *defines* the
  -- type will not (cannot) be included.
  --
  -- Very simple constructors, Haskell98 with no existentials or anything,
  -- probably look nicer in non-GADT syntax.  In source code, all constructors
  -- must be declared with the same (GADT vs. not) syntax, and it probably
  -- is less confusing to follow that principle for the documentation as well.
  --
  -- There is no sensible infix-representation for GADT-syntax constructor
  -- declarations.  They cannot be made in source code, but we could end up
  -- with some here in the case where some constructors use existentials.
  -- That seems like an acceptable compromise (they'll just be documented
  -- in prefix position), since, otherwise, the logic (at best) gets much more
  -- complicated. (would use dataConIsInfix.)
  alg_use_gadt_syntax = any (not . isVanillaDataCon) (tyConDataCons tc)
  alg_cons = map (synifyDataCon alg_use_gadt_syntax) (tyConDataCons tc)
  -- "deriving" doesn't affect the signature, no need to specify any.
  alg_deriv = Nothing
  syn_type = synifyType WithinType (synTyConType tc)
 in if isSynTyCon tc
  then TySynonym name tyvars typats syn_type
  else TyData alg_nd alg_ctx name tyvars typats alg_kindSig alg_cons alg_deriv


-- User beware: it is your responsibility to pass True (use_gadt_syntax)
-- for any constructor that would be misrepresented by omitting its
-- result-type.
-- But you might want pass False in simple enough cases,
-- if you think it looks better.
synifyDataCon :: Bool -> DataCon -> LConDecl Name
synifyDataCon use_gadt_syntax dc = noLoc $
 let
  -- dataConIsInfix allegedly tells us whether it was declared with
  -- infix *syntax*.
  use_infix_syntax = dataConIsInfix dc
  use_named_field_syntax = not (null field_tys)
  name = synifyName dc
  -- con_qvars means a different thing depending on gadt-syntax
  (univ_tvs, ex_tvs, _eq_spec, theta, arg_tys, res_ty) = dataConFullSig dc

  qvars = if use_gadt_syntax
          then synifyTyVars (univ_tvs ++ ex_tvs)
          else synifyTyVars ex_tvs

  -- skip any EqTheta, use 'orig'inal syntax
  ctx = synifyCtx theta

  linear_tys = zipWith (\ty bang ->
            let tySyn = synifyType WithinType ty
            in case bang of
                 HsUnpackFailed -> noLoc $ HsBangTy HsStrict tySyn
                 HsNoBang       -> tySyn
                      -- HsNoBang never appears, it's implied instead.
                 _              -> noLoc $ HsBangTy bang tySyn
          )
          arg_tys (dataConStrictMarks dc)
  field_tys = zipWith (\field synTy -> ConDeclField
                                           (synifyName field) synTy Nothing)
                (dataConFieldLabels dc) linear_tys
  hs_arg_tys = case (use_named_field_syntax, use_infix_syntax) of
          (True,True) -> error "synifyDataCon: contradiction!"
          (True,False) -> RecCon field_tys
          (False,False) -> PrefixCon linear_tys
          (False,True) -> case linear_tys of
                           [a,b] -> InfixCon a b
                           _ -> error "synifyDataCon: infix with non-2 args?"
  hs_res_ty = if use_gadt_syntax
              then ResTyGADT (synifyType WithinType res_ty)
              else ResTyH98
 -- finally we get synifyDataCon's result!
 in ConDecl name Implicit{-we don't know nor care-}
      qvars ctx hs_arg_tys hs_res_ty Nothing
      False --we don't want any "deprecated GADT syntax" warnings!


synifyName :: NamedThing n => n -> Located Name
synifyName = noLoc . getName


synifyIdSig :: SynifyTypeState -> Id -> Sig Name
synifyIdSig s i = TypeSig [synifyName i] (synifyType s (varType i))


synifyCtx :: [PredType] -> LHsContext Name
synifyCtx = noLoc . map (synifyType WithinType)


synifyTyVars :: [TyVar] -> [LHsTyVarBndr Name]
synifyTyVars = map synifyTyVar
  where
    synifyTyVar tv = noLoc $ let
      kind = tyVarKind tv
      name = getName tv
     in if isLiftedTypeKind kind
        then UserTyVar name placeHolderKind
        else KindedTyVar name kind


--states of what to do with foralls:
data SynifyTypeState
  = WithinType
  -- ^ normal situation.  This is the safe one to use if you don't
  -- quite understand what's going on.
  | ImplicitizeForAll
  -- ^ beginning of a function definition, in which, to make it look
  --   less ugly, those rank-1 foralls are made implicit.
  | DeleteTopLevelQuantification
  -- ^ because in class methods the context is added to the type
  --   (e.g. adding @forall a. Num a =>@ to @(+) :: a -> a -> a@)
  --   which is rather sensible,
  --   but we want to restore things to the source-syntax situation where
  --   the defining class gets to quantify all its functions for free!


synifyType :: SynifyTypeState -> Type -> LHsType Name
synifyType _ (TyVarTy tv) = noLoc $ HsTyVar (getName tv)
synifyType _ (TyConApp tc tys)
  -- Use non-prefix tuple syntax where possible, because it looks nicer.
  | isTupleTyCon tc, tyConArity tc == length tys =
     noLoc $ HsTupleTy (case tupleTyConSort tc of
                          BoxedTuple   -> HsBoxyTuple liftedTypeKind
                          FactTuple    -> HsBoxyTuple constraintKind
                          UnboxedTuple -> HsUnboxedTuple)
                       (map (synifyType WithinType) tys)
  -- ditto for lists
  | getName tc == listTyConName, [ty] <- tys =
     noLoc $ HsListTy (synifyType WithinType ty)
  -- ditto for implicit parameter tycons
  | Just ip <- tyConIP_maybe tc
  , [ty] <- tys
  = noLoc $ HsIParamTy ip (synifyType WithinType ty)
  -- and equalities
  | tc == eqTyCon
  , [ty1, ty2] <- tys
  = noLoc $ HsEqTy (synifyType WithinType ty1) (synifyType WithinType ty2)
  -- Most TyCons:
  | otherwise =
    foldl (\t1 t2 -> noLoc (HsAppTy t1 t2))
      (noLoc $ HsTyVar (getName tc))
      (map (synifyType WithinType) tys)
synifyType _ (AppTy t1 t2) = let
  s1 = synifyType WithinType t1
  s2 = synifyType WithinType t2
  in noLoc $ HsAppTy s1 s2
synifyType _ (FunTy t1 t2) = let
  s1 = synifyType WithinType t1
  s2 = synifyType WithinType t2
  in noLoc $ HsFunTy s1 s2
synifyType s forallty@(ForAllTy _tv _ty) =
  let (tvs, ctx, tau) = tcSplitSigmaTy forallty
  in case s of
    DeleteTopLevelQuantification -> synifyType ImplicitizeForAll tau
    _ -> let
      forallPlicitness = case s of
              WithinType -> Explicit
              ImplicitizeForAll -> Implicit
              _ -> error "synifyType: impossible case!!!"
      sTvs = synifyTyVars tvs
      sCtx = synifyCtx ctx
      sTau = synifyType WithinType tau
     in noLoc $
           HsForAllTy forallPlicitness sTvs sCtx sTau


synifyInstHead :: ([TyVar], [PredType], Class, [Type]) ->
                  ([HsType Name], Name, [HsType Name])
synifyInstHead (_, preds, cls, ts) =
  ( map (unLoc . synifyType WithinType) preds
  , getName cls
  , map (unLoc . synifyType WithinType) ts
  )
