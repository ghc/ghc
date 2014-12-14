{-# LANGUAGE CPP, PatternGuards #-}
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

import Bag ( emptyBag )
import BasicTypes ( TupleSort(..) )
import Class
import CoAxiom
import ConLike
import Data.Either (lefts, rights)
import Data.List( partition )
import Data.Monoid (mempty)
import DataCon
import FamInstEnv
import Haddock.Types
import HsSyn
import Kind ( splitKindFunTys, synTyConResKind, isKind )
import Name
import PatSyn
import PrelNames (ipClassName)
import SrcLoc ( Located, noLoc, unLoc )
import TcType ( tcSplitSigmaTy )
import TyCon
import Type (isStrLitTy, mkFunTys)
import TypeRep
import TysPrim ( alphaTyVars )
import TysWiredIn ( listTyConName, eqTyCon )
import Unique ( getUnique )
import Var



-- the main function here! yay!
tyThingToLHsDecl :: TyThing -> Either ErrMsg ([ErrMsg], (HsDecl Name))
tyThingToLHsDecl t = case t of
  -- ids (functions and zero-argument a.k.a. CAFs) get a type signature.
  -- Including built-in functions like seq.
  -- foreign-imported functions could be represented with ForD
  -- instead of SigD if we wanted...
  --
  -- in a future code version we could turn idVarDetails = foreign-call
  -- into a ForD instead of a SigD if we wanted.  Haddock doesn't
  -- need to care.
  AnId i -> allOK $ SigD (synifyIdSig ImplicitizeForAll i)

  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ATyCon tc
    | Just cl <- tyConClass_maybe tc -- classes are just a little tedious
    -> let extractFamilyDecl :: TyClDecl a -> Either ErrMsg (LFamilyDecl a)
           extractFamilyDecl (FamDecl d) = return $ noLoc d
           extractFamilyDecl _           =
             Left "tyThingToLHsDecl: impossible associated tycon"

           atTyClDecls = [synifyTyCon Nothing at_tc | ATI at_tc _ <- classATItems cl]
           atFamDecls  = map extractFamilyDecl (rights atTyClDecls)
           tyClErrors = lefts atTyClDecls
           famDeclErrors = lefts atFamDecls
       in withErrs (tyClErrors ++ famDeclErrors) . TyClD $ ClassDecl
         { tcdCtxt = synifyCtx (classSCTheta cl)
         , tcdLName = synifyName cl
         , tcdTyVars = synifyTyVars (classTyVars cl)
         , tcdFDs = map (\ (l,r) -> noLoc
                        (map getName l, map getName r) ) $
                         snd $ classTvsFds cl
         , tcdSigs = noLoc (MinimalSig . fmap noLoc $ classMinimalDef cl) :
                      map (noLoc . synifyIdSig DeleteTopLevelQuantification)
                        (classMethods cl)
         , tcdMeths = emptyBag --ignore default method definitions, they don't affect signature
         -- class associated-types are a subset of TyCon:
         , tcdATs = rights atFamDecls
         , tcdATDefs = [] --ignore associated type defaults
         , tcdDocs = [] --we don't have any docs at this point
         , tcdFVs = placeHolderNamesTc }
    | otherwise
    -> synifyTyCon Nothing tc >>= allOK . TyClD

  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ACoAxiom ax -> synifyAxiom ax >>= allOK

  -- a data-constructor alone just gets rendered as a function:
  AConLike (RealDataCon dc) -> allOK $ SigD (TypeSig [synifyName dc]
    (synifyType ImplicitizeForAll (dataConUserType dc)) [])

  AConLike (PatSynCon ps) ->
      let (univ_tvs, ex_tvs, req_theta, prov_theta, arg_tys, res_ty) = patSynSig ps
          qtvs = univ_tvs ++ ex_tvs
          ty = mkFunTys arg_tys res_ty
      in allOK . SigD $ PatSynSig (synifyName ps)
                          (Implicit, synifyTyVars qtvs)
                          (synifyCtx req_theta)
                          (synifyCtx prov_theta)
                          (synifyType WithinType ty)
  where
    withErrs e x = return (e, x)
    allOK x = return (mempty, x)

synifyAxBranch :: TyCon -> CoAxBranch -> TyFamInstEqn Name
synifyAxBranch tc (CoAxBranch { cab_tvs = tkvs, cab_lhs = args, cab_rhs = rhs })
  = let name       = synifyName tc
        typats     = map (synifyType WithinType) args
        hs_rhs     = synifyType WithinType rhs
        (kvs, tvs) = partition isKindVar tkvs
    in TyFamEqn { tfe_tycon = name
                , tfe_pats  = HsWB { hswb_cts = typats
                                    , hswb_kvs = map tyVarName kvs
                                    , hswb_tvs = map tyVarName tvs
                                    , hswb_wcs = [] }
                , tfe_rhs   = hs_rhs }

synifyAxiom :: CoAxiom br -> Either ErrMsg (HsDecl Name)
synifyAxiom ax@(CoAxiom { co_ax_tc = tc })
  | isOpenTypeFamilyTyCon tc
  , Just branch <- coAxiomSingleBranch_maybe ax
  = return $ InstD (TyFamInstD
                    (TyFamInstDecl { tfid_eqn = noLoc $ synifyAxBranch tc branch
                                   , tfid_fvs = placeHolderNamesTc }))

  | Just ax' <- isClosedSynFamilyTyCon_maybe tc
  , getUnique ax' == getUnique ax   -- without the getUniques, type error
  = synifyTyCon (Just ax) tc >>= return . TyClD

  | otherwise
  = Left "synifyAxiom: closed/open family confusion"

-- | Turn type constructors into type class declarations
synifyTyCon :: Maybe (CoAxiom br) -> TyCon -> Either ErrMsg (TyClDecl Name)
synifyTyCon coax tc
  | isFunTyCon tc || isPrimTyCon tc
  = return $
    DataDecl { tcdLName = synifyName tc
             , tcdTyVars =       -- tyConTyVars doesn't work on fun/prim, but we can make them up:
                         let mk_hs_tv realKind fakeTyVar
                                = noLoc $ KindedTyVar (getName fakeTyVar)
                                                      (synifyKindSig realKind)
                         in HsQTvs { hsq_kvs = []   -- No kind polymorphism
                                   , hsq_tvs = zipWith mk_hs_tv (fst (splitKindFunTys (tyConKind tc)))
                                                                alphaTyVars --a, b, c... which are unfortunately all kind *
                                   }

           , tcdDataDefn = HsDataDefn { dd_ND = DataType  -- arbitrary lie, they are neither
                                                    -- algebraic data nor newtype:
                                      , dd_ctxt = noLoc []
                                      , dd_cType = Nothing
                                      , dd_kindSig = Just (synifyKindSig (tyConKind tc))
                                               -- we have their kind accurately:
                                      , dd_cons = []  -- No constructors
                                      , dd_derivs = Nothing }
           , tcdFVs = placeHolderNamesTc }

  | isTypeFamilyTyCon tc
  = case famTyConFlav_maybe tc of
      Just rhs ->
        let info = case rhs of
              OpenSynFamilyTyCon -> return OpenTypeFamily
              ClosedSynFamilyTyCon (CoAxiom { co_ax_branches = branches }) ->
                return $ ClosedTypeFamily
                  (brListMap (noLoc . synifyAxBranch tc) branches)
              BuiltInSynFamTyCon {} -> return $ ClosedTypeFamily []
              AbstractClosedSynFamilyTyCon {} -> return $ ClosedTypeFamily []
        in info >>= \i ->
           return (FamDecl
                   (FamilyDecl { fdInfo = i
                               , fdLName = synifyName tc
                               , fdTyVars = synifyTyVars (tyConTyVars tc)
                               , fdKindSig =
                                 Just (synifyKindSig (synTyConResKind tc))
                               }))
      Nothing -> Left "synifyTyCon: impossible open type synonym?"

  | isDataFamilyTyCon tc
  = --(why no "isOpenAlgTyCon"?)
    case algTyConRhs tc of
        DataFamilyTyCon -> return $
          FamDecl (FamilyDecl DataFamily (synifyName tc) (synifyTyVars (tyConTyVars tc))
                              Nothing) --always kind '*'
        _ -> Left "synifyTyCon: impossible open data type?"
  | Just ty <- synTyConRhs_maybe tc
  = return $ SynDecl { tcdLName = synifyName tc
                     , tcdTyVars = synifyTyVars (tyConTyVars tc)
                     , tcdRhs = synifyType WithinType ty
                     , tcdFVs = placeHolderNamesTc }
  | otherwise =
  -- (closed) newtype and data
  let
  alg_nd = if isNewTyCon tc then NewType else DataType
  alg_ctx = synifyCtx (tyConStupidTheta tc)
  name = case coax of
    Just a -> synifyName a -- Data families are named according to their
                           -- CoAxioms, not their TyCons
    _ -> synifyName tc
  tyvars = synifyTyVars (tyConTyVars tc)
  kindSig = Just (tyConKind tc)
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
  use_gadt_syntax = any (not . isVanillaDataCon) (tyConDataCons tc)
  consRaw = map (synifyDataCon use_gadt_syntax) (tyConDataCons tc)
  cons = rights consRaw
  -- "deriving" doesn't affect the signature, no need to specify any.
  alg_deriv = Nothing
  defn = HsDataDefn { dd_ND      = alg_nd
                    , dd_ctxt    = alg_ctx
                    , dd_cType   = Nothing
                    , dd_kindSig = fmap synifyKindSig kindSig
                    , dd_cons    = cons
                    , dd_derivs  = alg_deriv }
 in case lefts consRaw of
  [] -> return $
        DataDecl { tcdLName = name, tcdTyVars = tyvars, tcdDataDefn = defn
                 , tcdFVs = placeHolderNamesTc }
  dataConErrs -> Left $ unlines dataConErrs

-- User beware: it is your responsibility to pass True (use_gadt_syntax)
-- for any constructor that would be misrepresented by omitting its
-- result-type.
-- But you might want pass False in simple enough cases,
-- if you think it looks better.
synifyDataCon :: Bool -> DataCon -> Either ErrMsg (LConDecl Name)
synifyDataCon use_gadt_syntax dc =
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
                src_bang = case bang of
                             HsUnpack {} -> HsUserBang (Just True) True
                             HsStrict    -> HsUserBang (Just False) True
                             _           -> bang
            in case src_bang of
                 HsNoBang -> tySyn
                 _        -> noLoc $ HsBangTy bang tySyn
            -- HsNoBang never appears, it's implied instead.
          )
          arg_tys (dataConStrictMarks dc)
  field_tys = zipWith (\field synTy -> noLoc $ ConDeclField
                                               [synifyName field] synTy Nothing)
                (dataConFieldLabels dc) linear_tys
  hs_arg_tys = case (use_named_field_syntax, use_infix_syntax) of
          (True,True) -> Left "synifyDataCon: contradiction!"
          (True,False) -> return $ RecCon field_tys
          (False,False) -> return $ PrefixCon linear_tys
          (False,True) -> case linear_tys of
                           [a,b] -> return $ InfixCon a b
                           _ -> Left "synifyDataCon: infix with non-2 args?"
  hs_res_ty = if use_gadt_syntax
              then ResTyGADT (synifyType WithinType res_ty)
              else ResTyH98
 -- finally we get synifyDataCon's result!
 in hs_arg_tys >>=
      \hat -> return . noLoc $ ConDecl [name] Implicit -- we don't know nor care
                qvars ctx hat hs_res_ty Nothing
                -- we don't want any "deprecated GADT syntax" warnings!
                False

synifyName :: NamedThing n => n -> Located Name
synifyName = noLoc . getName


synifyIdSig :: SynifyTypeState -> Id -> Sig Name
synifyIdSig s i = TypeSig [synifyName i] (synifyType s (varType i)) []


synifyCtx :: [PredType] -> LHsContext Name
synifyCtx = noLoc . map (synifyType WithinType)


synifyTyVars :: [TyVar] -> LHsTyVarBndrs Name
synifyTyVars ktvs = HsQTvs { hsq_kvs = map tyVarName kvs
                           , hsq_tvs = map synifyTyVar tvs }
  where
    (kvs, tvs) = partition isKindVar ktvs
    synifyTyVar tv
      | isLiftedTypeKind kind = noLoc (UserTyVar name)
      | otherwise             = noLoc (KindedTyVar name (synifyKindSig kind))
      where
        kind = tyVarKind tv
        name = getName tv

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
                          BoxedTuple      -> HsBoxedTuple
                          ConstraintTuple -> HsConstraintTuple
                          UnboxedTuple    -> HsUnboxedTuple)
                       (map (synifyType WithinType) tys)
  -- ditto for lists
  | getName tc == listTyConName, [ty] <- tys =
     noLoc $ HsListTy (synifyType WithinType ty)
  -- ditto for implicit parameter tycons
  | tyConName tc == ipClassName
  , [name, ty] <- tys
  , Just x <- isStrLitTy name
  = noLoc $ HsIParamTy (HsIPName x) (synifyType WithinType ty)
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
      sTvs = synifyTyVars tvs
      sCtx = synifyCtx ctx
      sTau = synifyType WithinType tau
      mkHsForAllTy forallPlicitness =
        noLoc $ HsForAllTy forallPlicitness Nothing sTvs sCtx sTau
  in case s of
    DeleteTopLevelQuantification -> synifyType ImplicitizeForAll tau
    WithinType -> mkHsForAllTy Explicit
    ImplicitizeForAll -> mkHsForAllTy Implicit

synifyType _ (LitTy t) = noLoc $ HsTyLit $ synifyTyLit t

synifyTyLit :: TyLit -> HsTyLit
synifyTyLit (NumTyLit n) = HsNumTy n
synifyTyLit (StrTyLit s) = HsStrTy s

synifyKindSig :: Kind -> LHsKind Name
synifyKindSig k = synifyType WithinType k

synifyInstHead :: ([TyVar], [PredType], Class, [Type]) -> InstHead Name
synifyInstHead (_, preds, cls, types) =
  ( getName cls
  , map (unLoc . synifyType WithinType) ks
  , map (unLoc . synifyType WithinType) ts
  , ClassInst $ map (unLoc . synifyType WithinType) preds
  )
  where (ks,ts) = break (not . isKind) types

-- Convert a family instance, this could be a type family or data family
synifyFamInst :: FamInst -> Bool -> Either ErrMsg (InstHead Name)
synifyFamInst fi opaque =
  let fff = case fi_flavor fi of
        SynFamilyInst | opaque -> return $ TypeInst Nothing
        SynFamilyInst ->
          return . TypeInst . Just . unLoc . synifyType WithinType $ fi_rhs fi
        DataFamilyInst c ->
          synifyTyCon (Just $ famInstAxiom fi) c >>= return . DataInst
  in fff >>= \f' -> return (fi_fam fi , map (unLoc . synifyType WithinType) ks,
                            map (unLoc . synifyType WithinType) ts , f')
  where (ks,ts) = break (not . isKind) $ fi_tys fi
