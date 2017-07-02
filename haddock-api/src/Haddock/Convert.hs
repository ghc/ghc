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
import BasicTypes ( TupleSort(..), SourceText(..), LexicalFixity(..) )
import Class
import CoAxiom
import ConLike
import Data.Either (lefts, rights)
import DataCon
import FamInstEnv
import HsSyn
import Name
import NameSet ( emptyNameSet )
import RdrName ( mkVarUnqual )
import PatSyn
import SrcLoc ( Located, noLoc, unLoc, GenLocated(..), srcLocSpan )
import TcType ( tcSplitSigmaTy )
import TyCon
import Type
import TyCoRep
import TysPrim ( alphaTyVars )
import TysWiredIn ( listTyConName, starKindTyConName, unitTy )
import PrelNames ( hasKey, eqTyConKey, ipClassKey
                 , tYPETyConKey, liftedRepDataConKey )
import Unique ( getUnique )
import Util ( filterByList, filterOut )
import Var

import Haddock.Types
import Haddock.Interface.Specialize



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
         , tcdFixity = Prefix
         , tcdFDs = map (\ (l,r) -> noLoc
                        (map (noLoc . getName) l, map (noLoc . getName) r) ) $
                         snd $ classTvsFds cl
         , tcdSigs = noLoc (MinimalSig NoSourceText . noLoc . fmap noLoc $ classMinimalDef cl) :
                      map (noLoc . synifyTcIdSig DeleteTopLevelQuantification)
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
    (synifySigWcType ImplicitizeForAll (dataConUserType dc)))

  AConLike (PatSynCon ps) ->
    allOK . SigD $ PatSynSig [synifyName ps] (synifyPatSynSigType ps)
  where
    withErrs e x = return (e, x)
    allOK x = return (mempty, x)

synifyAxBranch :: TyCon -> CoAxBranch -> TyFamInstEqn Name
synifyAxBranch tc (CoAxBranch { cab_tvs = tkvs, cab_lhs = args, cab_rhs = rhs })
  = let name       = synifyName tc
        typats     = map (synifyType WithinType) args
        hs_rhs     = synifyType WithinType rhs
    in TyFamEqn { tfe_tycon = name
                , tfe_pats  = HsIB { hsib_body = typats
                                   , hsib_vars = map tyVarName tkvs
                                   , hsib_closed = True }
                , tfe_fixity = Prefix
                , tfe_rhs   = hs_rhs }

synifyAxiom :: CoAxiom br -> Either ErrMsg (HsDecl Name)
synifyAxiom ax@(CoAxiom { co_ax_tc = tc })
  | isOpenTypeFamilyTyCon tc
  , Just branch <- coAxiomSingleBranch_maybe ax
  = return $ InstD (TyFamInstD
                    (TyFamInstDecl { tfid_eqn = noLoc $ synifyAxBranch tc branch
                                   , tfid_fvs = placeHolderNamesTc }))

  | Just ax' <- isClosedSynFamilyTyConWithAxiom_maybe tc
  , getUnique ax' == getUnique ax   -- without the getUniques, type error
  = synifyTyCon (Just ax) tc >>= return . TyClD

  | otherwise
  = Left "synifyAxiom: closed/open family confusion"

-- | Turn type constructors into type class declarations
synifyTyCon :: Maybe (CoAxiom br) -> TyCon -> Either ErrMsg (TyClDecl Name)
synifyTyCon _coax tc
  | isFunTyCon tc || isPrimTyCon tc
  = return $
    DataDecl { tcdLName = synifyName tc
             , tcdTyVars =       -- tyConTyVars doesn't work on fun/prim, but we can make them up:
                         let mk_hs_tv realKind fakeTyVar
                                = noLoc $ KindedTyVar (noLoc (getName fakeTyVar))
                                                      (synifyKindSig realKind)
                         in HsQTvs { hsq_implicit = []   -- No kind polymorphism
                                   , hsq_explicit = zipWith mk_hs_tv (fst (splitFunTys (tyConKind tc)))
                                                                alphaTyVars --a, b, c... which are unfortunately all kind *
                                   , hsq_dependent = emptyNameSet }

           , tcdFixity = Prefix

           , tcdDataDefn = HsDataDefn { dd_ND = DataType  -- arbitrary lie, they are neither
                                                    -- algebraic data nor newtype:
                                      , dd_ctxt = noLoc []
                                      , dd_cType = Nothing
                                      , dd_kindSig = Just (synifyKindSig (tyConKind tc))
                                               -- we have their kind accurately:
                                      , dd_cons = []  -- No constructors
                                      , dd_derivs = noLoc [] }
           , tcdDataCusk = False
           , tcdFVs = placeHolderNamesTc }

synifyTyCon _coax tc
  | Just flav <- famTyConFlav_maybe tc
  = case flav of
      -- Type families
      OpenSynFamilyTyCon -> mkFamDecl OpenTypeFamily
      ClosedSynFamilyTyCon mb
        | Just (CoAxiom { co_ax_branches = branches }) <- mb
          -> mkFamDecl $ ClosedTypeFamily $ Just
            $ map (noLoc . synifyAxBranch tc) (fromBranches branches)
        | otherwise
          -> mkFamDecl $ ClosedTypeFamily $ Just []
      BuiltInSynFamTyCon {}
        -> mkFamDecl $ ClosedTypeFamily $ Just []
      AbstractClosedSynFamilyTyCon {}
        -> mkFamDecl $ ClosedTypeFamily Nothing
      DataFamilyTyCon {}
        -> mkFamDecl DataFamily
  where
    resultVar = famTcResVar tc
    mkFamDecl i = return $ FamDecl $
      FamilyDecl { fdInfo = i
                 , fdLName = synifyName tc
                 , fdTyVars = synifyTyVars (tyConTyVars tc)
                 , fdFixity = Prefix
                 , fdResultSig =
                       synifyFamilyResultSig resultVar (tyConResKind tc)
                 , fdInjectivityAnn =
                       synifyInjectivityAnn  resultVar (tyConTyVars tc)
                                       (familyTyConInjectivityInfo tc)
                 }

synifyTyCon coax tc
  | Just ty <- synTyConRhs_maybe tc
  = return $ SynDecl { tcdLName = synifyName tc
                     , tcdTyVars = synifyTyVars (tyConTyVars tc)
                     , tcdFixity = Prefix
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
  alg_deriv = noLoc []
  defn = HsDataDefn { dd_ND      = alg_nd
                    , dd_ctxt    = alg_ctx
                    , dd_cType   = Nothing
                    , dd_kindSig = fmap synifyKindSig kindSig
                    , dd_cons    = cons
                    , dd_derivs  = alg_deriv }
 in case lefts consRaw of
  [] -> return $
        DataDecl { tcdLName = name, tcdTyVars = tyvars, tcdFixity = Prefix
                 , tcdDataDefn = defn
                 , tcdDataCusk = False, tcdFVs = placeHolderNamesTc }
  dataConErrs -> Left $ unlines dataConErrs

synifyInjectivityAnn :: Maybe Name -> [TyVar] -> Injectivity
                     -> Maybe (LInjectivityAnn Name)
synifyInjectivityAnn Nothing _ _            = Nothing
synifyInjectivityAnn _       _ NotInjective = Nothing
synifyInjectivityAnn (Just lhs) tvs (Injective inj) =
    let rhs = map (noLoc . tyVarName) (filterByList inj tvs)
    in Just $ noLoc $ InjectivityAnn (noLoc lhs) rhs

synifyFamilyResultSig :: Maybe Name -> Kind -> LFamilyResultSig Name
synifyFamilyResultSig  Nothing    kind =
   noLoc $ KindSig  (synifyKindSig kind)
synifyFamilyResultSig (Just name) kind =
   noLoc $ TyVarSig (noLoc $ KindedTyVar (noLoc name) (synifyKindSig kind))

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

  linear_tys =
    zipWith (\ty bang ->
               let tySyn = synifyType WithinType ty
               in case bang of
                    (HsSrcBang _ NoSrcUnpack NoSrcStrict) -> tySyn
                    bang' -> noLoc $ HsBangTy bang' tySyn)
            arg_tys (dataConSrcBangs dc)

  field_tys = zipWith con_decl_field (dataConFieldLabels dc) linear_tys
  con_decl_field fl synTy = noLoc $
    ConDeclField [noLoc $ FieldOcc (noLoc $ mkVarUnqual $ flLabel fl) (flSelector fl)] synTy
                 Nothing
  hs_arg_tys = case (use_named_field_syntax, use_infix_syntax) of
          (True,True) -> Left "synifyDataCon: contradiction!"
          (True,False) -> return $ RecCon (noLoc field_tys)
          (False,False) -> return $ PrefixCon linear_tys
          (False,True) -> case linear_tys of
                           [a,b] -> return $ InfixCon a b
                           _ -> Left "synifyDataCon: infix with non-2 args?"
  gadt_ty = HsIB [] (synifyType WithinType res_ty) False
 -- finally we get synifyDataCon's result!
 in hs_arg_tys >>=
      \hat ->
        if use_gadt_syntax
           then return $ noLoc $
              ConDeclGADT { con_names = [name]
                          , con_type = gadt_ty
                          , con_doc =  Nothing }
           else return $ noLoc $
              ConDeclH98 { con_name = name
                         , con_qvars = Just qvars
                         , con_cxt   = Just ctx
                         , con_details =  hat
                         , con_doc =  Nothing }

synifyName :: NamedThing n => n -> Located Name
synifyName n = L (srcLocSpan (getSrcLoc n)) (getName n)


synifyIdSig :: SynifyTypeState -> Id -> Sig Name
synifyIdSig s i = TypeSig [synifyName i] (synifySigWcType s (varType i))

synifyTcIdSig :: SynifyTypeState -> Id -> Sig Name
synifyTcIdSig s i = ClassOpSig False [synifyName i] (synifySigType s (varType i))

synifyCtx :: [PredType] -> LHsContext Name
synifyCtx = noLoc . map (synifyType WithinType)


synifyTyVars :: [TyVar] -> LHsQTyVars Name
synifyTyVars ktvs = HsQTvs { hsq_implicit = []
                           , hsq_explicit = map synifyTyVar ktvs
                           , hsq_dependent = emptyNameSet }

synifyTyVar :: TyVar -> LHsTyVarBndr Name
synifyTyVar tv
  | isLiftedTypeKind kind = noLoc (UserTyVar (noLoc name))
  | otherwise             = noLoc (KindedTyVar (noLoc name) (synifyKindSig kind))
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


synifySigType :: SynifyTypeState -> Type -> LHsSigType Name
-- The empty binders is a bit suspicious;
-- what if the type has free variables?
synifySigType s ty = mkEmptyImplicitBndrs (synifyType s ty)

synifySigWcType :: SynifyTypeState -> Type -> LHsSigWcType Name
-- Ditto (see synifySigType)
synifySigWcType s ty = mkEmptyWildCardBndrs (mkEmptyImplicitBndrs (synifyType s ty))

synifyPatSynSigType :: PatSyn -> LHsSigType Name
-- Ditto (see synifySigType)
synifyPatSynSigType ps = mkEmptyImplicitBndrs (synifyPatSynType ps)

synifyType :: SynifyTypeState -> Type -> LHsType Name
synifyType _ (TyVarTy tv) = noLoc $ HsTyVar NotPromoted $ noLoc (getName tv)
synifyType _ (TyConApp tc tys)
  -- Use */# instead of TYPE 'Lifted/TYPE 'Unlifted (#473)
  | tc `hasKey` tYPETyConKey
  , [TyConApp lev []] <- tys
  , lev `hasKey` liftedRepDataConKey
  = noLoc (HsTyVar NotPromoted (noLoc starKindTyConName))
  -- Use non-prefix tuple syntax where possible, because it looks nicer.
  | Just sort <- tyConTuple_maybe tc
  , tyConArity tc == length tys
  = noLoc $ HsTupleTy (case sort of
                          BoxedTuple      -> HsBoxedTuple
                          ConstraintTuple -> HsConstraintTuple
                          UnboxedTuple    -> HsUnboxedTuple)
                       (map (synifyType WithinType) tys)
  -- ditto for lists
  | getName tc == listTyConName, [ty] <- tys =
     noLoc $ HsListTy (synifyType WithinType ty)
  -- ditto for implicit parameter tycons
  | tc `hasKey` ipClassKey
  , [name, ty] <- tys
  , Just x <- isStrLitTy name
  = noLoc $ HsIParamTy (noLoc $ HsIPName x) (synifyType WithinType ty)
  -- and equalities
  | tc `hasKey` eqTyConKey
  , [ty1, ty2] <- tys
  = noLoc $ HsEqTy (synifyType WithinType ty1) (synifyType WithinType ty2)
  -- Most TyCons:
  | otherwise =
    foldl (\t1 t2 -> noLoc (HsAppTy t1 t2))
      (noLoc $ HsTyVar NotPromoted $ noLoc (getName tc))
      (map (synifyType WithinType) $
       filterOut isCoercionTy tys)
synifyType s (AppTy t1 (CoercionTy {})) = synifyType s t1
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
      sPhi = HsQualTy { hst_ctxt = synifyCtx ctx
                      , hst_body = synifyType WithinType tau }
  in case s of
    DeleteTopLevelQuantification -> synifyType ImplicitizeForAll tau
    WithinType        -> noLoc $ HsForAllTy { hst_bndrs = map synifyTyVar tvs
                                            , hst_body  = noLoc sPhi }
    ImplicitizeForAll -> noLoc sPhi

synifyType _ (LitTy t) = noLoc $ HsTyLit $ synifyTyLit t
synifyType s (CastTy t _) = synifyType s t
synifyType _ (CoercionTy {}) = error "synifyType:Coercion"

synifyPatSynType :: PatSyn -> LHsType Name
synifyPatSynType ps = let
  (univ_tvs, req_theta, ex_tvs, prov_theta, arg_tys, res_ty) = patSynSig ps
  req_theta' | null req_theta && not (null prov_theta && null ex_tvs) = [unitTy]
               -- HACK: a HsQualTy with theta = [unitTy] will be printed as "() =>",
               -- i.e., an explicit empty context, which is what we need. This is not
               -- possible by taking theta = [], as that will print no context at all
             | otherwise = req_theta
  sForAll []  s = s
  sForAll tvs s = HsForAllTy { hst_bndrs = map synifyTyVar tvs
                             , hst_body  = noLoc s }
  sQual theta s = HsQualTy   { hst_ctxt  = synifyCtx theta
                             , hst_body  = noLoc s }
  sTau = unLoc $ synifyType WithinType $ mkFunTys arg_tys res_ty
  in noLoc $ sForAll univ_tvs $ sQual req_theta' $ sForAll ex_tvs $ sQual prov_theta sTau

synifyTyLit :: TyLit -> HsTyLit
synifyTyLit (NumTyLit n) = HsNumTy NoSourceText n
synifyTyLit (StrTyLit s) = HsStrTy NoSourceText s

synifyKindSig :: Kind -> LHsKind Name
synifyKindSig k = synifyType WithinType k

synifyInstHead :: ([TyVar], [PredType], Class, [Type]) -> InstHead Name
synifyInstHead (_, preds, cls, types) = specializeInstHead $ InstHead
    { ihdClsName = getName cls
    , ihdKinds = map (unLoc . synifyType WithinType) ks
    , ihdTypes = map (unLoc . synifyType WithinType) ts
    , ihdInstType = ClassInst
        { clsiCtx = map (unLoc . synifyType WithinType) preds
        , clsiTyVars = synifyTyVars $ classTyVars cls
        , clsiSigs = map synifyClsIdSig $ classMethods cls
        , clsiAssocTys = do
            (Right (FamDecl fam)) <- map (synifyTyCon Nothing) $ classATs cls
            pure $ mkPseudoFamilyDecl fam
        }
    }
  where
    (ks,ts) = partitionInvisibles (classTyCon cls) id types
    synifyClsIdSig = synifyIdSig DeleteTopLevelQuantification

-- Convert a family instance, this could be a type family or data family
synifyFamInst :: FamInst -> Bool -> Either ErrMsg (InstHead Name)
synifyFamInst fi opaque = do
    ityp' <- ityp $ fi_flavor fi
    return InstHead
        { ihdClsName = fi_fam fi
        , ihdKinds = synifyTypes ks
        , ihdTypes = synifyTypes ts
        , ihdInstType = ityp'
        }
  where
    ityp SynFamilyInst | opaque = return $ TypeInst Nothing
    ityp SynFamilyInst =
        return . TypeInst . Just . unLoc . synifyType WithinType $ fi_rhs fi
    ityp (DataFamilyInst c) =
        DataInst <$> synifyTyCon (Just $ famInstAxiom fi) c
    (ks,ts) = partitionInvisibles (famInstTyCon fi) id $ fi_tys fi
    synifyTypes = map (unLoc. synifyType WithinType)
