{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------

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
module Haddock.Convert
  ( tyThingToLHsDecl
  , synifyInstHead
  , synifyFamInst
  , synifyKindSig
  , PrintRuntimeReps (..)
  ) where

import Control.DeepSeq (force)
import Data.Either (lefts, partitionEithers, rights)
import Data.Maybe (catMaybes, mapMaybe)
import GHC.Builtin.Names
  ( boxedRepDataConKey
  , eqTyConKey
  , hasKey
  , ipClassKey
  , liftedDataConKey
  , tYPETyConKey
  )
import GHC.Builtin.Types
  ( eqTyConName
  , liftedTypeKindTyConName
  , listTyConName
  , promotedConsDataCon
  , promotedNilDataCon
  , unitTy
  )

import GHC.Builtin.Types.Prim (alphaTyVars)
import GHC.Core.Class
import GHC.Core.Coercion.Axiom
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.FamInstEnv
import GHC.Core.PatSyn
import GHC.Core.TyCo.Compare (eqTypes)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Hs
import GHC.Types.Basic (DefMethSpec (..), TupleSort (..))
import GHC.Types.Id (idType, setIdType)
import GHC.Types.Name
import GHC.Types.Name.Reader (mkVarUnqual)
import GHC.Types.Name.Set (emptyNameSet)
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.TyThing
import GHC.Types.Unique (getUnique)
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Unit.Types
import GHC.Utils.Misc
  ( chkAppend
  , dropList
  , equalLength
  , filterByList
  , filterOut, dropTail
  )
import GHC.Utils.Panic.Plain (assert)

import Haddock.GhcUtils (defaultRuntimeRepVars, mkEmptySigType, orderedFVs)
import Haddock.Interface.RenameType
import Haddock.Types

-- | Whether or not to default 'RuntimeRep' variables to 'LiftedRep'. Check
-- out Note [Defaulting RuntimeRep variables] in GHC.Iface.Type for the
-- motivation.
data PrintRuntimeReps = ShowRuntimeRep | HideRuntimeRep deriving (Show)

-- the main function here! yay!
tyThingToLHsDecl
  :: PrintRuntimeReps
  -> TyThing
  -> Either String ([String], (HsDecl GhcRn))
tyThingToLHsDecl prr t = case t of
  -- ids (functions and zero-argument a.k.a. CAFs) get a type signature.
  -- Including built-in functions like seq.
  -- foreign-imported functions could be represented with ForD
  -- instead of SigD if we wanted...
  --
  -- in a future code version we could turn idVarDetails = foreign-call
  -- into a ForD instead of a SigD if we wanted.  Haddock doesn't
  -- need to care.
  AnId i -> allOK $ SigD noExtField (synifyIdSig prr ImplicitizeForAll [] i)
  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ATyCon tc
    | Just cl <- tyConClass_maybe tc -> -- classes are just a little tedious
        let extractFamilyDecl :: TyClDecl a -> Either String (FamilyDecl a)
            extractFamilyDecl (FamDecl _ d) = return d
            extractFamilyDecl _ =
              Left "tyThingToLHsDecl: impossible associated tycon"

            cvt :: HsTyVarBndr flag GhcRn -> HsType GhcRn
            -- Without this signature, we trigger GHC#18932
            cvt (HsTvb { tvb_var = bvar, tvb_kind = bkind }) =
              case bkind of
                HsBndrNoKind _    -> cvt' bvar
                HsBndrKind _ kind -> HsKindSig noAnn (noLocA (cvt' bvar)) kind

            cvt' :: HsBndrVar GhcRn -> HsType GhcRn
            cvt' (HsBndrVar _ nm)   = HsTyVar noAnn NotPromoted (fmap noUserRdr nm)
            cvt' (HsBndrWildCard _) = HsWildCardTy noExtField

            -- \| Convert a LHsTyVarBndr to an equivalent LHsType.
            hsLTyVarBndrToType :: LHsTyVarBndr flag GhcRn -> LHsType GhcRn
            hsLTyVarBndrToType = fmap cvt

            extractFamDefDecl :: FamilyDecl GhcRn -> Type -> TyFamDefltDecl GhcRn
            extractFamDefDecl fd rhs =
              TyFamInstDecl noAnn $
                FamEqn
                  { feqn_ext = noAnn
                  , feqn_tycon = fdLName fd
                  , feqn_bndrs = HsOuterImplicit{hso_ximplicit = hsq_ext (fdTyVars fd)}
                  , feqn_pats =
                      map (HsValArg noExtField . hsLTyVarBndrToType) $
                        hsq_explicit $
                          fdTyVars fd
                  , feqn_fixity = fdFixity fd
                  , feqn_rhs = synifyType WithinType emptyVarSet rhs
                  }

            extractAtItem
              :: ClassATItem
              -> Either String (LFamilyDecl GhcRn, Maybe (LTyFamDefltDecl GhcRn))
            extractAtItem (ATI at_tc def) = do
              tyDecl <- synifyTyCon prr Nothing at_tc
              famDecl <- extractFamilyDecl tyDecl
              let defEqnTy = fmap (noLocA . extractFamDefDecl famDecl . fst) def
              pure (noLocA famDecl, defEqnTy)

            atTyClDecls = map extractAtItem (classATItems cl)
            (atFamDecls, atDefFamDecls) = unzip (rights atTyClDecls)
            vs = tyConVisibleTyVars (classTyCon cl)
         in withErrs (lefts atTyClDecls) . TyClD noExtField $
              ClassDecl
                { -- This should not always be `Just`, since `Just` of an empty
                  -- context causes pretty printing to print `()` for the
                  -- context
                  tcdCtxt =
                    case classSCTheta cl of
                      [] -> Nothing
                      th -> Just $ synifyCtx th
                , tcdLName = synifyNameN cl
                , tcdTyVars = synifyTyVars vs
                , tcdFixity = synifyFixity cl
                , tcdFDs =
                    map
                      ( \(l, r) ->
                          noLocA
                            (FunDep noAnn (map (noLocA . getName) l) (map (noLocA . getName) r))
                      )
                      $ snd
                      $ classTvsFds cl
                , tcdSigs =
                    noLocA (MinimalSig (noAnn, NoSourceText) . noLocA $ classMinimalDef cl)
                      : [ noLocA tcdSig
                        | clsOp <- classOpItems cl
                        , tcdSig <- synifyTcIdSig (mkVarSet vs) clsOp
                        ]
                , tcdMeths = [] -- ignore default method definitions, they don't affect signature
                -- class associated-types are a subset of TyCon:
                , tcdATs = atFamDecls
                , tcdATDefs = catMaybes atDefFamDecls
                , tcdDocs = [] -- we don't have any docs at this point
                , tcdCExt = emptyNameSet
                }
    | otherwise ->
        synifyTyCon prr Nothing tc >>= allOK . TyClD noExtField
  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ACoAxiom ax -> synifyAxiom ax >>= allOK
  -- a data-constructor alone just gets rendered as a function:
  AConLike (RealDataCon dc) ->
    allOK $
      SigD
        noExtField
        ( TypeSig
            noAnn
            [synifyNameN dc]
            (synifySigWcType ImplicitizeForAll [] (dataConWrapperType dc))
        )
  AConLike (PatSynCon ps) ->
    allOK . SigD noExtField $ PatSynSig noAnn [synifyNameN ps] (synifyPatSynSigType ps)
  where
    withErrs e x = return (e, x)
    allOK x = return (mempty, x)

synifyAxBranch :: TyCon -> CoAxBranch -> TyFamInstEqn GhcRn
synifyAxBranch tc (CoAxBranch{cab_tvs = tkvs, cab_lhs = args, cab_rhs = rhs}) =
  let name = synifyNameN tc
      args_types_only = filterOutInvisibleTypes tc args
      typats = map (synifyType WithinType emptyVarSet) args_types_only
      annot_typats = zipWith3 annotHsType args_poly args_types_only typats
      hs_rhs = synifyType WithinType emptyVarSet rhs
      outer_bndrs = HsOuterImplicit{hso_ximplicit = map tyVarName tkvs}
   in -- TODO: this must change eventually
      FamEqn
        { feqn_ext = noAnn
        , feqn_tycon = name
        , feqn_bndrs = outer_bndrs
        , feqn_pats = map (HsValArg noExtField) annot_typats
        , feqn_fixity = synifyFixity name
        , feqn_rhs = hs_rhs
        }
  where
    args_poly = tyConArgsPolyKinded tc

synifyAxiom :: CoAxiom br -> Either String (HsDecl GhcRn)
synifyAxiom ax@(CoAxiom{co_ax_tc = tc})
  | isOpenTypeFamilyTyCon tc
  , Just branch <- coAxiomSingleBranch_maybe ax =
      return $
        InstD noExtField $
          TyFamInstD noExtField $
            TyFamInstDecl{tfid_xtn = noAnn, tfid_eqn = synifyAxBranch tc branch}
  | Just ax' <- isClosedFamilyTyCon_maybe tc
  , getUnique ax' == getUnique ax -- without the getUniques, type error
    =
      synifyTyCon ShowRuntimeRep (Just ax) tc >>= return . TyClD noExtField
  | otherwise =
      Left "synifyAxiom: closed/open family confusion"

-- | Turn type constructors into data declarations, type families, or type synonyms
synifyTyCon
  :: PrintRuntimeReps
  -> Maybe (CoAxiom br)
  -- ^ RHS of type synonym
  -> TyCon
  -- ^ type constructor to convert
  -> Either String (TyClDecl GhcRn)
synifyTyCon prr _coax tc
  | isPrimTyCon tc =
      return $
        DataDecl
          { tcdLName = synifyNameN tc
          , tcdTyVars =
              HsQTvs
                { hsq_ext = [] -- No kind polymorphism
                , hsq_explicit =
                    zipWith
                      mk_hs_tv
                      (map scaledThing tyVarKinds)
                      alphaTyVars -- a, b, c... which are unfortunately all kind *
                }
          , tcdFixity = synifyFixity tc
          , tcdDataDefn =
              HsDataDefn
                { dd_ext = noAnn
                -- No constructors; arbitrary lie, they are neither
                -- algebraic data nor newtype:
                , dd_cons = DataTypeCons False []
                , dd_ctxt = Nothing
                , dd_cType = Nothing
                , dd_kindSig =
                    let res_kind = tyConResKind tc
                    in if isLiftedTypeKind res_kind
                       then Nothing -- Don't bother displaying :: *
                       else Just $ synifyKindSig res_kind
                , dd_derivs = []
                }
          , tcdDExt = DataDeclRn False emptyNameSet
          }
  where
    -- tyConTyVars doesn't work on fun/prim, but we can make them up:
    mk_hs_tv realKind fakeTyVar = noLocA $
      HsTvb { tvb_ext  = noAnn
            , tvb_flag = HsBndrRequired noExtField
            , tvb_var  = HsBndrVar noExtField (noLocA (getName fakeTyVar))
            , tvb_kind = if isLiftedTypeKind realKind
                         then HsBndrNoKind noExtField
                         else HsBndrKind noExtField (synifyKindSig realKind) }
    conKind = defaultType prr (tyConKind tc)
    tyVarKinds = fst . splitFunTys . snd . splitInvisPiTys $ conKind
synifyTyCon _prr _coax tc
  | Just flav <- famTyConFlav_maybe tc =
      case flav of
        -- Type families
        OpenSynFamilyTyCon -> mkFamDecl OpenTypeFamily
        ClosedSynFamilyTyCon mb
          | Just (CoAxiom{co_ax_branches = branches}) <- mb ->
              mkFamDecl $
                ClosedTypeFamily $
                  Just $
                    map (noLocA . synifyAxBranch tc) (fromBranches branches)
          | otherwise ->
              mkFamDecl $ ClosedTypeFamily $ Just []
        BuiltInSynFamTyCon{} ->
          mkFamDecl $ ClosedTypeFamily $ Just []
        AbstractClosedSynFamilyTyCon{} ->
          mkFamDecl $ ClosedTypeFamily Nothing
        DataFamilyTyCon{} ->
          mkFamDecl DataFamily
  where
    resultVar = tyConFamilyResVar_maybe tc
    mkFamDecl i =
      let (tvs, resultSig) = synifyFamilyResultSig resultVar tc
      in
      return $
        FamDecl noExtField $
          FamilyDecl
            { fdExt = noAnn
            , fdInfo = i
            , fdTopLevel = TopLevel
            , fdLName = synifyNameN tc
            , fdTyVars = tvs
            , fdFixity = synifyFixity tc
            , fdResultSig = resultSig
            , fdInjectivityAnn =
                synifyInjectivityAnn
                  resultVar
                  (tyConTyVars tc)
                  (tyConInjectivityInfo tc)
            }
synifyTyCon _prr coax tc
  -- type synonyms
  | Just ty <- synTyConRhs_maybe tc =
      return $
        SynDecl
          { tcdSExt = emptyNameSet
          , tcdLName = synifyNameN tc
          , tcdTyVars = synifyTyVars (tyConVisibleTyVars tc)
          , tcdFixity = synifyFixity tc
          , tcdRhs = synifyType WithinType emptyVarSet ty
          }
  -- (closed) newtype and data
  | otherwise = do
      let
        -- This should not always be `Just`, since `Just` of an empty
        -- context causes pretty printing to print `()` for the context
        alg_ctx =
          case tyConStupidTheta tc of
            [] -> Nothing
            th -> Just $ synifyCtx th

        -- Data families are named according to their CoAxioms, not their TyCons
        name = case coax of
          Just a -> synifyNameN a
          _ -> synifyNameN tc

        -- The kind signature.
        (tyvars, kindSig) = synifyTyConKindSig tc

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
        use_gadt_syntax = isGadtSyntaxTyCon tc

      consRaw <-
        case partitionEithers $
          synifyDataCon use_gadt_syntax
            <$> tyConDataCons tc of
          ([], cs) -> Right cs
          (errs, _) -> Left (unlines errs)

      cons <- case (isNewTyCon tc, consRaw) of
        (False, cons) -> Right (DataTypeCons False cons)
        (True, [con]) -> Right (NewTypeCon con)
        (True, _) -> Left "Newtype hasn't 1 constructor"

      let
        -- "deriving" doesn't affect the signature, no need to specify any.
        alg_deriv = []
        defn =
          HsDataDefn
            { dd_ext = noAnn
            , dd_ctxt = alg_ctx
            , dd_cType = Nothing
            , dd_kindSig = kindSig
            , dd_cons = cons
            , dd_derivs = alg_deriv
            }
      pure
        DataDecl
          { tcdLName = name
          , tcdTyVars = tyvars
          , tcdFixity = synifyFixity name
          , tcdDataDefn = defn
          , tcdDExt = DataDeclRn False emptyNameSet
          }

-- | Compute type variable binders & inline kind signature for the TyCon of a
-- data declaration.
--
-- This is only subtle when using GADT syntax;
-- see Note [Inline kind signatures with GADTSyntax].
synifyTyConKindSig :: TyCon -> (LHsQTyVars GhcRn, Maybe (LHsKind GhcRn))
synifyTyConKindSig tc
  | not $ isGadtSyntaxTyCon tc
  = let res_ki = tyConResKind tc
    in
      ( synifyTyVars (tyConVisibleTyVars tc)
      , if isLiftedTypeKind res_ki
        then Nothing
        else Just $ synifyKindSig res_ki
      )
  | otherwise
  = let
      -- Compute the prefix of user-written binders, including
      -- intervening non-user-written invisible binders.
      -- (1) in Note [Inline kind signatures with GADTSyntax]
      user_tvs = dropTail (tyConEtaBinders tc) (tyConBinders tc)
      user_vis_tvs = [ tv | Bndr tv vis <- user_tvs, isVisibleTcbVis vis ]

      -- (2) in Note [Inline kind signatures with GADTSyntax]
      user_ki = piResultTys (tyConKind tc) (map binderType user_tvs)

    in ( synifyTyVars user_vis_tvs
       , if isLiftedTypeKind user_ki
         then Nothing
         else Just $ synifyKindSig user_ki
       )

{- Note [Inline kind signatures with GADTSyntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose the user writes a declaration using GADT syntax, such as

  (G1) data Vec :: Nat -> Type -> Type where {..} -- arity 2

or

  (G2) data State s :: Effect where {..} -- arity 3
  with type synonym 'type Effect = (Type -> Type) -> Type -> Type'

The issue is that when kind-checking these declarations, GHC will invent
internal variable names (see GHC.Tc.Gen.HsType.splitTyConKind) so that there are
as many binders as the arity of the data declaration, i.e. we get

  (G1) data Vec a b where {..}
       tyConBinders = [a,b]
       tyConKind = Nat -> Type -> Type

  (G2) data State s a b where {..}
       tyConBinders = [s,a,b]
       tyConKind = Type -> Effect

So we do some fancy footwork in order to preserve the user-written kinds in
the generated Haddocks:

 (1) Filter out system names generated by GHC, e.g. 'a' and 'b' above.
     This is the purpose of the 'tyConEtaBinders' field of 'TyCon', letting us
     know how many binders were introduced by eta-expansion.

 (2) Add the return kind, but dropping one argument type for each
     user-written argument.
     For example, for (G2) we have 'State :: Type -> Effect'.
     As we have one user-written binder, we drop the initial 'Type'
     and return the inline kind signature 'Effect'.
     This is achieved using 'piResultTys'.
-}

synifyInjectivityAnn
  :: Maybe Name
  -> [TyVar]
  -> Injectivity
  -> Maybe (LInjectivityAnn GhcRn)
synifyInjectivityAnn (Just lhs) tvs (Injective inj) =
  let rhs = map (noLocA . tyVarName) (filterByList inj tvs)
   in Just $ noLocA $ InjectivityAnn noAnn (noLocA lhs) rhs
synifyInjectivityAnn _ _ _ = Nothing

synifyFamilyResultSig :: Maybe Name -> TyCon -> (LHsQTyVars GhcRn, LFamilyResultSig GhcRn)
synifyFamilyResultSig mbResVar tc = (tvs, resSig)
  where
    -- Use 'synifyTyConKindSig' as per Note [Inline kind signatures with GADTSyntax]
    (tvs, mbResKind) = synifyTyConKindSig tc
    resSig =
      case mbResVar of
        Nothing ->
          case mbResKind of
            Nothing -> noLocA $ NoSig noExtField
            Just ki -> noLocA $ KindSig noExtField ki

        -- There's a result type variable (injectivity annotation),
        -- e.g. 'type family F a b = (r :: k)'
        Just resVar ->
          let tvb =
                HsTvb
                  { tvb_ext  = noAnn
                  , tvb_flag = ()
                  , tvb_var  = HsBndrVar noExtField (noLocA resVar)
                  , tvb_kind =
                      case mbResKind of
                        Nothing -> HsBndrNoKind noExtField
                        Just ki -> HsBndrKind noExtField ki }
          in noLocA $ TyVarSig noExtField (noLocA tvb)

-- User beware: it is your responsibility to pass True (use_gadt_syntax) for any
-- constructor that would be misrepresented by omitting its result-type. But you
-- might want pass False in simple enough cases, if you think it looks better.
synifyDataCon :: Bool -> DataCon -> Either String (LConDecl GhcRn)
synifyDataCon use_gadt_syntax dc =
  let
    -- dataConIsInfix allegedly tells us whether it was declared with
    -- infix *syntax*.
    use_infix_syntax = dataConIsInfix dc
    use_named_field_syntax = not (null field_tys)
    name = synifyNameN dc
    -- con_qvars means a different thing depending on gadt-syntax
    (_univ_tvs, ex_tvs, _eq_spec, theta, arg_tys, res_ty) = dataConFullSig dc
    user_tvbndrs = dataConUserTyVarBinders dc -- Used for GADT data constructors

    split_invis_tvbs :: [TyVarBinder] -> ([InvisTVBinder], [TyVarBinder])
    split_invis_tvbs (Bndr tv (Invisible spec) : bs) =
      let ~(invis, other) = split_invis_tvbs bs
      in (Bndr tv spec : invis, other)
    split_invis_tvbs bs = ([], bs)

    split_req_tvbs :: [TyVarBinder] -> ([ReqTVBinder], [TyVarBinder])
    split_req_tvbs (Bndr tv Required : bs) =
      let ~(req, other) = split_req_tvbs bs
      in (Bndr tv () : req, other)
    split_req_tvbs bs = ([], bs)

    (outer_tvbs, inner_tvbs) = split_invis_tvbs user_tvbndrs

    outer_bndrs
      | null outer_tvbs =
          HsOuterImplicit{hso_ximplicit = []}
      | otherwise =
          HsOuterExplicit
            { hso_xexplicit = noExtField
            , hso_bndrs = map synifyTyVarBndr outer_tvbs
            }

    inner_bndrs = mk_telescopes inner_tvbs

    mk_telescopes bs
      | (invis, other) <- split_invis_tvbs bs, not (null invis)
      = mkHsForAllInvisTele noAnn (map synifyTyVarBndr invis) : mk_telescopes other

      | (req, other) <- split_req_tvbs bs, not (null req)
      = mkHsForAllVisTele noAnn (map synifyTyVarBndr req) : mk_telescopes other

      | otherwise = []

    -- skip any EqTheta, use 'orig'inal syntax
    ctx
      | null theta = Nothing
      | otherwise = Just $ synifyCtx theta

    linear_tys =
      zipWith
        ( \(Scaled mult ty) (HsSrcBang st unp str) ->
            let tySyn = synifyType WithinType emptyVarSet ty
                multSyn = synifyMultRec emptyVarSet mult
            in CDF (noAnn, st) unp str multSyn tySyn Nothing
        )
        arg_tys
        (dataConSrcBangs dc)

    field_tys = zipWith con_decl_field (dataConFieldLabels dc) linear_tys
    con_decl_field fl synTy =
      noLocA $
        HsConDeclRecField
          noExtField
          [noLocA $ FieldOcc (mkVarUnqual $ field_label $ flLabel fl) (noLocA (flSelector fl))]
          synTy

    mk_h98_arg_tys :: Either String (HsConDeclH98Details GhcRn)
    mk_h98_arg_tys = case (use_named_field_syntax, use_infix_syntax) of
      (True, True) -> Left "synifyDataCon: contradiction!"
      (True, False) -> return $ RecCon (noLocA field_tys)
      (False, False) -> return $ PrefixCon linear_tys
      (False, True) -> case linear_tys of
        [a, b] -> return $ InfixCon a b
        _ -> Left "synifyDataCon: infix with non-2 args?"

    mk_gadt_arg_tys :: HsConDeclGADTDetails GhcRn
    mk_gadt_arg_tys
      | use_named_field_syntax = RecConGADT noExtField (noLocA field_tys)
      | otherwise = PrefixConGADT noExtField linear_tys
   in
    -- finally we get synifyDataCon's result!
    if use_gadt_syntax
      then do
        let hat = mk_gadt_arg_tys
        return $
          noLocA $
            ConDeclGADT
              { con_g_ext = noExtField
              , con_names = pure name
              , con_outer_bndrs = noLocA outer_bndrs
              , con_inner_bndrs = inner_bndrs
              , con_mb_cxt = ctx
              , con_g_args = hat
              , con_res_ty = synifyType WithinType emptyVarSet res_ty
              , con_doc = Nothing
              }
      else do
        hat <- mk_h98_arg_tys
        return $
          noLocA $
            ConDeclH98
              { con_ext = noExtField
              , con_name = name
              , con_forall = False
              , con_ex_tvs = map (synifyTyVarBndr . (mkForAllTyBinder InferredSpec)) ex_tvs
              , con_mb_cxt = ctx
              , con_args = hat
              , con_doc = Nothing
              }

synifyNameN :: NamedThing n => n -> LocatedN Name
synifyNameN n = L (noAnnSrcSpan $! srcLocSpan (getSrcLoc n)) (getName n)

-- synifyName :: NamedThing n => n -> LocatedA Name
-- synifyName n = L (noAnnSrcSpan $ srcLocSpan (getSrcLoc n)) (getName n)

-- | Guess the fixity of a something with a name. This isn't quite right, since
-- a user can always declare an infix name in prefix form or a prefix name in
-- infix form. Unfortunately, that is not something we can usually reconstruct.
synifyFixity :: NamedThing n => n -> LexicalFixity
synifyFixity n
  | isSymOcc (getOccName n) = Infix
  | otherwise = Prefix

synifyIdSig
  :: PrintRuntimeReps
  -- ^ are we printing tyvars of kind 'RuntimeRep'?
  -> SynifyTypeState
  -- ^ what to do with a 'forall'
  -> [TyVar]
  -- ^ type variables bound from an outer scope
  -> Id
  -- ^ the 'Id' from which to get the type signature
  -> Sig GhcRn
synifyIdSig prr s boundTvs i = TypeSig noAnn [n] (synifySigWcType s boundTvs t)
  where
    !n = force $ synifyNameN i
    t = defaultType prr (varType i)

-- | Turn a 'ClassOpItem' into a list of signatures. The list returned is going
-- to contain the synified 'ClassOpSig' as well (when appropriate) a default
-- 'ClassOpSig'.
synifyTcIdSig :: TyVarSet -> ClassOpItem -> [Sig GhcRn]
synifyTcIdSig boundTvs (i, dm) =
  [ClassOpSig noAnn False [synifyNameN i] (mainSig (varType i))]
    ++ [ ClassOpSig noAnn True [noLocA dn] (defSig dt)
       | Just (dn, GenericDM dt) <- [dm]
       ]
  where
    mainSig t = synifySigType DeleteTopLevelQuantification boundTvs t
    defSig t = synifySigType ImplicitizeForAll boundTvs t

synifyCtx :: [PredType] -> LHsContext GhcRn
synifyCtx ts = noLocA (map (synifyType WithinType emptyVarSet) ts)

synifyTyVars :: [TyVar] -> LHsQTyVars GhcRn
synifyTyVars ktvs =
  HsQTvs
    { hsq_ext = []
    , hsq_explicit = map synifyTyVar ktvs
    }

synifyTyVar :: TyVar -> LHsTyVarBndr (HsBndrVis GhcRn) GhcRn
synifyTyVar = synify_ty_var emptyVarSet (HsBndrRequired noExtField)

synifyTyVarBndr :: VarBndr TyVar flag -> LHsTyVarBndr flag GhcRn
synifyTyVarBndr = synifyTyVarBndr' emptyVarSet

synifyTyVarBndr' :: VarSet -> VarBndr TyVar flag -> LHsTyVarBndr flag GhcRn
synifyTyVarBndr' no_kinds (Bndr tv spec) = synify_ty_var no_kinds spec tv

-- | Like 'synifyTyVarBndr', but accepts a set of variables for which to omit kind
-- signatures (even if they don't have kind 'Type').
synify_ty_var :: VarSet -> flag -> TyVar -> LHsTyVarBndr flag GhcRn
synify_ty_var no_kinds flag tv =
  noLocA (HsTvb noAnn flag bndr_var bndr_kind)
  where
    bndr_var  = HsBndrVar noExtField (noLocA name)
    bndr_kind | isLiftedTypeKind kind || tv `elemVarSet` no_kinds
              = HsBndrNoKind noExtField
              | otherwise
              = HsBndrKind noExtField (synifyKindSig kind)
    kind = tyVarKind tv
    name = getName tv

-- | Annotate (with HsKingSig) a type if the first parameter is True
-- and if the type contains a free variable.
-- This is used to synify type patterns for poly-kinded tyvars in
-- synifying class and type instances.
annotHsType
  :: Bool -- True <=> annotate
  -> Type
  -> LHsType GhcRn
  -> LHsType GhcRn
-- tiny optimization: if the type is annotated, don't annotate again.
annotHsType _ _ hs_ty@(L _ (HsKindSig{})) = hs_ty
annotHsType True ty hs_ty
  | not $ isEmptyVarSet $ filterVarSet isTyVar $ tyCoVarsOfType ty =
      let ki = typeKind ty
          hs_ki = synifyType WithinType emptyVarSet ki
       in noLocA (HsKindSig noAnn hs_ty hs_ki)
annotHsType _ _ hs_ty = hs_ty

-- | For every argument type that a type constructor accepts,
-- report whether or not the argument is poly-kinded. This is used to
-- eventually feed into 'annotThType'.
tyConArgsPolyKinded :: TyCon -> [Bool]
tyConArgsPolyKinded tc =
  map (is_poly_ty . tyVarKind) tc_vis_tvs
    ++ map (is_poly_ty . piTyBinderType) tc_res_kind_vis_bndrs
    ++ repeat True
  where
    is_poly_ty :: Type -> Bool
    is_poly_ty ty =
      not $
        isEmptyVarSet $
          filterVarSet isTyVar $
            tyCoVarsOfType ty

    tc_vis_tvs :: [TyVar]
    tc_vis_tvs = tyConVisibleTyVars tc

    tc_res_kind_vis_bndrs :: [PiTyBinder]
    tc_res_kind_vis_bndrs = filter isVisiblePiTyBinder $ fst $ splitPiTys $ tyConResKind tc

-- states of what to do with foralls:
data SynifyTypeState
  = -- | normal situation.  This is the safe one to use if you don't
    -- quite understand what's going on.
    WithinType
  | -- | beginning of a function definition, in which, to make it look
    --   less ugly, those rank-1 foralls (without kind annotations) are made
    --   implicit.
    ImplicitizeForAll
  | -- | because in class methods the context is added to the type
    --   (e.g. adding @forall a. Num a =>@ to @(+) :: a -> a -> a@)
    --   which is rather sensible,
    --   but we want to restore things to the source-syntax situation where
    --   the defining class gets to quantify all its functions for free!
    DeleteTopLevelQuantification

synifySigType :: SynifyTypeState -> TyVarSet -> Type -> LHsSigType GhcRn
-- The use of mkEmptySigType (which uses empty binders in OuterImplicit)
-- is a bit suspicious; what if the type has free variables?
synifySigType s boundTvs ty = mkEmptySigType (synifyType s boundTvs ty)

synifySigWcType :: SynifyTypeState -> [TyVar] -> Type -> LHsSigWcType GhcRn
-- Ditto (see synifySigType)
synifySigWcType s vs ty =
  mkEmptyWildCardBndrs (mkEmptySigType (rename (map getName vs) $ synifyType s (mkVarSet vs) ty))

synifyPatSynSigType :: PatSyn -> LHsSigType GhcRn
-- Ditto (see synifySigType)
synifyPatSynSigType ps = mkEmptySigType (synifyPatSynType ps)

-- | Depending on the first argument, try to default all type variables of kind
-- 'RuntimeRep' to 'LiftedType'.
defaultType :: PrintRuntimeReps -> Type -> Type
defaultType ShowRuntimeRep = id
defaultType HideRuntimeRep = defaultRuntimeRepVars

-- | Convert a core type into an 'HsType'.
synifyType
  :: SynifyTypeState
  -- ^ what to do with a 'forall'
  -> TyVarSet
  -- ^ bound type variables
  -> Type
  -- ^ the type to convert
  -> LHsType GhcRn
synifyType _ _ (TyVarTy tv) = noLocA $ HsTyVar noAnn NotPromoted $ noLocA (noUserRdr $ getName tv)
synifyType _ boundTvs (TyConApp tc tys) =
  maybe_sig res_ty
  where
    res_ty :: LHsType GhcRn
    res_ty
      -- Use */# instead of TYPE 'Lifted/TYPE 'Unlifted (#473)
      | tc `hasKey` tYPETyConKey
      , [TyConApp rep [TyConApp lev []]] <- tys
      , rep `hasKey` boxedRepDataConKey
      , lev `hasKey` liftedDataConKey =
          noLocA (HsTyVar noAnn NotPromoted (noLocA $ noUserRdr liftedTypeKindTyConName))
      -- Use non-prefix tuple syntax where possible, because it looks nicer.
      | Just sort <- tyConTuple_maybe tc
      , tyConArity tc == tys_len =
          noLocA $
            HsTupleTy
              noAnn
              ( case sort of
                  BoxedTuple -> HsBoxedOrConstraintTuple
                  ConstraintTuple -> HsBoxedOrConstraintTuple
                  UnboxedTuple -> HsUnboxedTuple
              )
              (map (synifyType WithinType boundTvs) vis_tys)
      | isUnboxedSumTyCon tc =
          noLocA $ HsSumTy noAnn (map (synifyType WithinType boundTvs) vis_tys)
      | Just dc <- isPromotedDataCon_maybe tc
      , isTupleDataCon dc
      , dataConSourceArity dc == length vis_tys =
          noLocA $ HsExplicitTupleTy noExtField IsPromoted (map (synifyType WithinType boundTvs) vis_tys)
      -- ditto for lists
      | getName tc == listTyConName
      , [ty] <- vis_tys =
          noLocA $ HsListTy noAnn (synifyType WithinType boundTvs ty)
      | tc == promotedNilDataCon
      , [] <- vis_tys =
          noLocA $ HsExplicitListTy noExtField IsPromoted []
      | tc == promotedConsDataCon
      , [ty1, ty2] <- vis_tys =
          let hTy = synifyType WithinType boundTvs ty1
           in case synifyType WithinType boundTvs ty2 of
                tTy
                  | L _ (HsExplicitListTy _ IsPromoted tTy') <- stripKindSig tTy ->
                      noLocA $ HsExplicitListTy noExtField IsPromoted (hTy : tTy')
                  | otherwise ->
                      noLocA $ HsOpTy noExtField IsPromoted hTy (noLocA $ noUserRdr $ getName tc) tTy
      -- ditto for implicit parameter tycons
      | tc `hasKey` ipClassKey
      , [name, ty] <- tys
      , Just x <- isStrLitTy name =
          noLocA $ HsIParamTy noAnn (noLocA $ HsIPName x) (synifyType WithinType boundTvs ty)
      -- and equalities
      | tc `hasKey` eqTyConKey
      , [ty1, ty2] <- tys =
          noLocA $
            HsOpTy
              noExtField
              NotPromoted
              (synifyType WithinType boundTvs ty1)
              (noLocA $ noUserRdr eqTyConName)
              (synifyType WithinType boundTvs ty2)
      -- and infix type operators
      | isSymOcc (nameOccName (getName tc))
      , ty1 : ty2 : tys_rest <- vis_tys =
          mk_app_tys
            ( HsOpTy
                noExtField
                prom
                (synifyType WithinType boundTvs ty1)
                (noLocA $ noUserRdr $ getName tc)
                (synifyType WithinType boundTvs ty2)
            )
            tys_rest
      -- Most TyCons:
      | otherwise =
          mk_app_tys
            (HsTyVar noAnn prom $ noLocA (noUserRdr $ getName tc))
            vis_tys
      where
        !prom = if isPromotedDataCon tc then IsPromoted else NotPromoted
        mk_app_tys ty_app ty_args =
          foldl
            (\t1 t2 -> noLocA $ HsAppTy noExtField t1 t2)
            (noLocA ty_app)
            ( map (synifyType WithinType boundTvs) $
                filterOut isCoercionTy ty_args
            )

    tys_len = length tys
    vis_tys = filterOutInvisibleTypes tc tys

    maybe_sig :: LHsType GhcRn -> LHsType GhcRn
    maybe_sig ty'
      | tyConAppNeedsKindSig False tc tys_len =
          let full_kind = typeKind (mkTyConApp tc tys)
              full_kind' = synifyType WithinType boundTvs full_kind
           in noLocA $ HsKindSig noAnn ty' full_kind'
      | otherwise = ty'
synifyType _ boundTvs ty@(AppTy{}) =
  let
    (ty_head, ty_args) = splitAppTys ty
    ty_head' = synifyType WithinType boundTvs ty_head
    ty_args' =
      map (synifyType WithinType boundTvs) $
        filterOut isCoercionTy $
          filterByList
            (map isVisibleForAllTyFlag $ appTyForAllTyFlags ty_head ty_args)
            ty_args
   in
    foldl (\t1 t2 -> noLocA $ HsAppTy noExtField t1 t2) ty_head' ty_args'
synifyType s boundTvs funty@(FunTy af w t1 t2)
  | isInvisibleFunArg af = synifySigmaType s boundTvs funty
  | otherwise = noLocA $ HsFunTy noExtField w' s1 s2
  where
    s1 = synifyType WithinType boundTvs t1
    s2 = synifyType WithinType boundTvs t2
    w' = synifyMultArrow boundTvs w
synifyType s boundTvs forallty@(ForAllTy (Bndr _ argf) _ty) =
  case argf of
    Required -> synifyVisForAllType boundTvs forallty
    Invisible _ -> synifySigmaType s boundTvs forallty
synifyType _ _ (LitTy t) = noLocA $ HsTyLit noExtField $ synifyTyLit t
synifyType s boundTvs (CastTy t _) = synifyType s boundTvs t
synifyType _ _ (CoercionTy{}) = error "synifyType:Coercion"

-- | Process a 'Type' which starts with a visible @forall@ into an 'HsType'
synifyVisForAllType
  :: TyVarSet
  -- ^ bound type variables
  -> Type
  -- ^ the forall type to convert
  -> LHsType GhcRn
synifyVisForAllType boundTvs ty =
  let (tvs, rho) = tcSplitForAllTysReqPreserveSynonyms ty

      sTvs = map (synifyTyVarBndr' noKindSigTvs) tvs
      noKindSigTvs = noKindSigTyVars ty

      -- Figure out what the type variable order would be inferred in the
      -- absence of an explicit forall
      tvs' = orderedFVs boundTvs [rho]
   in noLocA $
        HsForAllTy
          { hst_tele = mkHsForAllVisTele noAnn sTvs
          , hst_xforall = noExtField
          , hst_body = synifyType WithinType (extendVarSetList boundTvs tvs') rho
          }

-- | Process a 'Type' which starts with an invisible @forall@ or a constraint
-- into an 'HsType'
synifySigmaType
  :: SynifyTypeState
  -- ^ what to do with the 'forall'
  -> TyVarSet
  -- ^ bound type variables
  -> Type
  -- ^ the forall type to convert
  -> LHsType GhcRn
synifySigmaType s boundTvs ty =
  let (tvs, ctx, tau) = tcSplitSigmaTyPreserveSynonyms ty
      sPhi =
        HsQualTy
          { hst_ctxt = synifyCtx ctx
          , hst_xqual = noExtField
          , hst_body = synifyType WithinType (extendVarSetList boundTvs tvs' ) tau
          }

      sTy =
        HsForAllTy
          { hst_tele = mkHsForAllInvisTele noAnn sTvs
          , hst_xforall = noExtField
          , hst_body = noLocA sPhi
          }

      sTvs = map (synifyTyVarBndr' noKindSigTvs) tvs

      noKindSigTvs = noKindSigTyVars ty

      -- Figure out what the type variable order would be inferred in the
      -- absence of an explicit forall
      tvs' = orderedFVs boundTvs (ctx ++ [tau])
   in case s of
        DeleteTopLevelQuantification -> synifyType ImplicitizeForAll (extendVarSetList boundTvs tvs') tau
        -- Put a forall in if there are any type variables
        WithinType
          | not (null tvs) -> noLocA sTy
          | otherwise -> noLocA sPhi
        ImplicitizeForAll -> implicitForAll boundTvs tvs ctx (synifyType WithinType) tau

-- | Use an explicit forall if there are any type variables which require
-- explicit kind annotations or if the inferred type variable quantification
-- order would be different.
implicitForAll
  :: TyVarSet
  -- ^ bound type variables (e.g. bound from an outer scope)
  -> [InvisTVBinder]
  -- ^ type variable binders in the forall
  -> ThetaType
  -- ^ constraints right after the forall
  -> (TyVarSet -> Type -> LHsType GhcRn)
  -- ^ how to convert the inner type
  -> Type
  -- ^ inner type
  -> LHsType GhcRn
implicitForAll boundTvs tvbs ctx synInner tau
  | any (isHsKindedTyVar . unLoc) sTvs
  -- Explicit forall: some type variable needs an explicit kind annotation.
  = noLocA sTy
  | tvs /= inferredFreeTvs
  -- Explicit forall: the inferred quantification order would be different.
  = noLocA sTy
  | otherwise
  -- Implicit forall.
  = noLocA sPhi
  where
    tvs = binderVars tvbs
    sRho = synInner (extendVarSetList boundTvs inferredFreeTvs) tau
    sPhi
      | null ctx = unLoc sRho
      | otherwise =
          HsQualTy
            { hst_ctxt = synifyCtx ctx
            , hst_xqual = noExtField
            , hst_body = sRho
            }
    sTy =
      HsForAllTy
        { hst_tele = mkHsForAllInvisTele noAnn sTvs
        , hst_xforall = noExtField
        , hst_body = noLocA sPhi
        }

    no_kinds_needed = noKindSigTyVars tau
    sTvs = map (synifyTyVarBndr' no_kinds_needed) tvbs

    -- Figure out what the type variable order would be inferred in the
    -- absence of an explicit forall
    inferredFreeTvs = orderedFVs boundTvs (ctx ++ [tau])

-- | Returns a subset of the free type variables of the given type whose kinds
-- can definitely be inferred from their occurrences in the type.
--
-- This function is only a simple heuristic, which is used in order to avoid
-- needlessly cluttering Haddocks with explicit foralls that are not needed.
-- This function may return some type variables for which we aren't sure
-- (which will cause us to display the type with an explicit forall, just in
-- case).
--
-- In the future, we hope to address the issue of whether to print a type with
-- an explicit forall by storing whether the user wrote the type with an
-- explicit forall in the first place (see GHC ticket #26271).
noKindSigTyVars
  :: Type
  -- ^ type to inspect
  -> VarSet
  -- ^ set of variables whose kinds can definitely be inferred from occurrences in the type
noKindSigTyVars ty
  | Just ty' <- coreView ty
  = noKindSigTyVars ty'
  -- In a TyConApp 'T ty_1 ... ty_n', if 'ty_i = tv' is a type variable and the
  -- i-th argument of the kind of 'T' is monomorphic, then the kind of 'tv'
  -- is fully determined by its occurrence in the TyConApp.
  | Just (tc, args) <- splitTyConApp_maybe ty
  , let (tcArgBndrs, _tcResKi) = splitPiTys (tyConKind tc)
        tcArgKis = map (\case { Named (Bndr b _) -> tyVarKind b; Anon (Scaled _ t) _ -> t}) tcArgBndrs
  = mono_tvs tcArgKis args `unionVarSet` (mapUnionVarSet noKindSigTyVars args)
  -- If we have 'f ty_1 ... ty_n' where 'f :: ki_1 -> ... -> ki_n -> Type'
  -- then we can infer the kind of 'f' from the kinds of its arguments.
  --
  -- This special case handles common examples involving functors, monads...
  -- with type signatures such as '(a -> b) -> (f a -> f b)'.
  | (TyVarTy fun, args) <- splitAppTys ty
  , not (null args)
  , (funArgKinds, funResKind) <- splitFunTys (tyVarKind fun)
  , map scaledThing funArgKinds `eqTypes` map typeKind args
  , isLiftedTypeKind funResKind
  = ( `extendVarSet` fun ) $ mapUnionVarSet noKindSigTyVars args
  where
    mono_tvs :: [Type] -> [Type] -> VarSet
    mono_tvs (tcArgKi:tcArgKis) (arg:args)
      | TyVarTy arg_tv <- arg
      , noFreeVarsOfType tcArgKi
      = ( `extendVarSet` arg_tv ) $ mono_tvs tcArgKis args
      | otherwise
      = mono_tvs tcArgKis args
    mono_tvs _ _ = emptyVarSet
noKindSigTyVars (ForAllTy _ t) = noKindSigTyVars t
noKindSigTyVars (CastTy t _) = noKindSigTyVars t
noKindSigTyVars _ = emptyVarSet

synifyMultArrow :: TyVarSet -> Mult -> HsMultAnn GhcRn
synifyMultArrow boundTvs t = case t of
  OneTy -> HsLinearAnn noExtField
  ManyTy -> HsUnannotated noExtField
  ty -> HsExplicitMult noExtField (synifyType WithinType boundTvs ty)

synifyMultRec :: TyVarSet -> Mult -> HsMultAnn GhcRn
synifyMultRec boundTvs t = case t of
  OneTy -> HsUnannotated noExtField
  ty -> HsExplicitMult noExtField (synifyType WithinType boundTvs ty)

synifyPatSynType :: PatSyn -> LHsType GhcRn
synifyPatSynType ps =
  let (univ_tvbs, req_theta, ex_tvbs, prov_theta, arg_tys, res_ty) = patSynSigBndr ps

{- Recall that pattern synonyms have both "required" and "provided" constraints,
e.g.

  pattern P :: forall a b c. req => forall e f g => prov => arg_ty1 -> ... -> res_ty

Here:

  a, b, c are universal type variables
  req are required constraints

  e, f, g are existential type variables
  prov are provided constraints

The first pair comes from the outside, while the second pair is obtained upon
a successful match on the pattern.

Remarks:

  1. Both foralls are optional.

  2. If there is only one =>, we interpret the constraints as required.
     Thus, if we want an empty set of required constraints and a non-empty set
     of provided constraints, the type signature must be written like

       () => prov => res_ty
-}


      -- Add an explicit "() => ..." when req_theta is empty but there are
      -- existential variables or provided constraints.
      req_theta'
        | null req_theta
        , not (null prov_theta && null ex_tvbs) =
            [unitTy]
        | otherwise = req_theta
      univ_tvs = mkVarSet $ binderVars univ_tvbs
      ex_tvs = mkVarSet $ binderVars ex_tvbs


   in implicitForAll
        ex_tvs    -- consider the ex_tvs non-free, so that we don't quantify over them here
        univ_tvbs -- quantify only over the universals
        req_theta'
        ( \_ ->
          implicitForAll
            univ_tvs -- the univ_tvs are already bound
            ex_tvbs  -- quantify only over the existentials
            prov_theta
            (synifyType WithinType)
        )
        (mkScaledFunTys arg_tys res_ty)

synifyTyLit :: TyLit -> HsLit GhcRn
synifyTyLit (NumTyLit n) = HsNatural noExtField (mkIntegralLit n)
synifyTyLit (StrTyLit s) = HsString NoSourceText s
synifyTyLit (CharTyLit c) = HsChar NoSourceText c

synifyKindSig :: Kind -> LHsKind GhcRn
synifyKindSig k = synifyType WithinType emptyVarSet k

stripKindSig :: LHsType GhcRn -> LHsType GhcRn
stripKindSig (L _ (HsKindSig _ t _)) = t
stripKindSig t = t

synifyInstHead :: ([TyVar], [PredType], Class, [Type]) -> [(FamInst, Bool, Maybe (MDoc Name), Located Name, Maybe Module)] -> InstHead GhcRn
synifyInstHead (vs, preds, cls, types) associated_families =
  InstHead
    { ihdClsName = getName cls
    , ihdTypes = map unLoc annot_ts
    , ihdInstType =
        ClassInst
          { clsiCtx = map (unLoc . synifyType WithinType emptyVarSet) preds
          , clsiTyVars = synifyTyVars (tyConVisibleTyVars cls_tycon)
          , clsiSigs = map synifyClsIdSig $ specialized_class_methods
          , clsiAssocTys =
              [ (f_inst, f_doc, f_name, f_mod)
              | (f_i, opaque, f_doc, f_name, f_mod) <- associated_families
              , Right f_inst <- [synifyFamInst f_i opaque]
              ]
          }
    }
  where
    cls_tycon = classTyCon cls
    ts = filterOutInvisibleTypes cls_tycon types
    ts' = map (synifyType WithinType $ mkVarSet vs) ts
    annot_ts = zipWith3 annotHsType args_poly ts ts'
    args_poly = tyConArgsPolyKinded cls_tycon
    synifyClsIdSig = synifyIdSig ShowRuntimeRep DeleteTopLevelQuantification vs
    specialized_class_methods = [setIdType m (piResultTys (idType m) types) | m <- classMethods cls]

-- Convert a family instance, this could be a type family or data family
synifyFamInst :: FamInst -> Bool -> Either String (InstHead GhcRn)
synifyFamInst fi opaque = do
  ityp' <- ityp fam_flavor
  return
    InstHead
      { ihdClsName = fi_fam fi
      , ihdTypes = map unLoc annot_ts
      , ihdInstType = ityp'
      }
  where
    ityp SynFamilyInst | opaque = return $ TypeInst Nothing
    ityp SynFamilyInst =
      return . TypeInst . Just . unLoc $ synifyType WithinType emptyVarSet fam_rhs
    ityp (DataFamilyInst c) =
      DataInst <$> synifyTyCon HideRuntimeRep (Just $ famInstAxiom fi) c
    fam_tc = famInstTyCon fi
    fam_flavor = fi_flavor fi
    fam_lhs = fi_tys fi
    fam_rhs = fi_rhs fi

    eta_expanded_lhs
      -- eta-expand lhs types, because sometimes data/newtype
      -- instances are eta-reduced; See Trac #9692
      -- See Note [Eta reduction for data family axioms] in GHC.Tc.TyCl.Instance in GHC
      | DataFamilyInst rep_tc <- fam_flavor =
          let (_, rep_tc_args) = splitTyConApp fam_rhs
              etad_tyvars = dropList rep_tc_args $ tyConTyVars rep_tc
              etad_tys = mkTyVarTys etad_tyvars
              eta_exp_lhs = fam_lhs `chkAppend` etad_tys
           in eta_exp_lhs
      | otherwise =
          fam_lhs

    ts = filterOutInvisibleTypes fam_tc eta_expanded_lhs
    synifyTypes = map (synifyType WithinType emptyVarSet)
    ts' = synifyTypes ts
    annot_ts = zipWith3 annotHsType args_poly ts ts'
    args_poly = tyConArgsPolyKinded fam_tc

{-
Note [Invariant: Never expand type synonyms]

In haddock, we never want to expand a type synonym that may be presented to the
user, as we want to keep the link to the abstraction captured in the synonym.

All code in Haddock.Convert must make sure that this invariant holds.

See https://github.com/haskell/haddock/issues/879 for a bug where this
invariant didn't hold.
-}

-- | A version of 'TcType.tcSplitSigmaTy' that:
--
-- 1. Preserves type synonyms.
-- 2. Returns 'InvisTVBinder's instead of 'TyVar's.
--
-- See Note [Invariant: Never expand type synonyms]
tcSplitSigmaTyPreserveSynonyms :: Type -> ([InvisTVBinder], ThetaType, Type)
tcSplitSigmaTyPreserveSynonyms ty =
  case tcSplitForAllTysInvisPreserveSynonyms ty of
    (tvs, rho) -> case tcSplitPhiTyPreserveSynonyms rho of
      (theta, tau) -> (tvs, theta, tau)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitSomeForAllTysPreserveSynonyms
  :: (ForAllTyFlag -> Bool) -> Type -> ([ForAllTyBinder], Type)
tcSplitSomeForAllTysPreserveSynonyms argf_pred ty = split ty ty []
  where
    split _ (ForAllTy tvb@(Bndr _ argf) ty') tvs
      | argf_pred argf = split ty' ty' (tvb : tvs)
    split orig_ty _ tvs = (reverse tvs, orig_ty)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitForAllTysReqPreserveSynonyms :: Type -> ([ReqTVBinder], Type)
tcSplitForAllTysReqPreserveSynonyms ty =
  let (all_bndrs, body) = tcSplitSomeForAllTysPreserveSynonyms isVisibleForAllTyFlag ty
      req_bndrs = mapMaybe mk_req_bndr_maybe all_bndrs
   in assert
        (req_bndrs `equalLength` all_bndrs)
        (req_bndrs, body)
  where
    mk_req_bndr_maybe :: ForAllTyBinder -> Maybe ReqTVBinder
    mk_req_bndr_maybe (Bndr tv argf) = case argf of
      Required -> Just $ Bndr tv ()
      Invisible _ -> Nothing

-- | See Note [Invariant: Never expand type synonyms]
tcSplitForAllTysInvisPreserveSynonyms :: Type -> ([InvisTVBinder], Type)
tcSplitForAllTysInvisPreserveSynonyms ty =
  let (all_bndrs, body) = tcSplitSomeForAllTysPreserveSynonyms isInvisibleForAllTyFlag ty
      inv_bndrs = mapMaybe mk_inv_bndr_maybe all_bndrs
   in assert
        (inv_bndrs `equalLength` all_bndrs)
        (inv_bndrs, body)
  where
    mk_inv_bndr_maybe :: ForAllTyBinder -> Maybe InvisTVBinder
    mk_inv_bndr_maybe (Bndr tv argf) = case argf of
      Invisible s -> Just $ Bndr tv s
      Required -> Nothing

-- | See Note [Invariant: Never expand type synonyms]

-- | See Note [Invariant: Never expand type synonyms]
tcSplitPhiTyPreserveSynonyms :: Type -> (ThetaType, Type)
tcSplitPhiTyPreserveSynonyms ty0 = split ty0 []
  where
    split ty ts =
      case tcSplitPredFunTyPreserveSynonyms_maybe ty of
        Just (pred_, ty') -> split ty' (pred_ : ts)
        Nothing -> (reverse ts, ty)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitPredFunTyPreserveSynonyms_maybe :: Type -> Maybe (PredType, Type)
tcSplitPredFunTyPreserveSynonyms_maybe (FunTy af _ arg res)
  | isInvisibleFunArg af = Just (arg, res)
tcSplitPredFunTyPreserveSynonyms_maybe _ = Nothing
