{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Interface.AttachInstances
-- Copyright   :  (c) Simon Marlow 2006,
--                    David Waern  2006-2009,
--                    Isaac Dupree 2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Interface.AttachInstances (attachInstances, instHead) where

import Control.Applicative ((<|>))
import Control.Arrow hiding ((<+>))
import Control.DeepSeq (force)
import Control.Monad (unless)
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC
import GHC.Builtin.Types (unrestrictedFunTyConName)
import GHC.Core (isOrphan)
import GHC.Core.Class
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Env.Types
import GHC.HsToCore.Docs
import GHC.Iface.Load
import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.SrcLoc
import GHC.Types.Unique.Map
import GHC.Types.Var hiding (varName)
import GHC.Unit.Env
import GHC.Unit.Module.Env (mkModuleSet, moduleSetElts)
import GHC.Unit.State
import GHC.Utils.Outputable (sep, text, (<+>))

import Haddock.Convert
import Haddock.GhcUtils (typeNames)
import Haddock.Types

type ExportedNames = Set.Set Name
type Modules = Set.Set Module
type ExportInfo = (ExportedNames, Modules)

-- Also attaches fixities
attachInstances :: ExportInfo -> [Interface] -> InstIfaceMap -> Bool -> Ghc [Interface]
attachInstances expInfo ifaces instIfaceMap isOneShot = do
  -- We need to keep load modules in which we will look for instances. We've
  -- somewhat arbitrarily decided to load all modules which are available -
  -- either directly or from a re-export.
  --
  -- See https://github.com/haskell/haddock/issues/469.
  env <- getSession
  let mod_to_pkg_conf = moduleNameProvidersMap $ ue_homeUnitState $ hsc_unit_env env
      mods =
        mkModuleSet
          [ m
          | mod_map <- nonDetEltsUniqMap mod_to_pkg_conf
          , ( m
              , ModOrigin
                  { fromOrigUnit = fromOrig
                  , fromExposedReexport = reExp
                  }
              ) <-
              nonDetUniqMapToList mod_map
          , fromOrig == Just True || not (null reExp)
          ]
      mods_to_load = moduleSetElts mods
      mods_visible = mkModuleSet $ map ifaceMod ifaces

  (_msgs, mb_index) <- do
    hsc_env <- getSession
    liftIO $ runTcInteractive hsc_env $ do
      -- In one shot mode we don't want to load anything more than is already loaded
      unless isOneShot $ do
        let doc = text "Need interface for haddock"
        initIfaceTcRn $ mapM_ (loadSysInterface doc) mods_to_load
      cls_env@InstEnvs{ie_global, ie_local} <- tcGetInstEnvs
      fam_env@(pkg_fie, home_fie) <- tcGetFamInstEnvs
      -- We use Data.Sequence.Seq because we are creating left associated
      -- mappends.
      -- cls_index and fam_index below are adapted from GHC.Tc.Module.lookupInsts
      let cls_index =
            Map.fromListWith
              mappend
              [ (n, Seq.singleton ispec)
              | ispec <- instEnvElts ie_local ++ instEnvElts ie_global
              , instIsVisible mods_visible ispec
              , n <- nameSetElemsStable $ orphNamesOfClsInst ispec
              ]
          fam_index =
            Map.fromListWith
              mappend
              [ (n, Seq.singleton fispec)
              | fispec <- famInstEnvElts home_fie ++ famInstEnvElts pkg_fie
              , n <- nameSetElemsStable $ orphNamesOfFamInst fispec
              ]
          instance_map =
            mkNameEnv $
              [ (nm, (toList clss, toList fams))
              | (nm, (clss, fams)) <-
                  Map.toList $
                    Map.unionWith
                      mappend
                      (fmap (,Seq.empty) cls_index)
                      (fmap (Seq.empty,) fam_index)
              ]
      pure $ (cls_env{ie_visible = mods_visible}, fam_env, instance_map)

  let empty_index = (InstEnvs emptyInstEnv emptyInstEnv mods_visible, emptyFamInstEnvs, emptyNameEnv)
  mapM (attach $ fromMaybe empty_index mb_index) ifaces
  where
    -- TODO: take an IfaceMap as input
    ifaceMap = Map.fromList [(ifaceMod i, i) | i <- ifaces]

    attach (cls_insts, fam_insts, inst_map) iface = do
      let getInstDoc = findInstDoc iface ifaceMap instIfaceMap
          getFixity = findFixity iface ifaceMap instIfaceMap
          getInstLocIface name = Map.lookup name . instInstanceLocMap =<< Map.lookup (nameModule name) instIfaceMap

      newItems <-
        mapM
          (attachToExportItem cls_insts fam_insts inst_map expInfo getInstDoc getFixity getInstLocIface)
          (ifaceExportItems iface)
      let orphanInstances = attachOrphanInstances expInfo getInstDoc (ifaceInstances iface) fam_insts
      return $
        iface
          { ifaceExportItems = newItems
          , ifaceOrphanInstances = orphanInstances
          }

attachOrphanInstances
  :: ExportInfo
  -> (Name -> Maybe (MDoc Name))
  -- ^ how to lookup the doc of an instance
  -> [ClsInst]
  -- ^ a list of orphan instances
  -> FamInstEnvs
  -- ^ all the family instances (that we know of)
  -> [DocInstance GhcRn]
attachOrphanInstances expInfo getInstDoc cls_instances fam_index =
  [ (synifyInstHead i famInsts, getInstDoc n, (L (getSrcSpan n) n), nameModule_maybe n)
  | let is = [(instanceSig i, getName i) | i <- cls_instances, isOrphan (is_orphan i)]
  , (i@(_, _, cls, tys), n) <- List.sortBy (comparing $ first instHead) is
  , not $ isInstanceHidden expInfo (getName cls) tys
  , let famInsts = getFamInsts expInfo fam_index getInstDoc cls tys
  ]

attachToExportItem
  :: InstEnvs
  -- ^ all class instances (that we know of)
  -> FamInstEnvs
  -- ^ all the family instances (that we know of)
  -> NameEnv ([ClsInst], [FamInst])
  -- ^ all instances again, but for looking up instances for data families
  -> ExportInfo
  -> (Name -> Maybe (MDoc Name))
  -- ^ how to lookup the doc of an instance
  -> (Name -> Maybe Fixity)
  -- ^ how to lookup a fixity
  -> (Name -> Maybe RealSrcSpan)
  -- ^ how to lookup definition spans for instances
  -> ExportItem GhcRn
  -> Ghc (ExportItem GhcRn)
attachToExportItem cls_index fam_index index expInfo getInstDoc getFixity getInstLocIface export =
  case attachFixities export of
    ExportDecl e@(ExportD{expDDecl = L eSpan (TyClD _ d)}) -> do
      insts <-
        let nm = tcdName d
            (cls_instances, fam_instances) = case d of
              -- For type classes we can be more efficient by looking up the class in the inst map
              ClassDecl{} -> (classNameInstances cls_index nm, familyNameInstances fam_index nm)
              -- Otherwise, we have to filter through all the instances to see if they mention this
              -- name. See GHCi :info implementation
              _ -> fromMaybe ([], []) $ lookupNameEnv index nm

            fam_insts =
              [ ( synFamInst
                , getInstDoc n
                , spanNameE n synFamInst (L (locA eSpan) (tcdName d))
                , mb_mdl
                )
              | i <- List.sortBy (comparing instFam) fam_instances
              , let n = getName i
              , not $ isNameHidden expInfo (fi_fam i)
              , not $ any (isTypeHidden expInfo) (fi_tys i)
              , let opaque = isTypeHidden expInfo (fi_rhs i)
                    synFamInst = synifyFamInst i opaque
                    !mb_mdl = force $ nameModule_maybe n
              ]
            cls_insts =
              [ ( synClsInst
                , getInstDoc n
                , spanName n synClsInst (L (locA eSpan) (tcdName d))
                , mb_mdl
                )
              | let is = [(instanceSig i, getName i) | i <- cls_instances]
              , (i@(_, _, cls, tys), n) <- List.sortBy (comparing $ first instHead) is
              , not $ isInstanceHidden expInfo (getName cls) tys
              , let synClsInst = synifyInstHead i famInsts
                    famInsts = getFamInsts expInfo fam_index getInstDoc cls tys
                    !mb_mdl = force $ nameModule_maybe n
              ]
            -- fam_insts but with failing type fams filtered out
            cleanFamInsts = [(fi, n, L l r, m) | (Right fi, n, L l (Right r), m) <- fam_insts]
            famInstErrs = [errm | (Left errm, _, _, _) <- fam_insts]
         in do
              let mkBug = (text "haddock-bug:" <+>) . text
              putMsgM (sep $ map mkBug famInstErrs)
              return $ cls_insts ++ cleanFamInsts
      return $ ExportDecl e{expDInstances = insts}
    e -> return e
  where
    attachFixities
      ( ExportDecl
          ( e@ExportD
              { expDDecl = L _ d
              , expDPats = patsyns
              , expDSubDocs = subDocs
              }
            )
        ) =
        ExportDecl
          e
            { expDFixities = fixities
            }
        where
          fixities :: [(Name, Fixity)]
          !fixities = force . Map.toList $ List.foldl' f Map.empty all_names

          f :: Map.Map Name Fixity -> Name -> Map.Map Name Fixity
          f !fs n = Map.alter (<|> getFixity n) n fs

          patsyn_names :: [Name]
          patsyn_names = concatMap (getMainDeclBinder emptyOccEnv . fst) patsyns

          all_names :: [Name]
          all_names =
            getMainDeclBinder emptyOccEnv d
              ++ map fst subDocs
              ++ patsyn_names
    attachFixities e = e

    -- spanName: attach the location to the name that is the same file as the instance location
    spanName s (InstHead{ihdClsName = clsn}) (L instL instn) =
      let s1 = let orig_span = getSrcSpan s
               in if isGoodSrcSpan orig_span
                  then orig_span
                  else case getInstLocIface s of
                         Nothing -> orig_span
                         Just rs -> RealSrcSpan rs mempty
          sn =
            if srcSpanFileName_maybe s1 == srcSpanFileName_maybe instL
              then instn
              else clsn
       in L s1 sn
    -- spanName on Either
    spanNameE s (Left e) _ = L (getSrcSpan s) (Left e)
    spanNameE s (Right ok) linst =
      let L l r = spanName s ok linst
       in L l (Right r)

substAgrees :: [(TyVar, Type)] -> [(TyVar, Type)] -> Bool
substAgrees xs ys = go xs
  where
    go [] = True
    go ((v, t1) : zs) = case lookup v ys of
      Nothing -> go zs
      Just t2 -> eqType t1 t2 && go zs

getFamInsts
  :: ExportInfo
  -> FamInstEnvs
  -- ^ all the family instances (that we know of)
  -> (Name -> Maybe (MDoc Name))
  -- ^ how to lookup the doc of an instance
  -> Class
  -> [Type]
  -> [(FamInst, Bool, Maybe (MDoc Name), Located Name, Maybe Module)]
getFamInsts expInfo fam_index getInstDoc cls tys =
  [ (f_i, opaque, getInstDoc f_n, L (getSrcSpan f_n) f_n, nameModule_maybe f_n)
  | fam <- classATs cls
  , let vars = tyConTyVars fam
        tv_env = zip (classTyVars cls) tys
        m_instantiation = mapM (\v -> lookup v tv_env) vars
  , f_i <- case m_instantiation of
      -- If we have a complete instantation, we can just lookup in the family environment
      Just instantiation -> map fim_instance $ lookupFamInstEnv fam_index fam instantiation
      -- If we don't have a complete instantation, we need to look over all possible instances
      -- for the family and filter out the ones that don't agree with the typeclass instance
      Nothing ->
        [ f_i
        | f_i <- familyInstances fam_index fam
        , let co_tvs = tyConTyVars fam
              (_, lhs, _) = etaExpandCoAxBranch $ coAxiomSingleBranch $ fi_axiom f_i
        , substAgrees (zip co_tvs lhs) tv_env
        ]
  , let ax = fi_axiom f_i
        f_n = co_ax_name ax
  , not $ isNameHidden expInfo (fi_fam f_i)
  , not $ any (isTypeHidden expInfo) (fi_tys f_i)
  , let opaque = isTypeHidden expInfo (fi_rhs f_i)
  ]

-- | Lookup the doc associated with a certain instance
findInstDoc :: Interface -> IfaceMap -> InstIfaceMap -> Name -> Maybe (MDoc Name)
findInstDoc iface ifaceMap instIfaceMap = \name ->
  (Map.lookup name . ifaceDocMap $ iface)
    <|> (Map.lookup name . ifaceDocMap =<< Map.lookup (nameModule name) ifaceMap)
    <|> (Map.lookup name . instDocMap =<< Map.lookup (nameModule name) instIfaceMap)

-- | Lookup the fixity associated with a certain name
findFixity :: Interface -> IfaceMap -> InstIfaceMap -> Name -> Maybe Fixity
findFixity iface ifaceMap instIfaceMap = \name ->
  (Map.lookup name . ifaceFixMap $ iface)
    <|> (Map.lookup name . ifaceFixMap =<< Map.lookup (nameModule name) ifaceMap)
    <|> (Map.lookup name . instFixMap =<< Map.lookup (nameModule name) instIfaceMap)

--------------------------------------------------------------------------------
-- Collecting and sorting instances
--------------------------------------------------------------------------------

instHead :: ([TyVar], [PredType], Class, [Type]) -> ([Int], SName, [SimpleType])
instHead (_, _, cls, args) =
  (map argCount args, SName (className cls), map simplify args)

argCount :: Type -> Int
argCount (AppTy t _) = argCount t + 1
argCount (TyConApp _ ts) = length ts
argCount (FunTy _ _ _) = 2
argCount (ForAllTy _ t) = argCount t
argCount (CastTy t _) = argCount t
argCount _ = 0

simplify :: Type -> SimpleType
simplify (FunTy _ t1 t2) = SimpleType (SName unrestrictedFunTyConName) [simplify t1, simplify t2]
simplify (ForAllTy _ t) = simplify t
simplify (AppTy t1 t2) = SimpleType s (ts ++ maybeToList (simplify_maybe t2))
  where
    (SimpleType s ts) = simplify t1
simplify (TyVarTy v) = SimpleType (SName (tyVarName v)) []
simplify (TyConApp tc ts) =
  SimpleType
    (SName (tyConName tc))
    (mapMaybe simplify_maybe ts)
simplify (LitTy (NumTyLit n)) = SimpleIntTyLit n
simplify (LitTy (StrTyLit s)) = SimpleStringTyLit (unpackFS s)
simplify (LitTy (CharTyLit c)) = SimpleCharTyLit c
simplify (CastTy ty _) = simplify ty
simplify (CoercionTy _) = error "simplify:Coercion"

simplify_maybe :: Type -> Maybe SimpleType
simplify_maybe (CoercionTy{}) = Nothing
simplify_maybe ty = Just (simplify ty)

-- Used for sorting
instFam :: FamInst -> ([Int], SName, [SimpleType], Int, SimpleType)
instFam FamInst{fi_fam = n, fi_tys = ts, fi_rhs = t} =
  (map argCount ts, SName n, map simplify ts, argCount t, simplify t)

--------------------------------------------------------------------------------
-- Filtering hidden instances
--------------------------------------------------------------------------------

-- | A class or data type is hidden iff
--
-- * it is defined in one of the modules that are being processed
--
-- * and it is not exported by any non-hidden module
isNameHidden :: ExportInfo -> Name -> Bool
isNameHidden (names, modules) name =
  nameModule name `Set.member` modules
    && not (name `Set.member` names)

-- | We say that an instance is «hidden» iff its class or any (part)
-- of its type(s) is hidden.
isInstanceHidden :: ExportInfo -> Name -> [Type] -> Bool
isInstanceHidden expInfo cls tyNames =
  instClassHidden || instTypeHidden
  where
    instClassHidden :: Bool
    instClassHidden = isNameHidden expInfo cls

    instTypeHidden :: Bool
    instTypeHidden = any (isTypeHidden expInfo) tyNames

isTypeHidden :: ExportInfo -> Type -> Bool
isTypeHidden expInfo = typeHidden
  where
    typeHidden :: Type -> Bool
    typeHidden t = any nameHidden $ typeNames t

    nameHidden :: Name -> Bool
    nameHidden = isNameHidden expInfo
