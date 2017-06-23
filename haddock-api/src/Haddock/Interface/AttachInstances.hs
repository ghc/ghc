{-# LANGUAGE CPP, MagicHash #-}
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
-----------------------------------------------------------------------------
module Haddock.Interface.AttachInstances (attachInstances) where


import Haddock.Types
import Haddock.Convert
import Haddock.GhcUtils

import Control.Applicative
import Control.Arrow hiding ((<+>))
import Data.List
import Data.Ord (comparing)
import Data.Maybe ( maybeToList, mapMaybe, fromMaybe )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Class
import DynFlags
import CoreSyn (isOrphan)
import ErrUtils
import FamInstEnv
import FastString
import GHC
import InstEnv
import MonadUtils (liftIO)
import Name
import NameEnv
import Outputable (text, sep, (<+>))
import PrelNames
import SrcLoc
import TyCon
import TyCoRep
import TysPrim( funTyCon )
import Var hiding (varName)
#define FSLIT(x) (mkFastString# (x#))

type ExportedNames = Set.Set Name
type Modules = Set.Set Module
type ExportInfo = (ExportedNames, Modules)

-- Also attaches fixities
attachInstances :: ExportInfo -> [Interface] -> InstIfaceMap -> Ghc [Interface]
attachInstances expInfo ifaces instIfaceMap = do
  (_msgs, mb_index) <- getNameToInstancesIndex
  mapM (attach $ fromMaybe emptyNameEnv mb_index) ifaces
  where
    -- TODO: take an IfaceMap as input
    ifaceMap = Map.fromList [ (ifaceMod i, i) | i <- ifaces ]

    attach index iface = do
      newItems <- mapM (attachToExportItem index expInfo iface ifaceMap instIfaceMap)
                       (ifaceExportItems iface)
      let orphanInstances = attachOrphanInstances expInfo iface ifaceMap instIfaceMap (ifaceInstances iface)
      return $ iface { ifaceExportItems = newItems
                     , ifaceOrphanInstances = orphanInstances
                     }

attachOrphanInstances :: ExportInfo -> Interface -> IfaceMap -> InstIfaceMap -> [ClsInst] -> [DocInstance Name]
attachOrphanInstances expInfo iface ifaceMap instIfaceMap cls_instances =
  [ (synifyInstHead i, instLookup instDocMap n iface ifaceMap instIfaceMap, (L (getSrcSpan n) n))
  | let is = [ (instanceSig i, getName i) | i <- cls_instances, isOrphan (is_orphan i) ]
  , (i@(_,_,cls,tys), n) <- sortBy (comparing $ first instHead) is
  , not $ isInstanceHidden expInfo cls tys
  ]


attachToExportItem
  :: NameEnv ([ClsInst], [FamInst])
  -> ExportInfo
  -> Interface
  -> IfaceMap
  -> InstIfaceMap
  -> ExportItem Name
  -> Ghc (ExportItem Name)
attachToExportItem index expInfo iface ifaceMap instIfaceMap export =
  case attachFixities export of
    e@ExportDecl { expItemDecl = L eSpan (TyClD d) } -> do
      insts <-
        let mb_instances  = lookupNameEnv index (tcdName d)
            cls_instances = maybeToList mb_instances >>= fst
            fam_instances = maybeToList mb_instances >>= snd
            fam_insts = [ (synifyFamInst i opaque, doc,spanNameE n (synifyFamInst i opaque) (L eSpan (tcdName d)) )
                        | i <- sortBy (comparing instFam) fam_instances
                        , let n = getName i
                        , let doc = instLookup instDocMap n iface ifaceMap instIfaceMap
                        , not $ isNameHidden expInfo (fi_fam i)
                        , not $ any (isTypeHidden expInfo) (fi_tys i)
                        , let opaque = isTypeHidden expInfo (fi_rhs i)
                        ]
            cls_insts = [ (synifyInstHead i, instLookup instDocMap n iface ifaceMap instIfaceMap, spanName n (synifyInstHead i) (L eSpan (tcdName d)))
                        | let is = [ (instanceSig i, getName i) | i <- cls_instances ]
                        , (i@(_,_,cls,tys), n) <- sortBy (comparing $ first instHead) is
                        , not $ isInstanceHidden expInfo cls tys
                        ]
              -- fam_insts but with failing type fams filtered out
            cleanFamInsts = [ (fi, n, L l r) | (Right fi, n, L l (Right r)) <- fam_insts ]
            famInstErrs = [ errm | (Left errm, _, _) <- fam_insts ]
        in do
          dfs <- getDynFlags
          let mkBug = (text "haddock-bug:" <+>) . text
          liftIO $ putMsg dfs (sep $ map mkBug famInstErrs)
          return $ cls_insts ++ cleanFamInsts
      return $ e { expItemInstances = insts }
    e -> return e
  where
    attachFixities e@ExportDecl{ expItemDecl = L _ d
                               , expItemPats = patsyns
                               } = e { expItemFixities =
      nubByName fst $ expItemFixities e ++
      [ (n',f) | n <- getMainDeclBinder d
              , Just subs <- [instLookup instSubMap n iface ifaceMap instIfaceMap <|> Just []]
              , n' <- n : (subs ++ patsyn_names)
              , Just f <- [instLookup instFixMap n' iface ifaceMap instIfaceMap]
      ] }
      where
        patsyn_names = concatMap (getMainDeclBinder . fst) patsyns

    attachFixities e = e
    -- spanName: attach the location to the name that is the same file as the instance location
    spanName s (InstHead { ihdClsName = clsn }) (L instL instn) =
        let s1 = getSrcSpan s
            sn = if srcSpanFileName_maybe s1 == srcSpanFileName_maybe instL
                    then instn
                    else clsn
        in L (getSrcSpan s) sn
    -- spanName on Either
    spanNameE s (Left e) _ =  L (getSrcSpan s) (Left e)
    spanNameE s (Right ok) linst =
      let L l r = spanName s ok linst
      in L l (Right r)


instLookup :: (InstalledInterface -> Map.Map Name a) -> Name
            -> Interface -> IfaceMap -> InstIfaceMap -> Maybe a
instLookup f name iface ifaceMap instIfaceMap =
  case Map.lookup name (f $ toInstalledIface iface) of
    res@(Just _) -> res
    Nothing -> do
      let ifaceMaps = Map.union (fmap toInstalledIface ifaceMap) instIfaceMap
      iface' <- Map.lookup (nameModule name) ifaceMaps
      Map.lookup name (f iface')

--------------------------------------------------------------------------------
-- Collecting and sorting instances
--------------------------------------------------------------------------------


-- | Simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
-- For the benefit of the user (looks nice and predictable) and the
-- tests (which prefer output to be deterministic).
data SimpleType = SimpleType Name [SimpleType]
                | SimpleTyLit TyLit
                  deriving (Eq,Ord)


instHead :: ([TyVar], [PredType], Class, [Type]) -> ([Int], Name, [SimpleType])
instHead (_, _, cls, args)
  = (map argCount args, className cls, map simplify args)

argCount :: Type -> Int
argCount (AppTy t _)     = argCount t + 1
argCount (TyConApp _ ts) = length ts
argCount (FunTy _ _ )    = 2
argCount (ForAllTy _ t)  = argCount t
argCount (CastTy t _)    = argCount t
argCount _ = 0

simplify :: Type -> SimpleType
simplify (FunTy t1 t2)  = SimpleType funTyConName [simplify t1, simplify t2]
simplify (ForAllTy _ t) = simplify t
simplify (AppTy t1 t2) = SimpleType s (ts ++ maybeToList (simplify_maybe t2))
  where (SimpleType s ts) = simplify t1
simplify (TyVarTy v) = SimpleType (tyVarName v) []
simplify (TyConApp tc ts) = SimpleType (tyConName tc)
                                       (mapMaybe simplify_maybe ts)
simplify (LitTy l) = SimpleTyLit l
simplify (CastTy ty _) = simplify ty
simplify (CoercionTy _) = error "simplify:Coercion"

simplify_maybe :: Type -> Maybe SimpleType
simplify_maybe (CoercionTy {}) = Nothing
simplify_maybe ty              = Just (simplify ty)

-- Used for sorting
instFam :: FamInst -> ([Int], Name, [SimpleType], Int, SimpleType)
instFam FamInst { fi_fam = n, fi_tys = ts, fi_rhs = t }
  = (map argCount ts, n, map simplify ts, argCount t, simplify t)


funTyConName :: Name
funTyConName = mkWiredInName gHC_PRIM
                        (mkOccNameFS tcName FSLIT("(->)"))
                        funTyConKey
                        (ATyCon funTyCon)       -- Relevant TyCon
                        BuiltInSyntax

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
  nameModule name `Set.member` modules &&
  not (name `Set.member` names)

-- | We say that an instance is «hidden» iff its class or any (part)
-- of its type(s) is hidden.
isInstanceHidden :: ExportInfo -> Class -> [Type] -> Bool
isInstanceHidden expInfo cls tys =
    instClassHidden || instTypeHidden
  where
    instClassHidden :: Bool
    instClassHidden = isNameHidden expInfo $ getName cls

    instTypeHidden :: Bool
    instTypeHidden = any (isTypeHidden expInfo) tys

isTypeHidden :: ExportInfo -> Type -> Bool
isTypeHidden expInfo = typeHidden
  where
    typeHidden :: Type -> Bool
    typeHidden t =
      case t of
        TyVarTy {} -> False
        AppTy t1 t2 -> typeHidden t1 || typeHidden t2
        FunTy t1 t2 -> typeHidden t1 || typeHidden t2
        TyConApp tcon args -> nameHidden (getName tcon) || any typeHidden args
        ForAllTy bndr ty -> typeHidden (tyVarKind (binderVar bndr)) || typeHidden ty
        LitTy _ -> False
        CastTy ty _ -> typeHidden ty
        CoercionTy {} -> False

    nameHidden :: Name -> Bool
    nameHidden = isNameHidden expInfo
