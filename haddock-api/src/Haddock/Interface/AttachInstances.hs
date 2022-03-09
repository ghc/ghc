{-# LANGUAGE MagicHash, BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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

import Control.Applicative ((<|>))
import Control.Arrow hiding ((<+>))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe ( maybeToList, mapMaybe, fromMaybe )
import qualified Data.Map as Map
import qualified Data.Set as Set

import GHC.Data.FastString (unpackFS)
import GHC.Core.Class
import GHC.Core (isOrphan)
import GHC.Core.FamInstEnv
import GHC
import GHC.Core.InstEnv
import GHC.Unit.Module.Env ( ModuleSet, moduleSetElts )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Utils.Outputable (text, sep, (<+>))
import GHC.Types.SrcLoc
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Builtin.Types.Prim( funTyConName )
import GHC.Types.Var hiding (varName)
import GHC.HsToCore.Docs

type ExportedNames = Set.Set Name
type Modules = Set.Set Module
type ExportInfo = (ExportedNames, Modules)

-- Also attaches fixities
attachInstances :: ExportInfo -> [Interface] -> InstIfaceMap -> ModuleSet -> Ghc [Interface]
attachInstances expInfo ifaces instIfaceMap mods = do
  (_msgs, mb_index) <- getNameToInstancesIndex (map ifaceMod ifaces) mods'
  mapM (attach $ fromMaybe emptyNameEnv mb_index) ifaces
  where
    mods' = Just (moduleSetElts mods)

    -- TODO: take an IfaceMap as input
    ifaceMap = Map.fromList [ (ifaceMod i, i) | i <- ifaces ]

    attach index iface = do

      let getInstDoc = findInstDoc iface ifaceMap instIfaceMap
          getFixity = findFixity iface ifaceMap instIfaceMap

      newItems <- mapM (attachToExportItem index expInfo getInstDoc getFixity)
                       (ifaceExportItems iface)
      let orphanInstances = attachOrphanInstances expInfo getInstDoc (ifaceInstances iface)
      return $ iface { ifaceExportItems = newItems
                     , ifaceOrphanInstances = orphanInstances
                     }

attachOrphanInstances
  :: ExportInfo
  -> (Name -> Maybe (MDoc Name))      -- ^ how to lookup the doc of an instance
  -> [ClsInst]                        -- ^ a list of orphan instances
  -> [DocInstance GhcRn]
attachOrphanInstances expInfo getInstDoc cls_instances =
  [ (synifyInstHead i, getInstDoc n, (L (getSrcSpan n) n), Nothing)
  | let is = [ (instanceSig i, getName i) | i <- cls_instances, isOrphan (is_orphan i) ]
  , (i@(_,_,cls,tys), n) <- sortBy (comparing $ first instHead) is
  , not $ isInstanceHidden expInfo cls tys
  ]


attachToExportItem
  :: NameEnv ([ClsInst], [FamInst])   -- ^ all instances (that we know of)
  -> ExportInfo
  -> (Name -> Maybe (MDoc Name))      -- ^ how to lookup the doc of an instance
  -> (Name -> Maybe Fixity)           -- ^ how to lookup a fixity
  -> ExportItem GhcRn
  -> Ghc (ExportItem GhcRn)
attachToExportItem index expInfo getInstDoc getFixity export =
  case attachFixities export of
    e@ExportDecl { expItemDecl = L eSpan (TyClD _ d) } -> do
      insts <-
        let mb_instances  = lookupNameEnv index (tcdName d)
            cls_instances = maybeToList mb_instances >>= fst
            fam_instances = maybeToList mb_instances >>= snd
            fam_insts = [ ( synFamInst
                          , getInstDoc n
                          , spanNameE n synFamInst (L (locA eSpan) (tcdName d))
                          , nameModule_maybe n
                          )
                        | i <- sortBy (comparing instFam) fam_instances
                        , let n = getName i
                        , not $ isNameHidden expInfo (fi_fam i)
                        , not $ any (isTypeHidden expInfo) (fi_tys i)
                        , let opaque = isTypeHidden expInfo (fi_rhs i)
                        , let synFamInst = synifyFamInst i opaque
                        ]
            cls_insts = [ ( synClsInst
                          , getInstDoc n
                          , spanName n synClsInst (L (locA eSpan) (tcdName d))
                          , nameModule_maybe n
                          )
                        | let is = [ (instanceSig i, getName i) | i <- cls_instances ]
                        , (i@(_,_,cls,tys), n) <- sortBy (comparing $ first instHead) is
                        , not $ isInstanceHidden expInfo cls tys
                        , let synClsInst = synifyInstHead i
                        ]
              -- fam_insts but with failing type fams filtered out
            cleanFamInsts = [ (fi, n, L l r, m) | (Right fi, n, L l (Right r), m) <- fam_insts ]
            famInstErrs = [ errm | (Left errm, _, _, _) <- fam_insts ]
        in do
          let mkBug = (text "haddock-bug:" <+>) . text
          putMsgM (sep $ map mkBug famInstErrs)
          return $ cls_insts ++ cleanFamInsts
      return $ e { expItemInstances = insts }
    e -> return e
  where
    attachFixities e@ExportDecl{ expItemDecl = L _ d
                               , expItemPats = patsyns
                               , expItemSubDocs = subDocs
                               } = e { expItemFixities =
      nubByName fst $ expItemFixities e ++
      [ (n',f) | n <- getMainDeclBinder emptyOccEnv d
               , n' <- n : (map fst subDocs ++ patsyn_names)
               , f <- maybeToList (getFixity n')
      ] }
      where
        patsyn_names = concatMap (getMainDeclBinder emptyOccEnv . fst) patsyns

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

-- | Lookup the doc associated with a certain instance
findInstDoc :: Interface -> IfaceMap -> InstIfaceMap -> Name -> Maybe (MDoc Name)
findInstDoc iface ifaceMap instIfaceMap = \name ->
  (Map.lookup name . ifaceDocMap $ iface) <|>
  (Map.lookup name . ifaceDocMap =<< Map.lookup (nameModule name) ifaceMap) <|>
  (Map.lookup name . instDocMap =<< Map.lookup (nameModule name) instIfaceMap)

-- | Lookup the fixity associated with a certain name
findFixity :: Interface -> IfaceMap -> InstIfaceMap -> Name -> Maybe Fixity
findFixity iface ifaceMap instIfaceMap = \name ->
  (Map.lookup name . ifaceFixMap $ iface) <|>
  (Map.lookup name . ifaceFixMap =<< Map.lookup (nameModule name) ifaceMap) <|>
  (Map.lookup name . instFixMap =<< Map.lookup (nameModule name) instIfaceMap)


--------------------------------------------------------------------------------
-- Collecting and sorting instances
--------------------------------------------------------------------------------

-- | Stable name for stable comparisons. GHC's `Name` uses unstable
-- ordering based on their `Unique`'s.
newtype SName = SName Name

instance Eq SName where
  SName n1 == SName n2 = n1 `stableNameCmp` n2 == EQ

instance Ord SName where
  SName n1 `compare` SName n2 = n1 `stableNameCmp` n2

-- | Simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
-- For the benefit of the user (looks nice and predictable) and the
-- tests (which prefer output to be deterministic).
data SimpleType = SimpleType SName [SimpleType]
                | SimpleIntTyLit Integer
                | SimpleStringTyLit String
                | SimpleCharTyLit Char
                  deriving (Eq,Ord)


instHead :: ([TyVar], [PredType], Class, [Type]) -> ([Int], SName, [SimpleType])
instHead (_, _, cls, args)
  = (map argCount args, SName (className cls), map simplify args)

argCount :: Type -> Int
argCount (AppTy t _)     = argCount t + 1
argCount (TyConApp _ ts) = length ts
argCount (FunTy _ _ _ _) = 2
argCount (ForAllTy _ t)  = argCount t
argCount (CastTy t _)    = argCount t
argCount _ = 0

simplify :: Type -> SimpleType
simplify (FunTy _ _ t1 t2)  = SimpleType (SName funTyConName) [simplify t1, simplify t2]
simplify (ForAllTy _ t) = simplify t
simplify (AppTy t1 t2) = SimpleType s (ts ++ maybeToList (simplify_maybe t2))
  where (SimpleType s ts) = simplify t1
simplify (TyVarTy v) = SimpleType (SName (tyVarName v)) []
simplify (TyConApp tc ts) = SimpleType (SName (tyConName tc))
                                       (mapMaybe simplify_maybe ts)
simplify (LitTy (NumTyLit n)) = SimpleIntTyLit n
simplify (LitTy (StrTyLit s)) = SimpleStringTyLit (unpackFS s)
simplify (LitTy (CharTyLit c)) = SimpleCharTyLit c
simplify (CastTy ty _) = simplify ty
simplify (CoercionTy _) = error "simplify:Coercion"

simplify_maybe :: Type -> Maybe SimpleType
simplify_maybe (CoercionTy {}) = Nothing
simplify_maybe ty              = Just (simplify ty)

-- Used for sorting
instFam :: FamInst -> ([Int], SName, [SimpleType], Int, SimpleType)
instFam FamInst { fi_fam = n, fi_tys = ts, fi_rhs = t }
  = (map argCount ts, SName n, map simplify ts, argCount t, simplify t)


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
        FunTy _ _ t1 t2 -> typeHidden t1 || typeHidden t2
        TyConApp tcon args -> nameHidden (getName tcon) || any typeHidden args
        ForAllTy bndr ty -> typeHidden (tyVarKind (binderVar bndr)) || typeHidden ty
        LitTy _ -> False
        CastTy ty _ -> typeHidden ty
        CoercionTy {} -> False

    nameHidden :: Name -> Bool
    nameHidden = isNameHidden expInfo
