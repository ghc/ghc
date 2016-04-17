{-# OPTIONS_GHC -fno-warn-orphans #-}
-- This module is full of orphans, unfortunately
module GHCi.TH.Binary () where

import Data.Binary
import qualified Data.ByteString as B
import Data.Typeable
import GHC.Serialized
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-- Put these in a separate module because they take ages to compile

instance Binary TH.Loc
instance Binary TH.Name
instance Binary TH.ModName
instance Binary TH.NameFlavour
instance Binary TH.PkgName
instance Binary TH.NameSpace
instance Binary TH.Module
instance Binary TH.Info
instance Binary TH.Type
instance Binary TH.TyLit
instance Binary TH.TyVarBndr
instance Binary TH.Role
instance Binary TH.Lit
instance Binary TH.Range
instance Binary TH.Stmt
instance Binary TH.Pat
instance Binary TH.Exp
instance Binary TH.Dec
instance Binary TH.Overlap
instance Binary TH.Guard
instance Binary TH.Body
instance Binary TH.Match
instance Binary TH.Fixity
instance Binary TH.TySynEqn
instance Binary TH.FamFlavour
instance Binary TH.FunDep
instance Binary TH.AnnTarget
instance Binary TH.RuleBndr
instance Binary TH.Phases
instance Binary TH.RuleMatch
instance Binary TH.Inline
instance Binary TH.Pragma
instance Binary TH.Safety
instance Binary TH.Callconv
instance Binary TH.Foreign
instance Binary TH.Bang
instance Binary TH.SourceUnpackedness
instance Binary TH.SourceStrictness
instance Binary TH.DecidedStrictness
instance Binary TH.FixityDirection
instance Binary TH.OccName
instance Binary TH.Con
instance Binary TH.AnnLookup
instance Binary TH.ModuleInfo
instance Binary TH.Clause
instance Binary TH.InjectivityAnn
instance Binary TH.FamilyResultSig
instance Binary TH.TypeFamilyHead

-- We need Binary TypeRep for serializing annotations

instance Binary TyCon where
    put tc = put (tyConPackage tc) >> put (tyConModule tc) >> put (tyConName tc)
    get = mkTyCon3 <$> get <*> get <*> get

instance Binary TypeRep where
    put type_rep = put (splitTyConApp type_rep)
    get = do
        (ty_con, child_type_reps) <- get
        return (mkTyConApp ty_con child_type_reps)

instance Binary Serialized where
    put (Serialized tyrep wds) = put tyrep >> put (B.pack wds)
    get = Serialized <$> get <*> (B.unpack <$> get)
