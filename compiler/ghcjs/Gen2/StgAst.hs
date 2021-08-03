{-
   some instances for printing the StgSyn AST in Haskell syntax.
-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gen2.StgAst where

import           Data.Char     (isSpace)
import qualified Data.Foldable as F
import           Data.Set      (Set)
import qualified Data.Set      as S
import           DataCon
import           DynFlags
import Prelude

import           BasicTypes
-- import           Control.Lens
import           Compiler.JMacro.Lens
import           CoreSyn
import           CostCentre
import           ForeignCall
import           Id
import           Literal
import           Module
import           Name
import           Outputable    hiding ((<>))
import           PrimOp
import           StgSyn
import           TyCon
import           Type
import           Unique
import           UniqFM
import           IdInfo
import qualified Var

import           Coercion
import           CoAxiom
import           Gen2.Utils

-- this is a hack to be able to use pprShow in a Show instance, should be removed
{-# NOINLINE hackPprDflags #-}
hackPprDflags :: DynFlags
hackPprDflags = unsafeGlobalDynFlags

-- | replace all whitespace with space
fixSpace :: String -> String
fixSpace xs = map f xs
  where
    f c | isSpace c = ' '
        | otherwise = c


-- fixme make this more informative
instance Show Type where
  show ty = fixSpace (showPpr hackPprDflags ty)
instance Show CostCentre where show _ = "CostCentre"
instance Show CostCentreStack where show _ = "CostCentreStack"
instance Show Module where show m = unitIdString (moduleUnitId m) ++ ":" ++ moduleNameString (moduleName m)
instance Show TyCon where show = show . tyConName
instance Show NoExtFieldSilent where show _ = "NoExtFieldSilent"
instance Show Name where
  show n = case nameModule_maybe n of
                  Nothing -> show (nameOccName n)
                  Just m  -> show m ++ "." ++ show (nameOccName n)
instance Show OccName where show = occNameString
instance Show DataCon where show d = show (dataConName d)
instance Show Var where show v = "(" ++ show (Var.varName v) ++ "[" ++
                                 encodeUnique (getKey (getUnique v)) ++
                                 "]" ++ if isGlobalId v then "G" else "L" ++
                                 " <" ++ show (idDetails v) ++ "> :: " ++
                                 show (Var.varType v) ++ ")"
instance Show IdDetails where
  show VanillaId         = "VanillaId"
  show RecSelId {}       = "RecSelId"
  show (DataConWorkId dc) = "DataConWorkId " ++ show dc
  show (DataConWrapId dc) = "DataConWrapId " ++ show dc
  show ClassOpId {}      = "ClassOpId"
  show PrimOpId {}       = "PrimOpId"
  show FCallId {}        = "FCallId"
  show TickBoxOpId {}    = "VanillaId"
  show DFunId {}         = "DFunId"
  show CoVarId           = "CoVarId"
  show JoinId {}         = "JoinId"

deriving instance Show UpdateFlag
deriving instance Show PrimOpVecCat
deriving instance Show LitNumType
deriving instance Show Literal
deriving instance Show PrimOp
deriving instance Show AltCon
deriving instance Show AltType
deriving instance Show PrimCall
deriving instance Show ForeignCall
deriving instance Show CCallTarget
deriving instance Show CCallSpec
deriving instance Show CCallConv
deriving instance Show FunctionOrData
deriving instance Show StgExpr
deriving instance Show StgBinding
deriving instance Show StgTopBinding
deriving instance Show StgRhs
deriving instance Show StgOp
deriving instance Show a => Show (Tickish a)
--
instance Show Coercion where show co = showPpr hackPprDflags co
deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Bind a)
instance Show CoAxiomRule where show _ = "CoAxiomRule"
instance Show (CoAxiom a) where show _ = "CoAxiom"
deriving instance Show LeftOrRight
deriving instance Show Role
instance Show StgArg where
  show a@(StgVarArg occ) = "StgVarArg " ++ show occ ++ " :: " ++ show (stgArgType a)
  show (StgLitArg l)   = "StgLitArg " ++ show l
deriving instance Show UnfoldingGuidance
deriving instance Show UnfoldingSource
deriving instance Show Unfolding


s :: a -> Set a
s = S.singleton

l :: (a -> Set Id) -> [a] -> Set Id
l = F.foldMap

-- | collect Ids that this binding refers to
--   (does not include the bindees themselves)
-- first argument is Id -> StgExpr map for unfloated arguments
bindingRefs :: UniqFM StgExpr -> StgBinding -> Set Id
bindingRefs u (StgNonRec _ rhs) = rhsRefs u rhs
bindingRefs u (StgRec bs)       = l (rhsRefs u . snd) bs

rhsRefs :: UniqFM StgExpr -> StgRhs -> Set Id
rhsRefs u (StgRhsClosure _ _ _ _ body) = exprRefs u body
rhsRefs u (StgRhsCon _ d args) = l s [ i | AnId i <- dataConImplicitTyThings d] <> l (argRefs u) args

exprRefs :: UniqFM StgExpr -> StgExpr -> Set Id
exprRefs u (StgApp f args) = s f <> l (argRefs u) args
exprRefs u (StgConApp d args _) = l s [ i | AnId i <- dataConImplicitTyThings d] <> l (argRefs u) args
exprRefs u (StgOpApp _ args _) = l (argRefs u) args
exprRefs _  StgLit {} = mempty
exprRefs _  StgLam {} = mempty
exprRefs u (StgCase expr _ _ alts) = exprRefs u expr <> alts^.folded._3.to (exprRefs u)
exprRefs u (StgLet _ bnd expr) = bindingRefs u bnd <> exprRefs u expr
exprRefs u (StgLetNoEscape _ bnd expr) = bindingRefs u bnd <> exprRefs u expr
exprRefs u (StgTick _ expr) = exprRefs u expr

argRefs :: UniqFM StgExpr -> StgArg -> Set Id
argRefs u (StgVarArg id)
  | Just e <- lookupUFM u id = exprRefs u e
  | otherwise                = s id
argRefs _ _ = mempty
