-- debug start
import GHC.Core.Coercion.Axiom

import GHC.Types.Id.Info
import GHC.Core.TyCo.Rep
import qualified GHC.Types.Var as Var
-- import qualified GHC.Types.CostCentre as CC
import GHC.Hs.Extension

-- | replace all whitespace with space
fixSpace :: String -> String
fixSpace xs = map f xs
  where
    f c | isSpace c = ' '
        | otherwise = c

render :: Outputable a => a -> String
render doc = renderWithContext defaultSDocContext (ppr doc)

-- fixme make this more informative
instance Show Type where
  show ty = fixSpace (render ty)
instance Show CostCentre where show _ = "CostCentre"
instance Show CostCentreStack where show _ = "CostCentreStack"
instance Show Module where show m = moduleStableString m
  -- unitIdString (moduleUnit m) ++ ":" ++ moduleNameString (moduleName m)
instance Show TyCon where show = show . tyConName
instance Show NoExtFieldSilent where show _ = "NoExtFieldSilent"
instance Show Name where
  show n = case nameModule_maybe n of
                  Nothing -> show (nameOccName n)
                  Just m  -> show m ++ "." ++ show (nameOccName n)
instance Show OccName where show = occNameString
instance Show DataCon where show d = show (dataConName d)
instance Show Var where show v = "(" ++ show (Var.varName v) ++ "[" ++
                                 render (getKey (getUnique v)) ++
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
instance Show CC.CostCentreStack where show _ = "{CCS}"
instance Show CC.CostCentre where show _ = "{CC}"
deriving instance Show StgExpr
deriving instance Show StgBinding
deriving instance Show StgTopBinding
deriving instance Show StgRhs
deriving instance Show StgOp

deriving instance Show CgStgExpr
deriving instance Show CgStgBinding
deriving instance Show CgStgTopBinding
deriving instance Show CgStgRhs

deriving instance Show Tickish
deriving instance Show StgTickish

--
instance Show Coercion where show co = render co
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
deriving instance Show NoExtField

-- instance Show DIdSet where show _ = "{DIdSet}"
instance Show DIdSet where show = show . dVarSetElems
{-
traceCBC :: String -> BcM ()
traceCBC msg
  | True      = pure ()
  | otherwise = ioToBc (putStrLn msg)
-}