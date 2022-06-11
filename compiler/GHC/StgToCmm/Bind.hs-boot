module GHC.StgToCmm.Bind where

import GHC.StgToCmm.Monad( FCode, CgIdInfo )
import GHC.Stg.Syntax -- ( CgStgBinding )
import GHC.Types.Basic
import GHC.Platform
import GHC.Types.Id
import GHC.Types.CostCentre
import qualified GHC.Stack as S

cgBind :: S.HasCallStack => CgStgBinding -> FCode ()

cgTopRhsClosure :: Platform
                -> RecFlag              -- member of a recursive group?
                -> Id
                -> CostCentreStack      -- Optional cost centre annotation
                -> UpdateFlag
                -> [Id]                 -- Args
                -> CgStgExpr
                -> (CgIdInfo, FCode ())
