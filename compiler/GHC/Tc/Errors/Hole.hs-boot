-- This boot file is in place to break the loop where:
-- + GHC.Tc.Solver calls 'GHC.Tc.Errors.reportUnsolved',
-- + which calls 'GHC.Tc.Errors.Hole.findValidHoleFits`
-- + which calls 'GHC.Tc.Solver.simpl_top'
module GHC.Tc.Errors.Hole where

import GHC.Types.Var ( Id )
import GHC.Tc.Types  ( TcM )
import GHC.Tc.Types.Constraint ( CtEvidence, CtLoc, Hole, Implication )
import GHC.Utils.Outputable ( SDoc )
import GHC.Types.Var.Env ( TidyEnv )
import GHC.Tc.Errors.Hole.FitTypes ( HoleFit, TypedHole, HoleFitCandidate )
import GHC.Tc.Utils.TcType ( TcType, TcSigmaType, TcTyVar )
import GHC.Tc.Types.Evidence ( HsWrapper )
import GHC.Utils.FV ( FV )
import Data.Bool ( Bool )
import Data.Maybe ( Maybe )
import Data.Int ( Int )

findValidHoleFits :: TidyEnv -> [Implication] -> [CtEvidence] -> Hole
                  -> TcM (TidyEnv, SDoc)

tcCheckHoleFit :: TypedHole -> TcSigmaType -> TcSigmaType
               -> TcM (Bool, HsWrapper)

withoutUnification :: FV -> TcM a -> TcM a
tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
tcFilterHoleFits :: Maybe Int -> TypedHole -> (TcType, [TcTyVar])
                 -> [HoleFitCandidate] -> TcM (Bool, [HoleFit])
getLocalBindings :: TidyEnv -> CtLoc -> TcM [Id]
addHoleFitDocs :: [HoleFit] -> TcM [HoleFit]

data HoleFitDispConfig
data HoleFitSortingAlg

pprHoleFit :: HoleFitDispConfig -> HoleFit -> SDoc
getHoleFitSortingAlg :: TcM HoleFitSortingAlg
getHoleFitDispConfig :: TcM HoleFitDispConfig

zonkSubs :: TidyEnv -> [HoleFit] -> TcM (TidyEnv, [HoleFit])
sortHoleFitsBySize :: [HoleFit] -> TcM [HoleFit]
sortHoleFitsByGraph :: [HoleFit] -> TcM [HoleFit]
