-- This boot file is in place to break the loop where:
-- + GHC.Tc.Types needs 'HoleFitPlugin',
-- + which needs 'GHC.Tc.Errors.Hole.FitTypes'
-- + which needs 'GHC.Tc.Types'
module GHC.Tc.Errors.Hole.FitTypes where

import GHC.Base (Int, Maybe)
import GHC.Types.Var (Id)
import GHC.Types.Name (Name)
import GHC.Types.Name.Reader (GlobalRdrElt)
import GHC.Tc.Utils.TcType (TcType)
import GHC.Hs.Doc (HsDocString)
import GHC.Utils.Outputable (SDoc)

data HoleFitCandidate
  = IdHFCand Id
  | NameHFCand Name
  | GreHFCand GlobalRdrElt

data HoleFitPlugin
data HoleFit =
  HoleFit { hfId   :: Id
          , hfCand :: HoleFitCandidate
          , hfType :: TcType
          , hfRefLvl :: Int
          , hfWrap :: [TcType]
          , hfMatches :: [TcType]
          , hfDoc :: Maybe [HsDocString]
          }
 | RawHoleFit SDoc
