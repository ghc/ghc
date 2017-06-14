{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module TcSplice where
import Name
import HsExpr   ( PendingRnSplice )
import TcRnTypes( TcM , SpliceType )
import TcType   ( ExpRhoType )
import Annotations ( Annotation, CoreAnnTarget )
import HsExtension ( GhcTcId, GhcRn, GhcPs )

import HsSyn      ( HsSplice, HsBracket, HsExpr, LHsExpr, LHsType, LPat,
                    LHsDecl, ThModFinalizers )
import qualified Language.Haskell.TH as TH

tcSpliceExpr :: HsSplice GhcRn
             -> ExpRhoType
             -> TcM (HsExpr GhcTcId)

tcUntypedBracket :: HsExpr GhcRn
                 -> HsBracket GhcRn
                 -> [PendingRnSplice]
                 -> ExpRhoType
                 -> TcM (HsExpr GhcTcId)
tcTypedBracket :: HsExpr GhcRn
               -> HsBracket GhcRn
               -> ExpRhoType
               -> TcM (HsExpr GhcTcId)

runAnnotation     :: CoreAnnTarget -> LHsExpr GhcRn -> TcM Annotation

tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr GhcTcId) -> TcM (LHsExpr GhcTcId)

runMetaE :: LHsExpr GhcTcId -> TcM (LHsExpr GhcPs)
runMetaP :: LHsExpr GhcTcId -> TcM (LPat GhcPs)
runMetaT :: LHsExpr GhcTcId -> TcM (LHsType GhcPs)
runMetaD :: LHsExpr GhcTcId -> TcM [LHsDecl GhcPs]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a
runRemoteModFinalizers :: ThModFinalizers -> TcM ()
finishTH :: TcM ()
