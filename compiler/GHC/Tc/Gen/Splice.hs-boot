{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Tc.Gen.Splice where

import GHC.Prelude
import GHC.Types.Name
import GHC.Hs.Expr ( PendingRnSplice, DelayedSplice )
import GHC.Tc.Types( TcM , SpliceType )
import GHC.Tc.Utils.TcType   ( ExpRhoType )
import GHC.Types.Annotations ( Annotation, CoreAnnTarget )
import GHC.Hs.Extension      ( GhcTcId, GhcRn, GhcPs, GhcTc )

import GHC.Hs     ( HsSplice, HsBracket, HsExpr, LHsExpr, LHsType, LPat,
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

runTopSplice :: DelayedSplice -> TcM (HsExpr GhcTc)

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
