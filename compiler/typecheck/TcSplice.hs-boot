{-# LANGUAGE CPP #-}

module TcSplice where
import HsSyn    ( HsSplice, HsBracket, HsExpr, LHsExpr )
import HsExpr   ( PendingRnSplice )
import Name     ( Name )
import TcRnTypes( TcM, TcId )
import TcType   ( ExpRhoType )
import Annotations ( Annotation, CoreAnnTarget )

import HsSyn      ( LHsType, LPat, LHsDecl, ThModFinalizers )
import RdrName    ( RdrName )
import TcRnTypes  ( SpliceType )
import qualified Language.Haskell.TH as TH

tcSpliceExpr :: HsSplice Name
             -> ExpRhoType
             -> TcM (HsExpr TcId)

tcUntypedBracket :: HsBracket Name
                 -> [PendingRnSplice]
                 -> ExpRhoType
                 -> TcM (HsExpr TcId)
tcTypedBracket :: HsBracket Name
               -> ExpRhoType
               -> TcM (HsExpr TcId)

runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr TcId) -> TcM (LHsExpr TcId)

runMetaE :: LHsExpr TcId -> TcM (LHsExpr RdrName)
runMetaP :: LHsExpr TcId -> TcM (LPat RdrName)
runMetaT :: LHsExpr TcId  -> TcM (LHsType RdrName)
runMetaD :: LHsExpr TcId -> TcM [LHsDecl RdrName]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a
runRemoteModFinalizers :: ThModFinalizers -> TcM ()
finishTH :: TcM ()
