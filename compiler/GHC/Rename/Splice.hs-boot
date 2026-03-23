module GHC.Rename.Splice where

import GHC.Hs
import GHC.Tc.Utils.Monad
import GHC.Types.Name (Name)
import GHC.Types.Name.Set


rnSpliceType :: HsUntypedSplice GhcPs -> RnM (HsType GhcRn, FreeNames)
rnSplicePat  :: HsUntypedSplice GhcPs -> RnM ( (HsUntypedSplice GhcRn, HsUntypedSpliceResult (LPat GhcPs))
                                             , FreeNames)
rnSpliceTyPat  :: HsUntypedSplice GhcPs -> RnM ( (HsUntypedSplice GhcRn, HsUntypedSpliceResult (LHsType GhcPs))
                                             , FreeNames)
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeNames)

rnTopSpliceDecls :: HsUntypedSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeNames)

checkThLocalTyName :: Name -> RnM ()

checkThLocalNameNoLift :: LIdOccP GhcRn -> RnM ()
