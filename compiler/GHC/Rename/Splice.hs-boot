module GHC.Rename.Splice where

import GHC.Hs
import GHC.Tc.Utils.Monad
import GHC.Types.Name (Name)
import GHC.Types.Name.Set


rnSpliceType :: HsUntypedSplice GhcPs -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsUntypedSplice GhcPs -> RnM ( (HsUntypedSplice GhcRn, HsUntypedSpliceResult (LPat GhcPs))
                                             , FreeVars)
rnSpliceTyPat  :: HsUntypedSplice GhcPs -> RnM ( (HsUntypedSplice GhcRn, HsUntypedSpliceResult (LHsType GhcPs))
                                             , FreeVars)
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsUntypedSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)

checkThLocalTyName :: Name -> RnM ()
