module GHC.Rename.Splice where

import GHC.Prelude
import GHC.Hs
import GHC.Tc.Utils.Monad
import GHC.Types.Name.Set


rnSpliceType :: HsSplice GhcPs   -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsSplice GhcPs   -> RnM ( Either (Pat GhcPs) (Pat GhcRn)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
