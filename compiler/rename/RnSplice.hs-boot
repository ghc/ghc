module RnSplice where

import HsSyn
import TcRnMonad
import NameSet
import Kind


rnSpliceType :: HsSplice GhcPs   -> PostTc GhcRn Kind
             -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsSplice GhcPs   -> RnM ( Either (Pat GhcPs) (Pat GhcRn)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
