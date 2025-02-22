{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.TyCl.Instance ( tryTcInstDecls1, tcInstDecls1 ) where

import GHC.Hs
import GHC.Tc.Types
import GHC.Tc.Utils.Env( InstInfo )
import GHC.Tc.Deriv

-- We need this because of the mutual recursion
-- between GHC.Tc.TyCl and GHC.Tc.TyCl.Instance
tryTcInstDecls1 :: [LInstDecl GhcRn]
             -> TcM ([LInstDecl GhcRn], TcGblEnv, [InstInfo GhcRn], [DerivInfo], ThBindEnv)

tcInstDecls1 :: [LInstDecl GhcRn]
             -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo], ThBindEnv)