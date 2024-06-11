{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.TyCl.Instance where

import GHC.Prelude
import GHC.Hs
import GHC.Types.Var.Env
import GHC.Types.Name
import GHC.Core.FamInstEnv
import GHC.Tc.Types
import GHC.Tc.Utils.Env( InstInfo )
import GHC.Tc.Deriv.Utils
import GHC.Tc.Instance.Class( AssocInstInfo )

-- We need this because of the mutual recursion
-- between GHC.Tc.TyCl and GHC.Tc.TyCl.Instance
tcInstDecls1 :: [LInstDecl GhcRn]
             -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo], ThBindEnv)

tcDataFamInstDecl :: AssocInstInfo -> TyVarEnv Name -> LDataFamInstDecl GhcRn -> TcM (FamInst, Maybe DerivInfo)
tcTyFamInstDecl :: AssocInstInfo -> LTyFamInstDecl GhcRn -> TcM FamInst
