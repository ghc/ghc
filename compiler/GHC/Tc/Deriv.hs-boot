{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}


{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Handles @deriving@ clauses on @data@ declarations.
module GHC.Tc.Deriv ( tcDeriving, DerivInfo(..) ) where
import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Core.Type
import GHC.Core.TyCon

import GHC.Types.Name

import GHC.Data.Bag

data DerivInfo = DerivInfo { di_rep_tc  :: TyCon
                           , di_scoped_tvs :: ![(Name,TyVar)]
                           , di_clauses :: [LHsDerivingClause GhcRn]
                           , di_ctxt    :: HsCtxt
                           }


tcDeriving  :: [DerivInfo]
            -> [LDerivDecl GhcRn]
            -> TcM (TcGblEnv, Bag (InstInfo GhcRn), HsValBinds GhcRn)
