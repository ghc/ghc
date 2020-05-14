module GHC.Tc.Utils.Unify where

import GHC.Prelude
import GHC.Tc.Utils.TcType   ( TcTauType )
import GHC.Tc.Types          ( TcM )
import GHC.Tc.Types.Evidence ( TcCoercion )
import GHC.Hs.Expr      ( HsExpr )
import GHC.Hs.Type     ( HsType )
import GHC.Hs.Extension ( GhcRn )

-- This boot file exists only to tie the knot between
--              GHC.Tc.Utils.Unify and Inst

unifyType :: Maybe (HsExpr GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Maybe (HsType GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
