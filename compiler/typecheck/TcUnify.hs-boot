module TcUnify where
import TcType      ( TcTauType )
import TcRnTypes   ( TcM )
import TcEvidence  ( TcCoercion )
import HsExpr      ( HsExpr )
import HsTypes     ( HsType )
import HsExtension ( GhcRn )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: Maybe (HsExpr GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Maybe (HsType GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
