module TcUnify where

import GhcPrelude
import TcType           ( TcTauType )
import TcRnTypes        ( TcM )
import TcEvidence       ( TcCoercion )
import GHC.Hs.Expr      ( HsExpr )
import GHC.Hs.Types     ( HsType )
import GHC.Hs.Extension ( GhcRn )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: Maybe (HsExpr GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Maybe (HsType GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
