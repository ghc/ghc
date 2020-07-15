module GHC.Tc.Utils.Unify where

import GHC.Prelude
import GHC.Tc.Utils.TcType   ( TcTauType )
import GHC.Tc.Types          ( TcM )
import GHC.Tc.Types.Evidence ( TcCoercion, HsWrapper )
import GHC.Tc.Types.Origin ( CtOrigin )
import GHC.Utils.Outputable( SDoc )
import GHC.Hs.Type     ( Mult )


-- This boot file exists only to tie the knot between
--              GHC.Tc.Utils.Unify and Inst

unifyType :: Maybe SDoc -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Maybe SDoc -> TcTauType -> TcTauType -> TcM TcCoercion

tcSubMult :: CtOrigin -> Mult -> Mult -> TcM HsWrapper
