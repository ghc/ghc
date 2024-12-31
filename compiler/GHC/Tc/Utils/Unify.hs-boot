module GHC.Tc.Utils.Unify where

import GHC.Prelude
import GHC.Core.Type           ( Mult )
import GHC.Tc.Utils.TcType     ( TcTauType )
import GHC.Tc.Types            ( TcM )
import GHC.Tc.Types.Constraint ( Cts )
import GHC.Tc.Types.Evidence   ( TcCoercion )
import GHC.Tc.Types.Origin     ( CtOrigin, TypedThing )
import GHC.Tc.Utils.TcType     ( TcType, ConcreteTvOrigin )

import GHC.Data.FastString ( FastString )


-- This boot file exists only to tie the knot between
--   GHC.Tc.Utils.Unify and GHC.Tc.Utils.Instantiate/GHC.Tc.Utils.TcMType

unifyType          :: Maybe TypedThing -> TcTauType -> TcTauType -> TcM TcCoercion
unifyInvisibleType :: TcTauType -> TcTauType -> TcM TcCoercion

tcSubMult :: CtOrigin -> Mult -> Mult -> TcM ()

makeTypeConcrete :: FastString -> ConcreteTvOrigin
                 -> TcType -> TcM (TcCoercion, Cts)
