module GHC.Tc.Utils.Concrete where

import GHC.Utils.Misc      ( HasDebugCallStack )
import GHC.Tc.Types        ( TcM )
import GHC.Tc.Types.Origin ( FixedRuntimeRepContext )
import GHC.Tc.Utils.TcType ( TcType )

hasFixedRuntimeRep_syntactic :: HasDebugCallStack
                             => FixedRuntimeRepContext
                             -> TcType
                             -> TcM ()
