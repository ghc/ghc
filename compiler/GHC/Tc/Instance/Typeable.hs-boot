module GHC.Tc.Instance.Typeable where

import GHC.Prelude
import GHC.Core.TyCon ( TyCon )
import GHC.Tc.Types ( TcM )

-- See Note [Forall matchability in Typeable kind prefixes] in GHC.Tc.Instance.Typeable.
tyConIsTypeable :: TyCon -> TcM Bool
