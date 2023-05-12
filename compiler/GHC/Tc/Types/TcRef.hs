module GHC.Tc.Types.TcRef where
import Data.IORef


-- | Type alias for 'IORef'; the convention is we'll use this for mutable
-- bits of data in 'TcGblEnv' which are updated during typechecking and
-- returned at the end.
type TcRef a     = IORef a