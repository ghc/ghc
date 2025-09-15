module T25790
    ( nest
    ) where

import Control.Monad.Reader

data RunS = RunS { depth :: Int }

nest :: ReaderT RunS IO a -> ReaderT RunS IO a
nest = local (\s -> s { depth = depth s })
