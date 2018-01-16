module T11232 where

import Control.Monad
import Data.Data

mkMp :: ( MonadPlus m
        , Typeable a
        , Typeable b
        )
     => (b -> m b)
     -> a
     -> m a
mkMp ext = unM (maybe (M (const mzero)) id (gcast (M ext)))

newtype M m x = M { unM :: x -> m x }
