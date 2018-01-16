{-# LANGUAGE CPP #-}

-- | Compatibility layer for "Control.Monad.Fail"
module Distribution.Compat.MonadFail ( MonadFail(fail) ) where
#if __GLASGOW_HASKELL__ >= 800
-- provided by base-4.9.0.0 and later
import Control.Monad.Fail (MonadFail(fail))
#else
-- the following code corresponds to
-- http://hackage.haskell.org/package/fail-4.9.0.0
import qualified Prelude as P
import Distribution.Compat.Prelude hiding (fail)

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

class Monad m => MonadFail m where
    fail :: String -> m a

-- instances provided by base-4.9

instance MonadFail Maybe where
    fail _ = Nothing

instance MonadFail [] where
    fail _ = []

instance MonadFail P.IO where
    fail = P.fail

instance MonadFail ReadPrec where
    fail = P.fail -- = P (\_ -> fail s)

instance MonadFail ReadP where
    fail = P.fail
#endif
