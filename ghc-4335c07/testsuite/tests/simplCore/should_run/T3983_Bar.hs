{-# LANGUAGE DeriveDataTypeable #-}
module T3983_Bar where
import Data.Dynamic
import Control.Exception
import Control.Monad (unless)

type Assertion = IO ()

data X = X String deriving (Show, Typeable)

instance Exception X

throwX = throw.X

catchX action = do { action; return True; } `catches` [Handler (\(X _) -> return False)]