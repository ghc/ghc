module Dep01 where

import Control.Monad
import Control.Monad.ST
import Data.STRef

sumST :: Num a => [a] -> IO a
sumST xs = unsafeSTToIO $ do
    n <- newSTRef 0
    forM_ xs $ \x -> do
        modifySTRef n (+x)
    readSTRef n

