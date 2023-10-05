module GHC.Types.Unique.MemoFun (memoiseUniqueFun) where

import GHC.Prelude
import GHC.Types.Unique
import GHC.Types.Unique.FM

import Data.IORef
import System.IO.Unsafe

memoiseUniqueFun :: Uniquable k => (k -> a) -> k -> a
memoiseUniqueFun fun = unsafePerformIO $ do
  ref <- newIORef emptyUFM
  return $ \k -> unsafePerformIO $ do
    m <- readIORef ref
    case lookupUFM m k of
      Just a  -> return a
      Nothing -> do
        let !a  = fun k
            !m' = addToUFM m k a
        writeIORef ref m'
        return a
