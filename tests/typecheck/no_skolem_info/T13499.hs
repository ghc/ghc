{-# LANGUAGE StaticPointers #-}

import Data.Typeable (Typeable)
import GHC.StaticPtr (StaticPtr)

f :: Typeable a => StaticPtr (a -> a)
f = static (\a -> _)

main :: IO ()
main = return ()
