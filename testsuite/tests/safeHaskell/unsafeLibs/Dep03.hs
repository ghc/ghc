module Dep03 where

import Foreign

bad :: IO a -> a
bad = unsafePerformIO

