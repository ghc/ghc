{-# LANGUAGE TypeFamilies #-}
module T10634 where

import Data.Int (Int8, Int16, Int32)

type family Up a
type instance Up Int8  = Int16
type instance Up Int16 = Int32

class (Up (Down a) ~ a) => Convert a where
   type Down a
   down :: a -> Down a

instance Convert Int16 where
   type Down Int16 = Int8
   down = fromIntegral

instance Convert Int32 where
   type Down Int32 = Int16
   down = fromIntegral

x :: Int8
x = down 8
