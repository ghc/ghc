{-# Language TypeApplications #-}
{-# Language TypeFamilies        #-}

module T15816 where
import Data.Kind

data family U
data instance U @Int
