module T14363a where

import Data.Coerce

contra :: (a -> b) -> (f b -> f a)
contra = undefined

foo x = [coerce, contra]
