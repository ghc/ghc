module CrossDepApi (A (A), dep) where

import CrossDep (A (A))
import qualified CrossDep

dep :: A
dep = CrossDep.dep
