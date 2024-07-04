module DepApi (A (A), dep) where

import Dep (A (A))
import qualified Dep

dep :: A
dep = Dep.dep
