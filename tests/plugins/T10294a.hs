module T10294a where

import SayAnnNames
import Data.Data

baz :: Constr
baz = toConstr SomeAnn
