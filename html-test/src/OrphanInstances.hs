module OrphanInstances where

import OrphanInstancesType
import OrphanInstancesClass

instance AClass AnotherType where
  aClass (AType n) = n
