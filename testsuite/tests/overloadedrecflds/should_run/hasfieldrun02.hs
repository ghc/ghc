{-# LANGUAGE DuplicateRecordFields, OverloadedLabels,
             ExistentialQuantification,
             FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeApplications #-}

import GHC.OverloadedLabels (IsLabel(..))
import GHC.Records (HasField(..))

data S = MkS { foo :: Int }
data T x y z = forall b . MkT { foo :: y, bar :: b }

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel _ = getField @x

main = do print (#foo (MkS 42))
          print (#foo (MkT True False))
