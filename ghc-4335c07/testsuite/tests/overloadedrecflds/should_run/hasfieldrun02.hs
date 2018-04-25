{-# LANGUAGE DuplicateRecordFields, OverloadedLabels,
             ExistentialQuantification,
             FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeApplications #-}

import GHC.OverloadedLabels
import GHC.Records

data S = MkS { foo :: Int }
data T x y z = forall b . MkT { foo :: y, bar :: b }

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

main = do print (#foo (MkS 42))
          print (#foo (MkT True False))
