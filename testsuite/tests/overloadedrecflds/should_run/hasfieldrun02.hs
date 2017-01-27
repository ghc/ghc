{-# LANGUAGE DuplicateRecordFields, OverloadedLabels,
             ExistentialQuantification,
             FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeApplications #-}

data S = MkS { foo :: Int }
data T x y z = forall b . MkT { foo :: y, bar :: b }

main = do print (#foo (MkS 42))
          print (#foo (MkT True False))
