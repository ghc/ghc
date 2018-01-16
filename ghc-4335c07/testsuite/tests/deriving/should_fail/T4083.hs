{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module T4083 where

data family F a
newtype instance F [a] = Maybe a

class C a where
  data D a

deriving instance C (Maybe a) => C (F [a])
