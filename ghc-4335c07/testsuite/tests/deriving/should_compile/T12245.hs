{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module T12245 where

import Data.Data ( Data )

data Foo f = Foo (f Bool) (f Int)

deriving instance Data (Foo [])
deriving instance Data (Foo Maybe)
