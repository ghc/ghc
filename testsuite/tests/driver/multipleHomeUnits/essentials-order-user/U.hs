{-# LANGUAGE NoImplicitPrelude #-}

-- A module in a unit whose GHC.Essentials comes from the home unit
-- 'essentials-home', whose GHC.Essentials is an empty stub.
module U where

import B ( T )

x = 42
