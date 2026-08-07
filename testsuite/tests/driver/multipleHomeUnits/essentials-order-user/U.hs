{-# LANGUAGE NoImplicitPrelude #-}

-- A module in a unit whose GHC.Essentials comes from the home unit
-- 'essentials-home', whose GHC.Essentials is an empty stub.
--
-- The import of B forces the unit that uses base's GHC.Essentials to be
-- compiled first.  U must nevertheless resolve its known entities through its
-- own unit's stub, and so must fail to find 'fromInteger'.  If it succeeds,
-- GHC resolved known entities through whichever GHC.Essentials it happened to
-- see first, rather than the one this module depends on.
module U where

import B ( T )

x = 42
