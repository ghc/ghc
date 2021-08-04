{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Version.Compat (
  module Base
, makeVersion
) where
import Data.Version as Base

#if !(MIN_VERSION_base(4,8,0))
import Prelude.Compat

-- | Construct tag-less 'Version'
--
-- /Since: 4.8.0.0/
makeVersion :: [Int] -> Version
makeVersion b = Version b []
#endif
