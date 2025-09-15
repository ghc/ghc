{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -W #-}
-- Produced a bogus unused-import warning in versions of GHC 6.6 

module Bug ( Structure (..) ) where

import Prelude hiding( Rational )
   -- Rational is exported by Prelude
import Data.Ratio ( Rational )

data Structure a where
   StructCons  :: Int   -> Structure Int
   StructRatio ::          Structure Rational


