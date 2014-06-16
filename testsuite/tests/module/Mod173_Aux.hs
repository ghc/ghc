module Mod173_Aux( module Mod173_Aux ) where

import qualified Data.List as Mod173_Aux( nub )
	-- This should not be exported

import Data.List as Mod173_Aux( partition ) 
	-- This one should be exported

frob x = Mod173_Aux.nub (x::[Int])	-- This one should
