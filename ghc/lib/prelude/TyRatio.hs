module PreludeRatio where

-- *** context missing ***
--  (NB: compiler's builtin idea of "data Ratio ..." must also
--	omit the context!)
data Ratio a = a :% a
	  deriving () -- NB ***

-- ToDo: Ratio Int# ???
-- {-# SPECIALIZE data a{Int#} :: Ratio a #-}
