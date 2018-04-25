
module DPH.War.Way
	(Way(..))
where
	
-- | A way to build the test
--	This holds extra options to pass to the program.
data Way
	= Way	{ wayName	:: String 
		, wayOptsComp	:: [String] 
		, wayOptsRun	:: [String] }
	deriving (Eq, Ord, Show)
