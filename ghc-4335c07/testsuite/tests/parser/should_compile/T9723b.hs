
{-# OPTIONS -fwarn-tabs #-}

-- Check we get a warning for multiple tabs, with the correct number of tabs
-- mentioned

module ShouldCompile where

-- tab in middle of line
tab1	= 'a'
-- tab at end of line
tab2 = 'b'	
-- two tabs in middle of line
tab3		= 'c'

tab4 = if True
-- tab at start of line
	then 'd'
-- tab at start of line
	else 'e'

	-- tab before a comment starts
