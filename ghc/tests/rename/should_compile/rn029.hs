-- !!! Checking that lazy name clashing works.
module ShouldSucceed where

import List ( reverse, sort )

sort :: Int	-- Clashes with List.sort, 
sort = 4	-- but never used, so OK	
	

reverse :: Int	-- Clashes with List.reverse, 
reverse = 3	-- but the only uses are qualified

x = ShouldSucceed.reverse

y = List.reverse


