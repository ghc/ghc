-- !!! Test that List.sortBy is stable.

import List

main = print (sortBy (\(a,b) (a',b')->compare a a')
		([1,1,1,1,1,1,1,1,1,1]`zip`[1..10]))
