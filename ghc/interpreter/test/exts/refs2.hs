--!!! Testing Mutvars

import ST

-- Note: equivalent code of the form: show (runST (newSTRef 'a' ...))
-- won't typecheck under Hugs 1.01.

a1 = show (runST prog)
 where
  prog :: ST s Char
  prog =
	newSTRef 'a'	>>= \ v ->
	readSTRef v

a2 = show (runST prog)
 where
  prog :: ST s Char
  prog =
	newSTRef 'a'		>>= \ v ->
	writeSTRef v 'b'	>>
	readSTRef v

a3 = show (runST prog)
 where
  prog :: ST s (Bool,Bool,Bool)
  prog =
	newSTRef 'a'		>>= \ v1 ->
	newSTRef 'a'		>>= \ v2 ->
	return (v1 == v1, v1 == v2, v2 == v2)

