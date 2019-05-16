module Lib where

data D1 = A1 | B1


-- Should print the unmatched constructors directly, i.e. A1 B1, B1 A1
f1 :: D1 -> D1 -> ()
f1 A1 A1 = ()
f1 B1 B1 = ()

data D2 = A2 | B2 | C2 | D2

-- Should *not* unfold all possible unmatched constructor pairs (exponential!),
-- instead print patterns of the form 'A2 p where p is not one of {A2}'
f2 :: D2 -> D2 -> ()
f2 A2 A2 = ()
f2 B2 B2 = ()
f2 C2 C2 = ()
f2 D2 D2 = ()

-- From here on, it's largely my taste deciding on which warnings to generate

data D3 = A3 | B3 | C3

{-# COMPLETE A3, B3 #-}

-- This is debatable, but IMO this should *not* output B3 as the only unmatched
-- pattern, because this would prematurely commit to the user-annotated
-- COMPLETE sig. Prints 'p where p is not one of {A3}' instead.
f3 :: D3 -> ()
f3 A3 = ()

-- This should print 'Just _' as the unmatched pattern, not 'Just ()'.
f4 :: Maybe () -> ()
f4 Nothing = ()
