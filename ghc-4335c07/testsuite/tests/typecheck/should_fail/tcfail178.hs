-- See Trac #1221

module ShouldFail where

a :: Num a => (Bool -> [a]) -> [a]
a x = x True ++ [1]

y :: b -> ()
y = const ()

-- Typechecks ok
b = a (const [2])

-- This one had an uninformative error message
c = a y

-- More informative
d = a ()

