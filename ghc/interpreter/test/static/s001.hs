--!!! Testing error checking in qualified names (patterns)

-- No qualified variables in patterns
module TestQual1 where
f (A.x : xs) = xs

