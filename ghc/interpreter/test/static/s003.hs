--!!! Testing error checking in qualified names (local variables)

-- No qualified local variables
module TestQual3 where
f x = A.y where A.y = x
