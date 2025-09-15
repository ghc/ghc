module T2182 where
instance Read (IO a) where
 readsPrec = undefined
x = read "" :: IO Bool
y = show (\x -> x)
z = (\x -> x) == (\y -> y)
