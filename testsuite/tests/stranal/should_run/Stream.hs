-- Adapted from symalg/RealM.hs's treeFrom

data Stream a = Stream a (Stream a)

-- This must not get a CPR signature that allows for nested cpr,
-- as it would make the worker call itself before producing the
-- Stream constructor.

forever :: a -> Stream a
forever x = Stream x (forever x)

main :: IO ()
main = forever () `seq` return ()
