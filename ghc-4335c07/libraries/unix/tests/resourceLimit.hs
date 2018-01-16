
-- #2038

import System.Posix.Resource

main :: IO ()
main = do
    let soft = ResourceLimit 5
        hard = ResourceLimit 10
    setResourceLimit ResourceCPUTime (ResourceLimits soft hard)
    r <- getResourceLimit ResourceCPUTime
    let (ResourceLimit s) = softLimit r
    let (ResourceLimit h) = hardLimit r
    putStrLn $ show s
    putStrLn $ show h

