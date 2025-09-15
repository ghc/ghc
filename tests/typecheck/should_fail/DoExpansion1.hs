module DoExpansion1 where


-- Ensure that >> expansions work okay

qqqqq1 :: IO ()
qqqqq1 = putStrLn 1  >> putStrLn "q2" >>  putStrLn "q3" -- this should error as "In the first argument to >>"


qqqqq2 :: IO ()
qqqqq2 = (putStrLn "q1" >> putStrLn 2) >> putStrLn "q3" -- this should error as "In first argument to >>
                                                        --                       In second argument to >>"

qqqqq3 :: IO ()
qqqqq3 = putStrLn "q1" >> (putStrLn "q2" >> putStrLn 3) -- this should error as "In second argument to >>
                                                        --                       In second argument to >>"

rrrr1 :: IO ()
rrrr1 = do putStrLn 1                -- this should error as "In the stmt of a do block"
           putStrLn "r2"
           putStrLn "r3"

rrrr2 :: IO ()
rrrr2 = do putStrLn "r1"
           putStrLn 2                -- this should error as "In the stmt of a do block"
           putStrLn "r3"


rrrr3 :: IO ()
rrrr3 = do putStrLn "r1"
           putStrLn "r2"
           putStrLn 3  -- this should error as "In the stmt of a do block"
