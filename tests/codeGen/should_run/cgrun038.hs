{-
From: Rajiv Mirani <mirani>
Date: Sat, 26 Aug 95 21:14:47 -0400
Subject: GHC bug

GHC can't parse the following program when there is no newline at the 
end of the last line:
-}

module Main where
main :: IO ()
main = return ()
-- random comment