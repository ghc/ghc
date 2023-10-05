
{-# LANGUAGE NPlusKPatterns #-}

-- !!! N-plus-K pattern in binding

-- From: Andreas Marth
-- Sent: Monday, June 07, 1999 5:02 PM
-- To: glasgow-haskell-bugs@majordomo.haskell.org
-- Subject: compiler-bug

module ShouldCompile where

erroR :: Int
erroR = n where
                (n+1,_) = (5,2)

-- Produced a -dcore-lint error in the desugarer output
-- (Was a missing case in DsHsSyn.collectTypedPatBinders)
