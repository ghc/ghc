{-# LANGUAGE ApplicativeComprehensions #-}
module Test where

-- This exposed a bug in zonking ApplicativeLastStmt
test :: IO Int
test = [h () | x <- pure (), h <- pure (\_ -> 3)]
