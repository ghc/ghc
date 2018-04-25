{-# LANGUAGE ApplicativeDo #-}
module Test where

-- This exposed a bug in zonking ApplicativeLastStmt
test :: IO Int
test
  = do
      x <- return ()
      h <- return (\_ -> 3)
      return (h ())
