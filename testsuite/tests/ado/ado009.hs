{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -ddump-rn -dsuppress-uniques #-}

module Test where

-- Make sure the $ stripped from the last stmt is printed
q :: IO ()
q = do
  a <- return ()
  return $ (\_ -> ()) a
