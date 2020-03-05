{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -ddump-rn -dsuppress-uniques #-}

module Test where

-- Sanity check for a simple expression not involving join.
q1 :: IO ()
q1 = do
    a <- pure ()
    b <- pure ()
    pure $ pureNothing a
  where
    pureNothing :: a -> ()
    pureNothing _ = ()

-- Sanity check for a simple expression involving join.
q2 :: IO ()
q2 = do
    a <- pure ()
    b <- pure ()
    doNothing a
  where
    doNothing :: a -> IO ()
    doNothing _ = pure ()
