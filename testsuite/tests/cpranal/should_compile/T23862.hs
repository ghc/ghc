{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module T23862 where

data family Result (check :: Bool) a
data instance Result True a = CheckedResult a
newtype instance Result False a = UncheckedResult a

data CheckSingleton (check :: Bool) where
   Checked :: CheckSingleton True
   Unchecked :: CheckSingleton False

app :: (() -> CheckSingleton check) -> Result check Bool
app m = case (m (), m ()) of
 (Checked, Unchecked)
   | CheckedResult x <- UncheckedResult (\_ -> True)
   -> CheckedResult (x True)
