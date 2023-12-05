{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module T23862 where

data Checked
data Unchecked

data family Result check a
data instance Result Checked a = CheckedResult a
newtype instance Result Unchecked a = UncheckedResult a

data CheckSingleton check where
   Checked :: CheckSingleton Checked
   Unchecked :: CheckSingleton Unchecked

und :: Bool -> Bool
und x = und x

app :: forall check. (() -> CheckSingleton check) -> Result check Bool
app m = let f :: Result check (Bool -> Bool)
            f = case m () of
                   Checked -> CheckedResult und
                   Unchecked -> UncheckedResult und
        in case m () of
             Checked -> case f of
                          CheckedResult x -> CheckedResult (x True)
             Unchecked -> UncheckedResult True
