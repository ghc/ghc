{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}

-- This is a copy of typecheck/should_run/T3500b, but it's here for
-- a different reason: at one time, it sent the compiler into a loop.
-- ANd T3500b isn't tested 'fast' mode

module T9565 where

newtype Mu f = Mu (f (Mu f))

type family Id m
type instance Id m = m

instance Show (Id (f (Mu f))) => Show (Mu f) where
    show (Mu f) = show f

showMu :: Mu (Either ()) -> String
showMu = show

item :: Mu (Either ())
item = Mu (Right (Mu (Left ())))

main = print (showMu item)
