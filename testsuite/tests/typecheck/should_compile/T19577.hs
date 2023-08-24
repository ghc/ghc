{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module T19577 where

data SBool (b :: Bool) where
  STrue :: forall b. (b ~ 'True) => SBool b
  SFalse :: forall b. (b ~ 'False) => SBool b

class Blah b where
  blah :: SBool b

instance Blah 'True where
  blah = STrue

foo :: Blah b => (SBool b -> Int) -> Int
foo f = f blah

bar = foo (\(STrue @True) -> 42)
