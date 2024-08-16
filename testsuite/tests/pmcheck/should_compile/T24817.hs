{-# LANGUAGE GADTs, DataKinds #-}

module T24817 where

data SBool b where
  STrue :: SBool True
  SFalse :: SBool False

foo :: forall a b. (SBool a, SBool b)
foo = error "urk"

bar :: Bool
bar = case foo of
        (STrue, SFalse) -> True
        _ -> False
