{-# LANGUAGE NoDeepSubsumption, NoExperimentalDependentTypes, RankNTypes, TypeApplications #-}
module LexicalOrdinaryShallow (shallow) where

{-# NOINLINE reader #-}
reader :: forall a. Show a => a -> String
reader = show

shallow :: (String, String)
shallow =
  let apply :: (forall a. Show a => a -> String) -> (String, String)
      apply f = (f @Bool True, f @Char 'q')
  in apply reader
