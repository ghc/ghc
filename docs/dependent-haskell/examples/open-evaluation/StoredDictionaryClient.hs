{-# LANGUAGE BangPatterns, Haskell2010, NoExperimentalDependentTypes #-}
module StoredDictionaryClient (storedDecisions, bindingDemand) where

import Control.Exception (ErrorCall, evaluate, try)
import RetainedStoredDictionary (Result(..), ignoreStored, inspectStored)
import StoredDictionary (Cell(Stored), Pick(..), pack, readStored)

instance Pick Bool where
    pick = not

-- | Compare ordinary and retained matches on two cells with equal payloads.
storedDecisions :: ((Bool, Bool), (Bool, Bool))
storedDecisions =
    let first = pack True
        second = Stored True
        readRetained cell = case inspectStored cell of Result b -> b
    in ((readStored first, readStored second), (readRetained first, readRetained second))

-- | Force the cell with a bang and leave it unevaluated with a lazy binding.
bindingDemand :: IO (Bool, Bool)
bindingDemand = do
    forced <- try (evaluate strict)
    let caught = case (forced :: Either ErrorCall Bool) of
            Left _ -> True
            Right _ -> False
    pure (caught, lazy)
  where
    strict =
        let !cell = error "strict cell" :: Cell [Bool]
        in case ignoreStored cell of Result b -> b
    lazy =
        let cell = error "unused lazy cell" :: Cell [Bool]
        in case ignoreStored cell of Result b -> b
