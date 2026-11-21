{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes #-}
module StoredDictionaryClient (storedDecisions) where

import RetainedStoredDictionary (Result(..), inspectStored)
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
