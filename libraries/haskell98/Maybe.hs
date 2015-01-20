{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Maybe (
        isJust, isNothing,
        fromJust, fromMaybe, listToMaybe, maybeToList,
        catMaybes, mapMaybe,

        -- ...and what the Prelude exports
        Maybe(Nothing, Just),
        maybe
    ) where

import Data.Maybe
