module T17500 where

import Data.List ( isInfixOf )

isBCOsFile :: String -> IO Bool
isBCOsFile fname = do
    content <- readFile fname
    pure $ "== Proto-BCOs ==" `isInfixOf` content  -- Check title line
