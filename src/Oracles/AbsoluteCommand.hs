{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.AbsoluteCommand (
    lookupInPath, absoluteCommandOracle
    ) where

import Base

newtype AbsoluteCommand = AbsoluteCommand String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

absoluteCommand :: String -> Action String
absoluteCommand = askOracle . AbsoluteCommand

-- | Lookup a @command@ in @PATH@ environment.
lookupInPath :: FilePath -> Action FilePath
lookupInPath c
    | c /= takeFileName c = return c
    | otherwise           = absoluteCommand c

-- | Split function. Splits a string @s@ into chunks
-- when the predicate @p@ holds. See: http://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
    case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'


absoluteCommandOracle :: Rules ()
absoluteCommandOracle = do
    o <- newCache $ \c -> do
        envPaths <- wordsWhen (== ':') <$> getEnvWithDefault "" "PATH"
        let candidates = map (-/- c) envPaths
        -- this will crash if we do not find any valid candidate.
        fullCommand <- head <$> filterM doesFileExist candidates
        putOracle $ "Found '" ++ c ++ "' at " ++ "'" ++ fullCommand ++ "'"
        return fullCommand
    _ <- addOracle $ \(AbsoluteCommand c) -> o c
    return ()
