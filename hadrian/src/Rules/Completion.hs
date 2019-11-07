module Rules.Completion where

import Base
import CommandLine
import Settings

-- | A phony @autocomplete@ rule that prints all valid setting keys
--   completions of the value specified in the @--complete-setting=...@ flag,
--   or simply all valid setting keys if no such argument is passed to Hadrian.
--
--   It is based on the 'completeSetting' function, from the "Settings" module.
completionRule :: Rules ()
completionRule =
  "autocomplete" ~> do
    partialStr <- fromMaybe "" <$> cmdCompleteSetting
    case completeSetting (splitOn "." partialStr) of
      [] -> fail $ "No valid completion found for " ++ partialStr
      cs -> forM_ cs $ \ks ->
        liftIO . putStrLn $ intercalate "." ks
