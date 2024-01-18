import Data.Char
import Data.Foldable
-- | Just like 'GHC.ResponseFile.escapeArg', but it is not exposed from base.
escapeArg :: String -> String
escapeArg = reverse . foldl' escape []

escape :: String -> Char -> String
escape cs c
  |    isSpace c
    || '\\' == c
    || '\'' == c
    || '"'  == c = c:'\\':cs -- n.b., our caller must reverse the result
  | otherwise    = c:cs

