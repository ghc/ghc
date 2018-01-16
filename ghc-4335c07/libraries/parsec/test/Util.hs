
module Util where

import Text.Parsec
import Text.Parsec.String ( Parser )

-- | Returns the error messages associated
-- with a failed parse.
parseErrors :: Parser a -> String -> [String]
parseErrors p input =
  case parse p "" input of
    Left err ->
      drop 1 $ lines $ show err
    Right{} -> []
