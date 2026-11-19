module ReadNatural (readNatural) where

import Numeric.Natural (Natural)
import Text.Read (readEither)

-- | Parse a natural number with the ordinary Read instance.
readNatural :: String -> Either String Natural
readNatural = readEither
