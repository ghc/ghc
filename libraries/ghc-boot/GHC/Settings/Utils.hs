module GHC.Settings.Utils where

import Prelude -- See Note [Why do we import Prelude here?]

import Data.Char (isSpace)

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing

maybeReadFuzzy :: Read a => String -> Maybe a
maybeReadFuzzy str = case reads str of
  [(x, s)] | all isSpace s -> Just x
  _ -> Nothing
