{-# LANGUAGE FlexibleContexts #-}
module T8011( clean )  where

import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import T8011a ( ToURL(URLT, toURL) )

clean :: (ToURL url, Show (URLT url)) => url -> String
clean = filter isAlphaNum . show . toURL
