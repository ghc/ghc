{-# LANGUAGE DeriveFoldable #-}
module CommentsTest (foo) where
{-
An opening comment
  {- with a nested one -}
  {-# nested PRAGMA #-}
-}

import qualified Data.List as DL

-- | The function @foo@ does blah
foo = let
        a = 1
        b = 2 -- value 2
      in a + b
