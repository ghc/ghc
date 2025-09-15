module T15226b where

import Control.Exception

data Str a = Str !a

bar :: Maybe a -> IO (Str (Maybe a))
bar x = do
  x' <- evaluate x
  pure (Str x')
