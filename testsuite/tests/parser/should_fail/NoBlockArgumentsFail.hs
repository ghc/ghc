module NoBlockArgumentsFail where

import Control.Monad

foo :: IO ()
foo = when True do
  return ()
