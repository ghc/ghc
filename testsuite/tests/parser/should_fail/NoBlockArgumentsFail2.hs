module NoBlockArgumentsFail2 where

import Control.Monad

foo :: IO ()
foo = forM [1 .. 10] \x -> print x
