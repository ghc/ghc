module T22715_2 where

import T22715_2a

debugTerminalKeys :: (forall m. CommandMonad m => m Char) -> Input IO Char
debugTerminalKeys eval = runIdT eval
