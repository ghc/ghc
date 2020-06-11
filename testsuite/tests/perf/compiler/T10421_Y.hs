-- Y.hs
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
-- Imagine the values defined in this module are complicated
-- and there is no useful inlining/strictness/etc. information

module T10421_Y where

import T10421_Form

mreq :: a -> b -> c -> IO (FormResult d, ())
mreq = undefined

mopt :: a -> b -> c -> IO (FormResult d, ())
mopt = undefined

textField = undefined
checkBoxField = undefined
