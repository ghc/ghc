{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module T16384 where

import GHC.Exts

wat :: () -> Int#
wat _ = $$([|| 1# ||])

