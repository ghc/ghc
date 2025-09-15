{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fprint-redundant-promotion-ticks #-}
module T24237 where

import Data.Proxy

foo :: Proxy '(:)
foo = ()
