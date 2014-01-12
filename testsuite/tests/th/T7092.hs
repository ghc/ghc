{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

-- Should not produce a name-shadowing warning (GHC 7.4 did)

module T7092 where

import T7092a

blah = $(code)
