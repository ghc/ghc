{-# LANGUAGE TemplateHaskell #-}
module T3899 where

import T3899a

f = $(nestedTuple 3)
