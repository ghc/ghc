{-# LANGUAGE ImplicitParams, TypeInType, ExplicitForAll #-}

module T11405 where

import GHC.Exts
import GHC.Stack

x :: forall (v :: Levity) (a :: TYPE v). (?callStack :: CallStack) => a
x = undefined
