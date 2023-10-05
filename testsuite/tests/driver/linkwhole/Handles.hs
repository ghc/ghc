module Handles
  ( hsNewSOHandle
  ) where

import Foreign

import Types

import MyCode

foreign export ccall "hs_soHandles"
  hsNewSOHandle :: SOHandleExport

hsNewSOHandle :: SOHandleExport
hsNewSOHandle = newStablePtr SOHandles
  { someData = "I'm a shared object"
  , someFn = myFunction
  }
