module FastcallCrossCore where

import FastcallCrossAux
import GHC.Wasm.Prim

fast_exported_there :: Int -> Int
fast_exported_there = fastcall js_exported

use_fast_exported_here :: Int -> Int
use_fast_exported_here = fast_exported_here

use_fast_private_here :: Int -> Int
use_fast_private_here = fast_private_here
