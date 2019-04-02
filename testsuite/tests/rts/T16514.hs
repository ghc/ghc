-- ensure that the XMM register values are properly preserved across STG
-- exit/entry. Note that this is very sensitive to code generation.

module Main where

import Control.Monad (when)
import System.Exit (exitWith, ExitCode(..))

foreign export ccall fn_hs :: IO ()

fn_hs :: IO ()
fn_hs = return ()

foreign import ccall test  :: IO Int

main :: IO ()
main = do res <- test
          when (res /= 0) (exitWith $ ExitFailure res)
