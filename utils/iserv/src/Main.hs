-- |
-- The Remote GHCi server.
--
-- For details on Remote GHCi, see Note [Remote GHCi] in
-- compiler/GHC/Runtime/Interpreter.hs.
--
module Main (main) where

import GHCi.Server (defaultServer)

main :: IO ()
main = defaultServer
