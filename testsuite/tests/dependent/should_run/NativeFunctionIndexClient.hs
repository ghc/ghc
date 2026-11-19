{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import qualified FunctionFieldClient as Client
import qualified FunctionFieldLibrary as Library

main :: IO ()
main = do
    let runtime value = newIORef value >>= readIORef
    yes <- runtime True
    no <- runtime False
    unread <- runtime (error "index argument")
    keep <- runtime id
    invert <- runtime not
    throw <- runtime (error "index function")
    print (Client.inspect (Library.build invert yes),
           Client.inspectViaCall (Library.repack (Library.build keep yes)),
           Client.proofOnly (Library.build invert no))
    print (Client.outer (Library.loose (error "container") unread),
           Client.which (Library.loose (Just throw) unread))
    print (Client.proofOnly (Library.build (const True) unread))
    demands "index function" (Client.proofOnly (Library.build throw yes))
    demands "index argument" (Client.proofOnly (Library.build invert unread))

demands :: String -> Bool -> IO ()
demands marker value = do
    result <- try (evaluate value)
    case result of
        Left e | marker `isInfixOf` displayException (e :: ErrorCall) ->
            putStrLn (marker ++ ": forced")
        _ -> fail ("Unexpected demand of " ++ marker)
