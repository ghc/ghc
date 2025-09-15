-- Test that we can catch JavaScript exceptions in Haskell and vice
-- versa.

module Test where

import Control.Exception
import GHC.Wasm.Prim

foreign import javascript safe "Promise.reject('game over')"
  js_game_over :: IO ()

foreign export javascript "testJSException"
  testJSException :: IO ()

testJSException :: IO ()
testJSException = catch (evaluate =<< js_game_over) $ \(e :: JSException) -> print e

foreign export javascript "testHSException"
  testHSException :: IO ()

testHSException :: IO ()
testHSException = fail "game over"
