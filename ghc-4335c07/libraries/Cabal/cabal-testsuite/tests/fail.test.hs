{-# LANGUAGE ScopedTypeVariables #-}
import Test.Cabal.Prelude
import Data.IORef
import Control.Monad.IO.Class
import Control.Exception (ErrorCall)

import qualified Control.Monad.Catch as Catch

main = setupTest $ do
  -- the following is a hack to check that `setup configure` indeed
  -- fails: all tests use `assertFailure` which uses `error` if the fail
  --
  -- note: we cannot use `fails $ do ...` here since that only checks that all
  -- assertions fail. If there is no assertion in `m`, then `fails m` will *succeed*.
  -- That's not what we want. So `fails (return ())` for example succeeds, even though
  -- `return ()` doesn't fail.
  succeededRef <- liftIO $ newIORef True
  setup "configure" [] `Catch.catch` \(_ :: ErrorCall) ->
    liftIO $ writeIORef succeededRef False
  succeeded <- liftIO $ readIORef succeededRef
  assertBool "test should have failed, but succeeded instead (configure exits with failure)" $ not succeeded
