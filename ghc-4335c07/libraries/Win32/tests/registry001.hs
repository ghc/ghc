import System.Win32
import Control.Exception
import Control.Monad

x = "bumble"
name = "test_registry001"

-- Create, read, and delete a value (test for bug #3241)
main = do
  k1 <- regCreateKey hKEY_CURRENT_USER "Software"
  k2 <- regCreateKey k1 "Haskell"
  k3 <- regCreateKey k2 "GHC"
  flip finally (regDeleteValue k3 name) $ do
  regSetStringValue k3 name x
  r <- regQueryValue k3 name
  print r
