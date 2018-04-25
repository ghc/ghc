module Main where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Applicative
import Prelude -- for AMP compat

data A = A String deriving (Eq, Show)

data E = E {
  a :: TVar [Int],
  b :: TVar A,
  c :: TVar [Int]
  }

consistency_1 :: E -> STM Bool
consistency_1 = \e -> do
  _ <- readTVar $ c e
  return True

installSanityChecks :: E -> IO ()
installSanityChecks e = do
  x e
  fail "You should see this failure"

x :: E -> IO ()
x e = do
  -- This unexpected succeeds
  atomically $ installCheck consistency_1
  -- error "derp2"
  where
    installCheck check = always $ check e

main :: IO ()
main = do
  state <- initialize
  installSanityChecks state

initialize :: IO E
initialize = E <$> newTVarIO [] <*> newTVarIO (A "USD") <*> newTVarIO []
