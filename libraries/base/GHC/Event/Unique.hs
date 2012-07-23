{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, NoImplicitPrelude #-}
module GHC.Event.Unique
    (
      UniqueSource
    , Unique(..)
    , newSource
    , newUnique
    ) where

import Data.Int (Int64)
import GHC.Base
import GHC.Conc.Sync (TVar, atomically, newTVarIO, readTVar, writeTVar)
import GHC.Num (Num(..))
import GHC.Show (Show(..))

-- We used to use IORefs here, but Simon switched us to STM when we
-- found that our use of atomicModifyIORef was subject to a severe RTS
-- performance problem when used in a tight loop from multiple
-- threads: http://hackage.haskell.org/trac/ghc/ticket/3838
--
-- There seems to be no performance cost to using a TVar instead.

newtype UniqueSource = US (TVar Int64)

newtype Unique = Unique { asInt64 :: Int64 }
    deriving (Eq, Ord, Num)

instance Show Unique where
    show = show . asInt64

newSource :: IO UniqueSource
newSource = US `fmap` newTVarIO 0

newUnique :: UniqueSource -> IO Unique
newUnique (US ref) = atomically $ do
  u <- readTVar ref
  let !u' = u+1
  writeTVar ref u'
  return $ Unique u'
{-# INLINE newUnique #-}

