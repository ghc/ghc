{-# LANGUAGE ExistentialQuantification #-}

module T27415 where

import Control.Concurrent.MVar

data ResultVar b = forall a . ResultVar (a -> b) (MVar (Maybe a))

mkResultVar :: MVar (Maybe a) -> ResultVar a
mkResultVar = ResultVar id
