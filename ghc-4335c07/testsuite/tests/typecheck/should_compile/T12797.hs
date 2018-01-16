{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExtendedDefaultRules      #-}

module T12797 where

import Prelude
import Control.Monad.IO.Class

type family FuncArg (m :: (* -> *)) :: Maybe *

test2 :: (MonadIO m, FuncArg m ~ 'Nothing) => m ()
test2 = liftIO $ print 6

