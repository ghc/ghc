{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExtendedDefaultRules      #-}

module T12797 where

import Prelude
import Control.Monad.IO.Class
import Data.Kind (Type)

type family FuncArg (m :: (Type -> Type)) :: Maybe Type

test2 :: (MonadIO m, FuncArg m ~ 'Nothing) => m ()
test2 = liftIO $ print 6

