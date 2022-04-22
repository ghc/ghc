module T21391a (readYamlFile) where

import Control.Monad (liftM)
import Control.Monad.Trans.Writer.Strict (tell, WriterT(..))

discard :: a -> b
discard x = discard x

data Pipe a = MkPipe a

sinkValue :: m ~ ResourceT IO => () -> Pipe (WriterT String m ())
sinkValue _ = tell' ()
  where
    tell' _ = lift' discard (tell "")

    lift' rest mr = MkPipe (liftM rest mr)
    {-# INLINE [1] lift' #-}

class FromYaml a where
    fromYaml :: () -> a

readYamlFile :: FromYaml a => a
readYamlFile = fromYaml (discard sinkValue)

newtype ResourceT m a = ResourceT { unResourceT :: IO a }

instance Monad m => Functor (ResourceT m) where
    fmap = discard
instance Monad m => Applicative (ResourceT m) where
    pure = discard
    (<*>) = discard
instance Monad m => Monad (ResourceT m) where
    (>>=) = discard

