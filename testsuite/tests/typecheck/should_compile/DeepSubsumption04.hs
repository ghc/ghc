{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE KindSignatures #-}
module DeepSubsumption04 where

import Data.Kind

data TermOutput = TermOutput

type TermAction = () -> TermOutput

type ActionT = WriterT TermAction

class MonadReader r m  where
  ask :: m r

data WriterT w (m :: Type -> Type) a = WriterT

type ActionM a = forall m . (MonadReader () m) => ActionT m a

output :: TermAction -> ActionM ()
output t = undefined

termText :: String -> TermOutput
termText = undefined

outputText :: String -> ActionM ()
outputText = output . const . termText
--outputText x = output . const . termText $ x

