{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module T21703 where

data Doc
data Val
data Head

data EvalM a = EvalM a

class MonadEval head val m | m -> head, head -> val where
  ret :: a -> m a       

instance MonadEval Head Val EvalM where
  ret = EvalM

class PrettyM m a where
  prettyM :: a -> m a

instance PrettyM EvalM Val where
  prettyM = ret
