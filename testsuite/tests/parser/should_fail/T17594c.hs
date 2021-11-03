module T17594c where

foo :: forall a m. Monad m => a -> m a
foo x = do
  @a <- undefined
  return (x :: a)
