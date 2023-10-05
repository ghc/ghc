{-# LANGUAGE LambdaCase #-}

-- This implements a (very) simple API along the lines of those used by
-- algebraic effect systems, and it uses a distinct prompt tag to identify each
-- handler. This is not the approach taken by real effect systems for various
-- reasons, but it's a decent, minimal exercise of the continuations API.

import Control.Applicative
import ContIO

data HandlerTag f where
  HandlerTag :: PromptTag a
             -> (forall b. f b -> (b -> IO a) -> IO a)
             -> HandlerTag f

send :: HandlerTag f -> f b -> IO b
send (HandlerTag tag f) v = control0 tag $ \k -> f v (prompt tag . k . pure)

handle :: (HandlerTag f -> IO a)
       -> (forall b. f b -> (b -> IO a) -> IO a)
       -> IO a
handle f g = do
  tag <- newPromptTag
  prompt tag $ f (HandlerTag tag g)

data NonDet a where
  Choice :: NonDet Bool

handleNonDet :: (HandlerTag NonDet -> IO a) -> IO [a]
handleNonDet f = handle (fmap (:[]) . f) $ \Choice k ->
  liftA2 (++) (k True) (k False)

amb :: HandlerTag NonDet -> a -> a -> IO a
amb tag a b = send tag Choice >>= \case
  True  -> pure a
  False -> pure b

example :: IO [[(Integer, Char)]]
example =
  handleNonDet $ \tag1 ->
  handleNonDet $ \tag2 -> do
    x <- amb tag2  1   2
    y <- amb tag1 'a' 'b'
    pure (x, y)

main :: IO ()
main = print =<< example
