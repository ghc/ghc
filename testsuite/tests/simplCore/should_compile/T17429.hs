{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module T17429
  ( zoomAcceptor
  ) where

type Zoom m = ( m ~ Emitter Int )

zoomAcceptor :: Zoom m => Emitter w a -> m w
zoomAcceptor = fmap id . zoomEmitter

zoomEmitter :: Emitter w a -> Emitter b w
zoomEmitter (Emitter go) =
  Emitter $ const ([], fst $ go ())

newtype Emitter w a = Emitter (() -> ([w], [a]))

instance Functor (Emitter w) where
  fmap f (Emitter go) = Emitter mapped
    where
    {-# INLINE mapped #-}
    mapped _ = fmap f <$> go ()
