{-# LANGUAGE RankNTypes #-}
module Hang where
import Control.Monad
import Data.Char

data Event
  = EventBeginDocument
  | EventEndDocument
  | EventBeginDoctype
  | EventEndDoctype
  | EventInstruction
  | EventBeginElement
  | EventEndElement
  | EventContent Content
  | EventComment
  | EventCDATA

data Content
  = ContentText String
  | ContentEntity String


peek :: Monad m => Consumer a m (Maybe a)
peek = undefined

type Consumer i m r = forall o. ConduitM i o m r

tag :: forall m a b c o . Monad m =>
    ConduitM Event o m (Maybe c)
tag = do
    _ <- dropWS
    return undefined
  where
-- Add this and it works
--     dropWS :: Monad m => ConduitM Event o m (Maybe Event)
    dropWS = do
-- Swap these two lines and it works
        -- let x = undefined
        x <- peek
        let isWS =
                case x of
                    -- Remove some of these and it works
                    Just EventBeginDocument -> True
                    Just EventEndDocument -> True
                    Just EventBeginDoctype{} -> True
                    Just EventEndDoctype -> True
                    Just EventInstruction{} -> True
                    Just EventBeginElement{} -> False
                    Just EventEndElement{} -> False
                    Just (EventContent (ContentText t))
                        | all isSpace t -> True
                        | otherwise -> False
                    Just (EventContent ContentEntity{}) -> False
                    Just EventComment{} -> True
                    Just EventCDATA{} -> False
                    Nothing -> False
        if isWS then dropWS else return x


-- Inlined Instances

instance Functor (ConduitM i o m) where
    fmap f (ConduitM c) = ConduitM $ \rest -> c (rest . f)

instance Applicative (ConduitM i o m) where
    pure x = ConduitM ($ x)
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad (ConduitM i o m) where
    return = pure
    ConduitM f >>= g = ConduitM $ \h -> f $ \a -> unConduitM (g a) h

instance Monad m => Functor (Pipe l i o u m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance Monad m => Applicative (Pipe l i o u m) where
    pure = Done
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad m => Monad (Pipe l i o u m) where
    return = pure
    {-# INLINE return #-}

    HaveOutput p c o >>= fp = HaveOutput (p >>= fp)            c          o
    NeedInput p c    >>= fp = NeedInput  (p >=> fp)            (c >=> fp)
    Done x           >>= fp = fp x
    PipeM mp         >>= fp = PipeM      ((>>= fp) `liftM` mp)
    Leftover p i     >>= fp = Leftover   (p >>= fp)            i

newtype ConduitM i o m r = ConduitM
    { unConduitM :: forall b.
                    (r -> Pipe i i o () m b) -> Pipe i i o () m b
    }

data Pipe l i o u m r =
    HaveOutput (Pipe l i o u m r) (m ()) o
  | NeedInput (i -> Pipe l i o u m r) (u -> Pipe l i o u m r)
  | Done r
  | PipeM (m (Pipe l i o u m r))
  | Leftover (Pipe l i o u m r) l
