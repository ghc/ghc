{-# LANGUAGE PolyKinds, DataKinds, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module CatPairs where
import Prelude hiding (id, (.))
import Control.Monad ((>=>))
import Control.Category

-- Categories over pairs of types.
-- Taken from Twan van Laarhoven:
--   http://twanvl.nl/blog/haskell/categories-over-pairs-of-types

type family Fst (xy :: (*,*)) :: *
type family Snd (xy :: (*,*)) :: *
type instance Fst '(x,y) = x
type instance Snd '(x,y) = y

-- Ceci n'est pas une pipe
data Pipe i o u m r = Pipe { runPipe :: Either i u -> m (Either o r) }

(>+>) :: Monad m
      => Pipe io1 io2 ur1 m ur2
      -> Pipe io2 io3 ur2 m ur3
      -> Pipe io1 io3 ur1 m ur3
(>+>) (Pipe f) (Pipe g) = Pipe (f >=> g)

idP :: Monad m => Pipe i i r m r
idP = Pipe return

newtype WrapPipe m iu or = WrapPipe
    { unWrapPipe :: Pipe (Fst iu) (Fst or) (Snd iu) m (Snd or) }

instance Monad m => Category (WrapPipe m) where
  id    = WrapPipe idP
  x . y = WrapPipe (unWrapPipe y >+> unWrapPipe x)
