{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T17839 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Language.Haskell.TH
import qualified Control.Monad.Writer as W
import Data.Functor.Identity


type LetT m a = WriterT [Locus] m a

type Code m a = m (TExp a)

type LetCode m a = LetT m (TExp a)

data Locus = Locus

instance (Monoid w, Quote m) => Quote (WriterT w m) where
  newName x = W.lift (newName x)

instance (Monoid w, Quote m) => Quote (StateT w m) where
  newName x = W.lift (newName x)


locus :: (Locus -> LetCode m a) -> Code m a
locus = undefined

newTypedName :: Quote m => m (TExp a)
newTypedName = do
  n <- newName "n"
  return (TExp (VarE n))


gen :: Quote m => Locus -> (Code Identity (a -> b) -> LetCode m a -> LetCode m b) -> LetCode m (a -> b)
gen l f = do
  n <- newTypedName
  [|| \a -> $$(f (Identity n) [|| a ||]) ||]


mrfix :: forall a b m r . (Monad m, Ord a, Quote m)
      => (forall m . (a -> Code m (b -> r)) -> (a -> Code m b -> Code m r))
      -> (a -> Code m (b -> r))
mrfix f x =
  flip evalStateT Map.empty $
    locus $ \locus -> do
      m <- get
      let loop :: a -> LetT (StateT (Map.Map a (Identity (TExp (b -> r)))) m) (TExp (b -> r))
          loop n =
            case Map.lookup n m of
              Just (Identity v) -> return v
              Nothing -> do
                gen locus (\g y -> do
                  modify (Map.insert n g)
                  f loop n y)
      loop x


