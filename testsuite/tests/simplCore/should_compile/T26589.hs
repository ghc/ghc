module T26589 ( executeTest ) where

-- base
import Data.Coerce ( coerce )
import Data.Foldable ( foldMap )

--------------------------------------------------------------------------------

newtype Traversal f = Traversal { getTraversal :: f () }

instance Applicative f => Semigroup (Traversal f) where
  Traversal f1 <> Traversal f2 = Traversal $ f1 *> f2
instance Applicative f => Monoid (Traversal f) where
  mempty = Traversal $ pure ()

newtype Seq a = Seq (FingerTree (Elem a))
newtype Elem a = Elem { getElem :: a }

data FingerTree a
    = EmptyT
    | Deep !a (FingerTree a) !a

executeTest :: Seq () -> IO ()
executeTest fins = destroyResources
  where
    destroyResources :: IO ()
    destroyResources =
      getTraversal $
        flip foldMap1 fins $ \ _ ->
          Traversal $ return ()

foldMap1 :: forall m a. Monoid m => (a -> m) -> Seq a -> m
foldMap1 = coerce (foldMap2 :: (Elem a -> m) -> FingerTree (Elem a) -> m)

foldMap2 :: Monoid m => (Elem a -> m) -> FingerTree (Elem a) -> m
foldMap2 _ EmptyT = mempty
foldMap2 f' (Deep pr' m' sf') = f' pr' <> foldMapTree f' m' <> f' sf'
      where
        foldMapTree :: Monoid m => (a -> m) -> FingerTree a -> m
        foldMapTree _ EmptyT = mempty
        foldMapTree f (Deep pr m sf) =
            f pr <>
            foldMapTree f m <>
            f sf
