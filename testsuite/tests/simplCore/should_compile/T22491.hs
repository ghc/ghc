{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T22491 (heapster_add_block_hints) where

import qualified Control.Exception as X
import Control.Applicative
import Control.Monad
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..), catches, Handler(..))
import Control.Monad.IO.Class
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Coerce (Coercible, coerce)
import Data.IORef
import Data.Kind (Type)
import Data.Monoid
import GHC.Exts (build)

failOnNothing :: Fail.MonadFail m => String -> Maybe a -> m a
failOnNothing err_str Nothing = Fail.fail err_str
failOnNothing _ (Just a) = return a

lookupLLVMSymbolModAndCFG :: HeapsterEnv -> String -> IO (Maybe (AnyCFG LLVM))
lookupLLVMSymbolModAndCFG _ _ = pure Nothing

heapster_add_block_hints :: HeapsterEnv -> String -> [Int] ->
                            (forall ext blocks ret.
                             CFG ext blocks ret ->
                             TopLevel Hint) ->
                            TopLevel ()
heapster_add_block_hints henv nm blks hintF =
  do env <- liftIO $ readIORef $ heapsterEnvPermEnvRef henv
     AnyCFG cfg <-
       failOnNothing ("Could not find symbol definition: " ++ nm) =<<
         io (lookupLLVMSymbolModAndCFG henv nm)
     let blocks = fmapFC blockInputs $ cfgBlockMap cfg
         block_idxs = fmapFC (blockIDIndex . blockID) $ cfgBlockMap cfg
     blkIDs <- case blks of
       [] -> pure $ toListFC (Some . BlockID) block_idxs
       _ -> forM blks $ \blk ->
         failOnNothing ("Block ID " ++ show blk ++
                        " not found in function " ++ nm)
                       (fmapF BlockID <$> intIndex blk (size blocks))
     env' <- foldM (\env' _ ->
                     permEnvAddHint env' <$>
                     hintF cfg)
       env blkIDs
     liftIO $ writeIORef (heapsterEnvPermEnvRef henv) env'

-----

data Some (f:: k -> Type) = forall x . Some (f x)

class FunctorF m where
  fmapF :: (forall x . f x -> g x) -> m f -> m g

mapSome :: (forall tp . f tp -> g tp) -> Some f -> Some g
mapSome f (Some x) = Some $! f x

instance FunctorF Some where fmapF = mapSome

type SingleCtx x = EmptyCtx ::> x

data Ctx k
  = EmptyCtx
  | Ctx k ::> k

type family (<+>) (x :: Ctx k) (y :: Ctx k) :: Ctx k where
  x <+> EmptyCtx = x
  x <+> (y ::> e) = (x <+> y) ::> e

data Height = Zero | Succ Height

data BalancedTree h (f :: k -> Type) (p :: Ctx k) where
  BalLeaf :: !(f x) -> BalancedTree 'Zero f (SingleCtx x)
  BalPair :: !(BalancedTree h f x)
          -> !(BalancedTree h f y)
          -> BalancedTree ('Succ h) f (x <+> y)

data BinomialTree (h::Height) (f :: k -> Type) :: Ctx k -> Type where
  Empty :: BinomialTree h f EmptyCtx

  PlusOne  :: !Int
           -> !(BinomialTree ('Succ h) f x)
           -> !(BalancedTree h f y)
           -> BinomialTree h f (x <+> y)

  PlusZero  :: !Int
            -> !(BinomialTree ('Succ h) f x)
            -> BinomialTree h f x

tsize :: BinomialTree h f a -> Int
tsize Empty = 0
tsize (PlusOne s _ _) = 2*s+1
tsize (PlusZero  s _) = 2*s

fmap_bin :: (forall tp . f tp -> g tp)
         -> BinomialTree h f c
         -> BinomialTree h g c
fmap_bin _ Empty = Empty
fmap_bin f (PlusOne s t x) = PlusOne s (fmap_bin f t) (fmap_bal f x)
fmap_bin f (PlusZero s t)  = PlusZero s (fmap_bin f t)
{-# INLINABLE fmap_bin #-}

fmap_bal :: (forall tp . f tp -> g tp)
         -> BalancedTree h f c
         -> BalancedTree h g c
fmap_bal = go
  where go :: (forall tp . f tp -> g tp)
              -> BalancedTree h f c
              -> BalancedTree h g c
        go f (BalLeaf x) = BalLeaf (f x)
        go f (BalPair x y) = BalPair (go f x) (go f y)
{-# INLINABLE fmap_bal #-}

traverse_bin :: Applicative m
             => (forall tp . f tp -> m (g tp))
             -> BinomialTree h f c
             -> m (BinomialTree h g c)
traverse_bin _ Empty = pure Empty
traverse_bin f (PlusOne s t x) = PlusOne s  <$> traverse_bin f t <*> traverse_bal f x
traverse_bin f (PlusZero s t)  = PlusZero s <$> traverse_bin f t
{-# INLINABLE traverse_bin #-}

traverse_bal :: Applicative m
             => (forall tp . f tp -> m (g tp))
             -> BalancedTree h f c
             -> m (BalancedTree h g c)
traverse_bal = go
  where go :: Applicative m
              => (forall tp . f tp -> m (g tp))
              -> BalancedTree h f c
              -> m (BalancedTree h g c)
        go f (BalLeaf x) = BalLeaf <$> f x
        go f (BalPair x y) = BalPair <$> go f x <*> go f y
{-# INLINABLE traverse_bal #-}

data Assignment (f :: k -> Type) (ctx :: Ctx k)
      = Assignment (BinomialTree 'Zero f ctx)

newtype Index (ctx :: Ctx k) (tp :: k) = Index { indexVal :: Int }

newtype Size (ctx :: Ctx k) = Size Int

intIndex :: Int -> Size ctx -> Maybe (Some (Index ctx))
intIndex i n | 0 <= i && i < sizeInt n = Just (Some (Index i))
             | otherwise = Nothing

size :: Assignment f ctx -> Size ctx
size (Assignment t) = Size (tsize t)

sizeInt :: Size ctx -> Int
sizeInt (Size n) = n

class FunctorFC (t :: (k -> Type) -> l -> Type) where
  fmapFC :: forall f g. (forall x. f x -> g x) ->
                        (forall x. t f x -> t g x)

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce

class FoldableFC (t :: (k -> Type) -> l -> Type) where
  foldMapFC :: forall f m. Monoid m => (forall x. f x -> m) -> (forall x. t f x -> m)
  foldMapFC f = foldrFC (mappend . f) mempty

  foldrFC :: forall f b. (forall x. f x -> b -> b) -> (forall x. b -> t f x -> b)
  foldrFC f z t = appEndo (foldMapFC (Endo #. f) t) z

  toListFC :: forall f a. (forall x. f x -> a) -> (forall x. t f x -> [a])
  toListFC f t = build (\c n -> foldrFC (\e v -> c (f e) v) n t)

foldMapFCDefault :: (TraversableFC t, Monoid m) => (forall x. f x -> m) -> (forall x. t f x -> m)
foldMapFCDefault = \f -> getConst . traverseFC (Const . f)
{-# INLINE foldMapFCDefault #-}

class (FunctorFC t, FoldableFC t) => TraversableFC (t :: (k -> Type) -> l -> Type) where
  traverseFC :: forall f g m. Applicative m
             => (forall x. f x -> m (g x))
             -> (forall x. t f x -> m (t g x))

instance FunctorFC Assignment where
  fmapFC = \f (Assignment x) -> Assignment (fmap_bin f x)
  {-# INLINE fmapFC #-}

instance FoldableFC Assignment where
  foldMapFC = foldMapFCDefault
  {-# INLINE foldMapFC #-}

instance TraversableFC Assignment where
  traverseFC = \f (Assignment x) -> Assignment <$> traverse_bin f x
  {-# INLINE traverseFC #-}

data CrucibleType

data TypeRepr (tp::CrucibleType) where

type CtxRepr = Assignment TypeRepr

data CFG (ext :: Type)
         (blocks :: Ctx (Ctx CrucibleType))
         (ret :: CrucibleType)
   = CFG { cfgBlockMap :: !(BlockMap ext blocks ret)
         }

type BlockMap ext blocks ret = Assignment (Block ext blocks ret) blocks

data Block ext (blocks :: Ctx (Ctx CrucibleType)) (ret :: CrucibleType) ctx
   = Block { blockID        :: !(BlockID blocks ctx)
           , blockInputs    :: !(CtxRepr ctx)
           }

newtype BlockID (blocks :: Ctx (Ctx CrucibleType)) (tp :: Ctx CrucibleType)
      = BlockID { blockIDIndex :: Index blocks tp }

data LLVM

data AnyCFG ext where
  AnyCFG :: CFG ext blocks ret
         -> AnyCFG ext

newtype StateContT s r m a
      = StateContT { runStateContT :: (a -> s -> m r)
                                   -> s
                                   -> m r
                   }

fmapStateContT :: (a -> b) -> StateContT s r m a -> StateContT s r m b
fmapStateContT = \f m -> StateContT $ \c -> runStateContT m (\v s -> (c $! f v) s)
{-# INLINE fmapStateContT #-}

applyStateContT :: StateContT s r m (a -> b) -> StateContT s r m a -> StateContT s r m b
applyStateContT = \mf mv ->
  StateContT $ \c ->
    runStateContT mf (\f -> runStateContT mv (\v s -> (c $! f v) s))
{-# INLINE applyStateContT #-}

returnStateContT :: a -> StateContT s r m a
returnStateContT = \v -> seq v $ StateContT $ \c -> c v
{-# INLINE returnStateContT #-}

bindStateContT :: StateContT s r m a -> (a -> StateContT s r m b) -> StateContT s r m b
bindStateContT = \m n -> StateContT $ \c -> runStateContT m (\a -> runStateContT (n a) c)
{-# INLINE bindStateContT #-}

instance Functor (StateContT s r m) where
  fmap = fmapStateContT

instance Applicative (StateContT s r m) where
  pure  = returnStateContT
  (<*>) = applyStateContT

instance Monad (StateContT s r m) where
  (>>=) = bindStateContT

instance MonadFail m => MonadFail (StateContT s r m) where
  fail = \msg -> StateContT $ \_ _ -> fail msg

instance MonadTrans (StateContT s r) where
  lift = \m -> StateContT $ \c s -> m >>= \v -> seq v (c v s)

instance MonadIO m => MonadIO (StateContT s r m) where
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (StateContT s r m) where
  throwM e = StateContT (\_k _s -> throwM e)

instance MonadCatch m => MonadCatch (StateContT s r m) where
  catch m hdl =
    StateContT $ \k s ->
      catch
        (runStateContT m k s)
        (\e -> runStateContT (hdl e) k s)

data TopLevelRO
data TopLevelRW
data Value

newtype TopLevel a =
  TopLevel_ (ReaderT TopLevelRO (StateContT TopLevelRW (Value, TopLevelRW) IO) a)
 deriving (Applicative, Functor, Monad, MonadFail, MonadThrow, MonadCatch)

instance MonadIO TopLevel where
  liftIO = io

io :: IO a -> TopLevel a
io f = TopLevel_ (liftIO f) `catches` [Handler handleIO]
  where
    rethrow :: X.Exception ex => ex -> TopLevel a
    rethrow ex = throwM (X.SomeException ex)

    handleIO :: X.IOException -> TopLevel a
    handleIO = rethrow

data HeapsterEnv = HeapsterEnv {
  heapsterEnvPermEnvRef :: IORef PermEnv
  }

data Hint where

data PermEnv = PermEnv {
  permEnvHints :: [Hint]
  }

permEnvAddHint :: PermEnv -> Hint -> PermEnv
permEnvAddHint env hint = env { permEnvHints = hint : permEnvHints env }

type family CtxToRList (ctx :: Ctx k) :: RList k where
  CtxToRList EmptyCtx = RNil
  CtxToRList (ctx' ::> x) = CtxToRList ctx' :> x

data RList a
  = RNil
  | (RList a) :> a
