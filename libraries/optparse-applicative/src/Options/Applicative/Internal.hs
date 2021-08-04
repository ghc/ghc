module Options.Applicative.Internal
  ( P
  , MonadP(..)
  , ParseError(..)

  , uncons
  , hoistMaybe
  , hoistEither
  , runReadM
  , withReadM

  , runP

  , Completion
  , runCompletion
  , contextNames

  , ListT
  , takeListT
  , runListT

  , NondetT
  , cut
  , (<!>)
  , disamb
  ) where

import Control.Applicative
import Prelude
import Control.Monad (MonadPlus(..), liftM, ap, guard)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except
  (runExcept, runExceptT, withExcept, ExceptT(..), throwE)
import Control.Monad.Trans.Reader
  (mapReaderT, runReader, runReaderT, Reader, ReaderT, ask)
import Control.Monad.Trans.State (StateT, get, put, modify, evalStateT, runStateT)

import Options.Applicative.Types

class (Alternative m, MonadPlus m) => MonadP m where
  enterContext :: String -> ParserInfo a -> m ()
  exitContext :: m ()
  getPrefs :: m ParserPrefs

  missingArgP :: ParseError -> Completer -> m a
  errorP :: ParseError -> m a
  exitP :: IsCmdStart -> ArgPolicy -> Parser b -> Maybe a -> m a

newtype P a = P (ExceptT ParseError (StateT [Context] (Reader ParserPrefs)) a)

instance Functor P where
  fmap f (P m) = P $ fmap f m

instance Applicative P where
  pure a = P $ pure a
  P f <*> P a = P $ f <*> a

instance Alternative P where
  empty = P empty
  P x <|> P y = P $ x <|> y

instance Monad P where
  return = pure
  P x >>= k = P $ x >>= \a -> case k a of P y -> y

instance MonadPlus P where
  mzero = P mzero
  mplus (P x) (P y) = P $ mplus x y

contextNames :: [Context] -> [String]
contextNames ns =
  let go (Context n _) = n
  in  reverse $ go <$> ns

instance MonadP P where
  enterContext name pinfo = P $ lift $ modify $ (:) $ Context name pinfo
  exitContext = P $ lift $ modify $ drop 1
  getPrefs = P . lift . lift $ ask

  missingArgP e _ = errorP e
  exitP i _ p = P . maybe (throwE . MissingError i . SomeParser $ p) return
  errorP = P . throwE

hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero return

hoistEither :: MonadP m => Either ParseError a -> m a
hoistEither = either errorP return

runP :: P a -> ParserPrefs -> (Either ParseError a, [Context])
runP (P p) = runReader . flip runStateT [] . runExceptT $ p

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

runReadM :: MonadP m => ReadM a -> String -> m a
runReadM (ReadM r) s = hoistEither . runExcept $ runReaderT r s

withReadM :: (String -> String) -> ReadM a -> ReadM a
withReadM f = ReadM . mapReaderT (withExcept f') . unReadM
  where
    f' (ErrorMsg err) = ErrorMsg (f err)
    f' e = e

data ComplResult a
  = ComplParser SomeParser ArgPolicy
  | ComplOption Completer
  | ComplResult a

instance Functor ComplResult where
  fmap = liftM

instance Applicative ComplResult where
  pure = ComplResult
  (<*>) = ap

instance Monad ComplResult where
  return = pure
  m >>= f = case m of
    ComplResult r -> f r
    ComplParser p a -> ComplParser p a
    ComplOption c -> ComplOption c

newtype Completion a =
  Completion (ExceptT ParseError (ReaderT ParserPrefs ComplResult) a)

instance Functor Completion where
  fmap f (Completion m) = Completion $ fmap f m

instance Applicative Completion where
  pure a = Completion $ pure a
  Completion f <*> Completion a = Completion $ f <*> a

instance Alternative Completion where
  empty = Completion empty
  Completion x <|> Completion y = Completion $ x <|> y

instance Monad Completion where
  return = pure
  Completion x >>= k = Completion $ x >>= \a -> case k a of Completion y -> y

instance MonadPlus Completion where
  mzero = Completion mzero
  mplus (Completion x) (Completion y) = Completion $ mplus x y

instance MonadP Completion where
  enterContext _ _ = return ()
  exitContext = return ()
  getPrefs = Completion $ lift ask

  missingArgP _ = Completion . lift . lift . ComplOption
  exitP _ a p _ = Completion . lift . lift $ ComplParser (SomeParser p) a
  errorP = Completion . throwE

runCompletion :: Completion r -> ParserPrefs -> Maybe (Either (SomeParser, ArgPolicy) Completer)
runCompletion (Completion c) prefs = case runReaderT (runExceptT c) prefs of
  ComplResult _ -> Nothing
  ComplParser p' a' -> Just $ Left (p', a')
  ComplOption compl -> Just $ Right compl

-- A "ListT done right" implementation

newtype ListT m a = ListT
  { stepListT :: m (TStep a (ListT m a)) }

data TStep a x
  = TNil
  | TCons a x

bimapTStep :: (a -> b) -> (x -> y) -> TStep a x -> TStep b y
bimapTStep _ _ TNil = TNil
bimapTStep f g (TCons a x) = TCons (f a) (g x)

hoistList :: Monad m => [a] -> ListT m a
hoistList = foldr (\x xt -> ListT (return (TCons x xt))) mzero

takeListT :: Monad m => Int -> ListT m a -> ListT m a
takeListT 0 = const mzero
takeListT n = ListT . liftM (bimapTStep id (takeListT (n - 1))) . stepListT

runListT :: Monad m => ListT m a -> m [a]
runListT xs = do
  s <- stepListT xs
  case s of
    TNil -> return []
    TCons x xt -> liftM (x :) (runListT xt)

instance Monad m => Functor (ListT m) where
  fmap f = ListT
         . liftM (bimapTStep f (fmap f))
         . stepListT

instance Monad m => Applicative (ListT m) where
  pure = hoistList . pure
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return = pure
  xs >>= f = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> return TNil
      TCons x xt -> stepListT $ f x `mplus` (xt >>= f)

instance Monad m => Alternative (ListT m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans ListT where
  lift = ListT . liftM (`TCons` mzero)

instance Monad m => MonadPlus (ListT m) where
  mzero = ListT (return TNil)
  mplus xs ys = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> stepListT ys
      TCons x xt -> return $ TCons x (xt `mplus` ys)

-- nondeterminism monad with cut operator

newtype NondetT m a = NondetT
  { runNondetT :: ListT (StateT Bool m) a }

instance Monad m => Functor (NondetT m) where
  fmap f = NondetT . fmap f . runNondetT

instance Monad m => Applicative (NondetT m) where
  pure = NondetT . pure
  NondetT m1 <*> NondetT m2 = NondetT (m1 <*> m2)

instance Monad m => Monad (NondetT m) where
  return = pure
  NondetT m1 >>= f = NondetT $ m1 >>= runNondetT . f

instance Monad m => MonadPlus (NondetT m) where
  mzero = NondetT mzero
  NondetT m1 `mplus` NondetT m2 = NondetT (m1 `mplus` m2)

instance Monad m => Alternative (NondetT m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans NondetT where
  lift = NondetT . lift . lift

(<!>) :: Monad m => NondetT m a -> NondetT m a -> NondetT m a
(<!>) m1 m2 = NondetT . mplus (runNondetT m1) $ do
  s <- lift get
  guard (not s)
  runNondetT m2

cut :: Monad m => NondetT m ()
cut = NondetT $ lift (put True)

disamb :: Monad m => Bool -> NondetT m a -> m (Maybe a)
disamb allow_amb xs = do
  xs' <- (`evalStateT` False)
       . runListT
       . takeListT (if allow_amb then 1 else 2)
       . runNondetT $ xs
  return $ case xs' of
    [x] -> Just x
    _   -> Nothing
