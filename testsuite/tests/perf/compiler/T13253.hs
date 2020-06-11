-- Exponential with GHC 8.10

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module T13253 where

import Control.Monad (liftM)
import Control.Monad.Trans.RWS.Lazy -- check how strict behaves
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString (ByteString)
import Data.Monoid (Any (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import System.Environment (getEnv)

type Handler = ReaderT () IO
type MForm = RWST (Maybe ([(String, Text)], ()), (), ()) Any [Int]
type Text = ByteString -- close enough

data HugeStruct = HugeStruct
  !Text
  !Text
  !Text
  !Text
  !Text
  !Text
  !Text
  !Text
  !Text -- 9th
  !Text
  !Text

data FormResult a = FormMissing
                  | FormFailure [Text]
                  | FormSuccess a
    deriving Show
instance Functor FormResult where
    fmap _ FormMissing = FormMissing
    fmap _ (FormFailure errs) = FormFailure errs
    fmap f (FormSuccess a) = FormSuccess $ f a
instance Applicative FormResult where
    pure = FormSuccess
    (FormSuccess f) <*> (FormSuccess g) = FormSuccess $ f g
    (FormFailure x) <*> (FormFailure y) = FormFailure $ x ++ y
    (FormFailure x) <*> _ = FormFailure x
    _ <*> (FormFailure y) = FormFailure y
    _ <*> _ = FormMissing
instance Monoid m => Monoid (FormResult m) where
    mempty = pure mempty
    mappend x y = mappend <$> x <*> y
instance Semigroup m => Semigroup (FormResult m) where
    x <> y = (<>) <$> x <*> y

mreq :: MonadIO m => String -> MForm m (FormResult Text, ())
-- fast
--mreq v = pure (FormFailure [], ())
-- slow
mreq v = mhelper v (\m l -> FormFailure ["fail"]) FormSuccess

askParams :: Monad m => MForm m (Maybe [(String, Text)])
askParams = do
    (x, _, _) <- ask
    return $ liftM fst x

mhelper
    :: MonadIO m
    => String
    -> (() -> () -> FormResult b) -- on missing
    -> (Text -> FormResult b)      -- on success
    -> MForm m (FormResult b, ())
mhelper v onMissing onFound = do
    -- without tell, also faster
    tell (Any True)
    -- with different "askParams": faster.
    -- mp <- liftIO $ read <$> readFile v
    mp <- askParams
    (res, x) <- case mp of
        Nothing -> return (FormMissing, ())
        Just p -> do
            return $ case lookup v p of
                Nothing -> (onMissing () (), ())
                Just t -> (onFound t, ())
    return (res, x)

-- not inlining, also faster:
-- {-# NOINLINE mhelper #-}

sampleForm2 :: MForm Handler (FormResult HugeStruct)
sampleForm2 = do
    (x01, _) <- mreq "UNUSED"
    (x02, _) <- mreq "UNUSED"
    (x03, _) <- mreq "UNUSED"
    (x04, _) <- mreq "UNUSED"
    (x05, _) <- mreq "UNUSED"
    (x06, _) <- mreq "UNUSED"
    (x07, _) <- mreq "UNUSED"
    (x08, _) <- mreq "UNUSED"
    (x09, _) <- mreq "UNUSED"
    (x10, _) <- mreq "UNUSED"
    (x11, _) <- mreq "UNUSED"

    let hugeStructRes = HugeStruct
          <$> x01
          <*> x02
          <*> x03
          <*> x04
          <*> x05
          <*> x06
          <*> x07
          <*> x08
          <*> x09
          <*> x10
          <*> x11

    pure hugeStructRes


main :: IO ()
main = pure ()
