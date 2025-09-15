module M
  ( mkB2
  ) where

import Control.Monad.Reader
import Data.Maybe

data A1 = A1 (Maybe String) (Maybe String) (Maybe String) (Maybe String)
data A2 = A2 A1 (Maybe String) (Maybe String) (Maybe String) (Maybe String)
                (Maybe String) (Maybe String) (Maybe String) (Maybe String)

data B1 = B1 !String !String !String !String
data B2 = B2 !B1 !String !String !String !String !String !String !String !String

type M a = ReaderT [(String, String)] (Either String) a

resolve :: Maybe String -> String -> M (Maybe String)
resolve (Just x) _ = pure (Just x)
resolve Nothing  v = asks $ lookup v

mkB1 :: A1 -> M B1
mkB1 (A1 a b c d) = do
  a' <- fromMaybe "" <$> resolve a "A"
  b' <- fromMaybe "" <$> resolve b "B"
  c' <- fromMaybe "" <$> resolve c "C"
  d' <- fromMaybe "" <$> resolve d "D"
  pure $ B1 a' b' c' d'

mkB2 :: A2 -> M B2
mkB2 (A2 a b c d e f g h i) = do
  a' <- mkB1 a
  b' <- fromMaybe "db" <$> resolve b "B"
  c' <- fromMaybe "dc" <$> resolve c "C"
  d' <- fromMaybe "dd" <$> resolve d "D"
  e' <- fromMaybe "de" <$> resolve e "E"
  f' <- fromMaybe "df" <$> resolve f "F"
  g' <- fromMaybe "dg" <$> resolve g "G"
  h' <- fromMaybe "dh" <$> resolve h "H"
  i' <- fromMaybe "di" <$> resolve i "I"
  pure $ B2 a' b' c' d' e' f' g' h' i'
