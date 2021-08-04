{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Strict.Classes (
    Strict (..),
) where

import Prelude ((.))
import qualified Prelude as L
import qualified Data.These as L

import Data.Strict.These
import Data.Strict.Tuple
import Data.Strict.Maybe
import Data.Strict.Either

import qualified Control.Monad.ST.Lazy as L
import qualified Control.Monad.ST.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- | Ad hoc conversion between "strict" and "lazy" versions of a structure.
--
-- Unfortunately all externally defined instances are doomed to
-- be orphans: https://gitlab.haskell.org/ghc/ghc/-/issues/11999
-- See also https://qfpl.io/posts/orphans-and-fundeps/index.html for
--
class Strict lazy strict | lazy -> strict, strict -> lazy where
  toStrict :: lazy -> strict
  toLazy   :: strict -> lazy

instance Strict (L.Maybe a) (Maybe a) where
  toStrict L.Nothing  = Nothing
  toStrict (L.Just x) = Just x
  
  toLazy Nothing  = L.Nothing
  toLazy (Just x) = L.Just x

instance Strict (a, b) (Pair a b) where
  toStrict (a, b) = a :!: b
  toLazy (a :!: b) = (a, b)

instance Strict (L.Either a b) (Either a b) where
  toStrict (L.Left x)  = Left x
  toStrict (L.Right y) = Right y
  
  toLazy (Left x)  = L.Left x
  toLazy (Right y) = L.Right y

instance Strict (L.These a b) (These a b) where
  toStrict (L.This x)    = This x
  toStrict (L.That y)    = That y
  toStrict (L.These x y) = These x y
  
  toLazy (This x)    = L.This x
  toLazy (That y)    = L.That y
  toLazy (These x y) = L.These x y

instance Strict LBS.ByteString BS.ByteString where
#if MIN_VERSION_bytestring(0,10,0)
  toStrict = LBS.toStrict
  toLazy   = LBS.fromStrict
#else
  toStrict = BS.concat . LBS.toChunks
  toLazy   = LBS.fromChunks . L.return {- singleton -}
#endif

instance Strict LT.Text T.Text where
  toStrict = LT.toStrict
  toLazy   = LT.fromStrict

instance Strict (L.ST s a) (S.ST s a) where
  toStrict = L.lazyToStrictST
  toLazy   = L.strictToLazyST

instance Strict (L.RWST r w s m a) (S.RWST r w s m a) where
  toStrict = S.RWST . L.runRWST
  toLazy   = L.RWST . S.runRWST

instance Strict (L.StateT s m a) (S.StateT s m a) where
  toStrict = S.StateT . L.runStateT
  toLazy   = L.StateT . S.runStateT

instance Strict (L.WriterT w m a) (S.WriterT w m a) where
  toStrict = S.WriterT . L.runWriterT
  toLazy   = L.WriterT . S.runWriterT
