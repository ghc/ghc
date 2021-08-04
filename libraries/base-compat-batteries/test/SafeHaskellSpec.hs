{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
module SafeHaskellSpec (main, spec) where

import TestHspecTrustworthy

-- The following modules should not be Unsafe (#56):
import Control.Concurrent.Compat ()
import Control.Concurrent.MVar.Compat ()
import Control.Exception.Compat ()
import Control.Monad.Compat ()
import Control.Monad.Fail.Compat ()
import Control.Monad.IO.Class.Compat ()
import Data.Bifoldable.Compat ()
import Data.Bifunctor.Compat ()
import Data.Bitraversable.Compat ()
import Data.Bits.Compat ()
import Data.Bool.Compat ()
import Data.Complex.Compat ()
import Data.Either.Compat ()
import Data.Foldable.Compat ()
import Data.Function.Compat ()
import Data.Functor.Compat ()
import Data.Functor.Compose.Compat ()
import Data.Functor.Const.Compat ()
import Data.Functor.Contravariant.Compat ()
import Data.Functor.Identity.Compat ()
import Data.Functor.Product.Compat ()
import Data.Functor.Sum.Compat ()
import Data.IORef.Compat ()
import Data.List.Compat ()
import Data.List.NonEmpty.Compat ()
import Data.Monoid.Compat ()
import Data.Proxy.Compat ()
import Data.Ratio.Compat ()
import Data.Semigroup.Compat ()
import Data.STRef.Compat ()
import Data.String.Compat ()
import Data.Type.Coercion.Compat ()
import Data.Type.Equality.Compat ()
import Data.Version.Compat ()
import Data.Void.Compat ()
import Data.Word.Compat ()
import Foreign.Compat ()
import Foreign.ForeignPtr.Compat ()
import Foreign.ForeignPtr.Safe.Compat ()
import Foreign.Marshal.Alloc.Compat ()
import Foreign.Marshal.Array.Compat ()
import Foreign.Marshal.Compat ()
import Foreign.Marshal.Safe.Compat ()
import Foreign.Marshal.Utils.Compat ()
import Numeric.Compat ()
import Numeric.Natural.Compat ()
import Prelude.Compat
import System.Environment.Compat ()
import System.Exit.Compat ()
import System.IO.Compat ()
import System.IO.Error.Compat ()
import Text.Read.Compat ()
import Type.Reflection.Compat ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
