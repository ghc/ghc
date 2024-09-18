{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module T25272 where
import Prelude (Integer, String, ($), (>>=), mempty)
import qualified Prelude
import GHC.Exts (ByteArray#)
import GHC.IO (IO(..))
import Language.Haskell.TH.Syntax (lift, Bytes(..), runIO)
import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import Data.Array.Byte

fromInteger :: Integer -> String
fromInteger _ = "yikes"

x :: Bytes
x = $(do
  let fromInteger = Prelude.fromInteger
  b <- runIO $ newForeignPtr_ nullPtr
  lift $ Bytes b 0 0
  )

y :: ByteArray
y = $(lift (mempty :: ByteArray))
