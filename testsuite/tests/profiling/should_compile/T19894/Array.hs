{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Array
    (
      Array(..)
    , fromList
    , read
    , length
    , writeNUnsafe
    , MA.unsafeInlineIO
    , MA.memcmp
    , unsafeFreezeWithShrink
    , foldl'
    , unsafeIndexIO
    )

where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.IO (unsafePerformIO)
import GHC.Ptr (Ptr(..))
import Unfold (Unfold(..))
import Fold (Fold(..))
import qualified MArray as MA
import qualified Unfold as UF
import Prelude hiding (Foldable(..), read)

data Array a =
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused addres
    }

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MA.Array a -> Array a
unsafeFreeze (MA.Array as ae _) = Array as ae

{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = unsafeFreeze $ MA.fromList xs

{-# INLINE [1] writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = unsafeFreeze <$> MA.writeNUnsafe n

{-# INLINE unsafeThaw #-}
unsafeThaw :: Array a -> MA.Array a
unsafeThaw (Array as ae) = MA.Array as ae ae

{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr =  MA.length (unsafeThaw arr)

{-# INLINE [1] read #-}
read :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
read = UF.lmap unsafeThaw MA.read

{-# INLINE unsafeFreezeWithShrink #-}
unsafeFreezeWithShrink :: Storable a => MA.Array a -> Array a
unsafeFreezeWithShrink arr = unsafePerformIO $ do
  MA.Array as ae _ <- MA.shrinkToFit arr
  return $ Array as ae

{-# INLINE [1] foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = MA.foldl' f z (unsafeThaw arr)

{-# INLINE [1] unsafeIndexIO #-}
unsafeIndexIO :: forall a. Storable a => Array a -> Int -> IO a
unsafeIndexIO arr = MA.unsafeIndexIO (unsafeThaw arr)

