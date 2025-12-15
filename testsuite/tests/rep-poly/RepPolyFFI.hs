{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module RepPolyFFI1 where

import GHC.Exts

foreign import ccall safe "foo" foo
  :: forall {l} (a :: TYPE (BoxedRep l)). Array# a -> Array# a
