{-# LANGUAGE CApiFFI #-}
import Foreign

foreign import capi "wrapper"
  wrapBool :: Bool -> IO (FunPtr Bool)

main = pure ()
