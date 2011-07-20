{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, InterruptibleFFI #-}

module TH_foreign where

import Foreign.Ptr
import Language.Haskell.TH

$(return [ForeignD (ImportF CCall Interruptible "&" (mkName "foo") (AppT (ConT ''Ptr) (ConT ''())))])

-- Should generate the same as this:
foreign import ccall interruptible "&" foo1 :: Ptr ()
