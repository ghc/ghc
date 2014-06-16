{-# LANGUAGE TemplateHaskell,ForeignFunctionInterface #-}

module T3319 where

import Foreign.Ptr
import Language.Haskell.TH

$(return [ForeignD (ImportF CCall Unsafe "&" (mkName "foo") (AppT (ConT ''Ptr) (ConT ''())))])

-- Should generate the same as this:
foreign import ccall unsafe "&" foo1 :: Ptr ()
