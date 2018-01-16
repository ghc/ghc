{-# LANGUAGE
            NoImplicitPrelude
           , ExistentialQuantification
  #-}

module Test where

hGetContents handle_ = handle_{ haType=SemiClosedHandle}

data HandleType = SemiClosedHandle

class Show a where
  show :: a -> a

-- they have to check whether the handle has indeed been closed.
data Handle__
  = forall dev . (Show dev) =>
    Handle__ {
      haDevice      :: !dev,
      haType        :: HandleType           -- type (read/write/append etc.)
}
