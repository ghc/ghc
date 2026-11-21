{-# LANGUAGE ExperimentalDependentTypes, LinearTypes #-}
module LexicalLetProvider
  ( HiddenPayload, producePayload, lazyPayload, bottomPayload
  , LinearPayload, linearPayload
  ) where

type HiddenPayload = exists a. (a, a -> Int)

{-# NOINLINE producePayload #-}
producePayload :: Bool -> HiddenPayload
producePayload True = (True, fromEnum)
producePayload False = ('b', fromEnum)

lazyPayload :: HiddenPayload
lazyPayload = (error "unused payload field" :: Bool, const 77)

bottomPayload :: Bool -> HiddenPayload
bottomPayload _ = error "bottom package"

type LinearPayload = exists a. (a, a %1 -> Int)

linearPayload :: Bool -> LinearPayload
linearPayload flag = (flag, readFlag)
  where
    readFlag :: Bool %1 -> Int
    readFlag True = 5
    readFlag False = 9
