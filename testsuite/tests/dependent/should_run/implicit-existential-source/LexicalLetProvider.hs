{-# LANGUAGE ExperimentalDependentTypes, LinearTypes #-}
module LexicalLetProvider
  ( HiddenPayload, producePayload, lazyPayload, bottomPayload
  , LinearPayload
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
