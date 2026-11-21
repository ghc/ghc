{-# LANGUAGE ExperimentalDependentTypes #-}
module LexicalLetProvider (HiddenPayload, producePayload, lazyPayload) where

type HiddenPayload = exists a. (a, a -> Int)

{-# NOINLINE producePayload #-}
producePayload :: Bool -> HiddenPayload
producePayload True = (True, fromEnum)
producePayload False = ('b', fromEnum)

lazyPayload :: HiddenPayload
lazyPayload = (error "unused payload field" :: Bool, const 77)
