{-# LANGUAGE TypeFamilies #-}

module T7010 where

type Vector = Serial Float
data Serial v = Serial

class MakeValueTuple a where
   type ValueTuple a :: *

instance MakeValueTuple Float where
   type ValueTuple Float = IO Float

instance (MakeValueTuple v) => MakeValueTuple (Serial v) where
   type ValueTuple (Serial v) = Serial (ValueTuple v)


stereoFromMono :: (v, v)
stereoFromMono = undefined

processIO ::
   (MakeValueTuple a) =>
   (ValueTuple a, ValueTuple a) ->
   (a, a)
processIO = undefined


phoneme :: (Vector, Vector)
phoneme = processIO stereoFromMono


withArgs ::
   (MakeValueTuple b) =>
   (a, ValueTuple b) ->
   (a, b)
withArgs = undefined

plug ::
   (MakeValueTuple b) =>
   (b, ValueTuple b)
plug = undefined

filterFormants :: (Float, Vector)
filterFormants = withArgs plug
