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

{-

   ( MakeValueTuple Vector  =  MakeValueTuple (Serial Float)
   , ValueTuple Vector ~ ValueTuple Vector    (agf))

--> MakeValueTuple Float
--> <solved>
-}


withArgs ::
   (MakeValueTuple b) =>
   (a, ValueTuple b) ->
   (a, b)
withArgs = undefined

plug ::
   (MakeValueTuple c) =>
   (c, ValueTuple c)
plug = undefined

filterFormants :: (Float, Vector)
filterFormants = withArgs plug

{- Call to withArgs generates

    (MakeValueTuple Vector, (c,ValueTuple c)~(Float,ValueTuple Vector), MakeValueTuple c)
=   (MakeValueTuple (Serial Float), ValueTuple Float ~ ValueTuple (Serial Float), MakeValueTuple Float)
=   (MakeValueTuple (Serial Float), MakeValueTuple Float,
     IO Float ~ Serial (ValueTuple Float))

     IO Float ~ Serial f, ValueTuple Float ~ f
=    IO Float ~ Serial f, IO Float ~ f
-}
