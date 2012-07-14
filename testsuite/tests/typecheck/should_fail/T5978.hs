{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module T5978 where

class C from to | from -> to where

instance C Float Char where
instance C Double Bool where


polyFoo :: (C from to) => from
polyFoo = undefined

polyBar ::
   (C fromA toA, C fromB toB) =>
   (toA -> toB) ->
   fromA -> fromB
polyBar = undefined


monoBar :: Double
monoBar = polyBar id monoFoo

monoFoo :: Float
monoFoo = polyFoo

