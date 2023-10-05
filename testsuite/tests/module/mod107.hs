-- !!! Redefining and using Prelude entities
module F where

sin :: Float -> Float
sin x = (x::Float)
f x = Prelude.sin (F.sin x)
