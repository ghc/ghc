{-# LANGUAGE Haskell2010 #-}
module Bug8 where

infix -->
infix --->

data Typ = Type (Typ,[Typ])
         | TFree (Typ, [Typ])

x --> y = Type(s,[s,t])
(--->) :: (Foldable t0) => t0 t -> Typ -> Typ
(--->) = flip $ foldr (-->)

s = undefined
t = undefined
main = undefined
