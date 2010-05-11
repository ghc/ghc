{- Note that declarations without type signatures are not included in the
   documentation. They could be, but that's a missing feature. -}

module Bug8 where

infix -->
infix --->

data Typ = Type (String,[Typ])
         | TFree (String, [String])

x --> y = Type("fun",[s,t])
(--->) = flip $ foldr (-->)

s = undefined
t = undefined
main = undefined
