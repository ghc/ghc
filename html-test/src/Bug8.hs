module Bug8 where

infix -->
infix --->

data Typ = Type (Typ,[Typ])
         | TFree (Typ, [Typ])

x --> y = Type(s,[s,t])
(--->) = flip $ foldr (-->)

s = undefined
t = undefined
main = undefined
