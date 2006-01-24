infix -->
infix --->

data Typ = Type (String,[Typ])
         | TFree (String, [String])

x --> y = Type("fun",[s,t])
(--->) = flip $ foldr (-->)
