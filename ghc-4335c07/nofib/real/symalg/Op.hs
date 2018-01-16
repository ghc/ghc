module Op (opName, opName1, opPrec, opPrec1, toOp, toOp1, opAssoc) where

opPrec1 :: String -> Int

opPrec1 "-" = 1
opPrec1 _   = 0

opPrec :: String -> Int

opPrec "==" = 5
opPrec ">=" = 5
opPrec "<=" = 5
opPrec "/=" = 5
opPrec ">"  = 5
opPrec "<"  = 5

opPrec "+" = 4
opPrec "-" = 4
opPrec "*" = 3
opPrec "/" = 3
opPrec "^" = 2
opPrec _   = 0

opName1 :: String -> String
opName1 "-" = "neg"
opName1 _   = ""

opName :: String -> String
opName "==" = "equ"
opName ">=" = "gte"
opName "<=" = "lte"
opName "/=" = "ne"
opName ">"  = "gt"
opName "<"  = "lt"
opName "+" = "add"
opName "-" = "sub"
opName "*" = "mul"
opName "/" = "div"
opName "^" = "pow"
opName _   = ""

opAssoc :: String -> String
opAssoc "==" = "non"
opAssoc ">=" = "non"
opAssoc "<=" = "non"
opAssoc "/=" = "non"
opAssoc ">" = "non"
opAssoc "<" = "non"
opAssoc "+" = "left"
opAssoc "-" = "left"
opAssoc "*" = "left"
opAssoc "/" = "left"
opAssoc "^" = "right"
opAssoc _   = "non"

toOp :: String -> String
toOp "add" = "+"
toOp "sub" = "-"
toOp "mul" = "*"
toOp "div" = "/"
toOp "pow" = "^"
toOp "equ" = "=="
toOp "lt"  = "<"
toOp "gt"  = ">"
toOp "lte" = "<="
toOp "gte" = ">="
toOp "ne"  = "/="
toOp _     = ""

toOp1 :: String -> String
toOp1 "neg" = "-"
toOp1 _     = ""
