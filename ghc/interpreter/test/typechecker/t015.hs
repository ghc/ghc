--!!! Can't derive instances if you use existentials

data Expr a = App (Expr (b -> a)) (Expr b)
            | K a
 deriving (Show)
