--!!! Expr a (the classic existential types + polymorphic recursion example)

data Expr a = App (Expr (b -> a)) (Expr b)
            | K a

eval :: Expr a -> a
eval (App f x) = (eval f) (eval x)
eval (K x)     = x
