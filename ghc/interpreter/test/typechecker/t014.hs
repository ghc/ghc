--!!! Leaving out signature in polymorphic recursion

data Expr a = App (Expr (b -> a)) (Expr b)
            | K a

--eval :: Expr a -> a
eval (App f x) = (eval f) (eval x)
eval (K x)     = x
