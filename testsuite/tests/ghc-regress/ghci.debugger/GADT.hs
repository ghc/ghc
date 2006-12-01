data Empty
data NonEmpty

data SafeList x y where
     Nil :: SafeList x Empty
     Cons:: x -> SafeList x y  -> SafeList x NonEmpty
--  deriving Show

safeHead :: SafeList x NonEmpty -> x
safeHead (Cons x _) = x

foo = Cons 3 (Cons 6 (Cons 9 Nil))


data Dict x where 
	Dict :: Num x => x -> Dict x
	
data Exist where
	Exist :: forall a. a -> Exist