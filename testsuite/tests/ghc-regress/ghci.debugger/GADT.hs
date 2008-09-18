data Empty
data NonEmpty

data SafeList x y where
     Nil :: SafeList x Empty
     Cons:: Eq x => x -> SafeList x y  -> SafeList x NonEmpty
     One :: Eq x => x -> SafeList x Empty -> SafeList x NonEmpty

safeHead :: SafeList x NonEmpty -> x
safeHead (Cons x _) = x

foo = Cons 3 (Cons 6 (Cons 9 Nil))


data Dict x where
        DictN :: Num x => x -> Dict x
        DictE :: Eq x =>  x -> Dict x

data Exist where
        Exist :: forall a. a -> Exist