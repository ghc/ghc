{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

class A a
class B a where b :: a -> ()
instance A a => B a where b = undefined

newtype Y a = Y (a -> ())

okIn701 :: B a => Y a
okIn701 = wrap $ const () . b

okIn702 :: B a => Y a
okIn702 = wrap $ b

okInBoth :: B a => Y a
okInBoth = Y $ const () . b

class Wrapper a where
    type Wrapped a
    wrap :: Wrapped a -> a
instance Wrapper (Y a) where
  type Wrapped (Y a) = a -> ()
  wrap = Y

fromTicket3018 :: Eq [a] => a -> ()
fromTicket3018 x = let {g :: Int -> Int; g = [x]==[x] `seq` id} in ()

main = undefined

