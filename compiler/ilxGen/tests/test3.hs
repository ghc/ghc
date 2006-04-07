foreign import "ilxHello" unsafe ilxHello :: IO ()
foreign import "ilxBad" unsafe ilxBad :: IO ()

class  Eqq a  where
    eqq		:: a -> Bool
    eqq2	:: a -> Bool

--    x /= y		= not (x == y)
--    x == y		= not (x /= y)
--    x /= y		=  True
    eqq x		=  False
    eqq2 x		=  True


data  Unit  =  Unit

instance Eqq Unit 
--  where
--    eqq Unit = True
--    eqq2 Unit = False

choose x = if eqq x then ilxHello else if eqq2 x then ilxBad else ilxBad

main = choose Unit
