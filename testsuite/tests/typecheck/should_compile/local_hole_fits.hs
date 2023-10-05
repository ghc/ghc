import Prelude (Show, String, error, IO(), show, Ordering(EQ))

head :: [a] -> a
head (x:xs) = _
head _ = error "Empty list!"

mshow :: Show a => a -> a
mshow a = _

main :: IO ()
main = error "no main"
