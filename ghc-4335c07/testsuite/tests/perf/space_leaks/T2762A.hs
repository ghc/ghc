
module T2762A (input) where

class InputOutput a where
    input :: String -> (a, String)

instance InputOutput Char where
    input (x : bs) = (x, bs)

instance InputOutput a => InputOutput [a] where
    input ('0':bs) = ([], bs)
    input ('1':bs) = case input bs of
                     (x, bs') ->
                         case input bs' of
                         ~(xs, bs'') -> (x : xs, bs'')
