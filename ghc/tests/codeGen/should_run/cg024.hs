-- !!! test super-dictionary grabification
--

main = putStr (show (is_one (1.2::Double)))

is_one :: RealFloat a => a -> Bool

is_one x = x == 1.0
