import MiniPrel

a :: Array Int Int
a = array (1,100) ((1 := 1) : [i := i * a!(i-1) | i <- [2..100]])
