data BigFam = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P

{-# NOINLINE lateSwitch #-}
lateSwitch P = "Cool"

main = putStrLn $ lateSwitch P
