module Bug where

pokeArray :: () -> ()
pokeArray = pokeArray

pokeSockAddr :: String -> () -> ()
pokeSockAddr path p = (case path of ('\0':_) -> pokeArray) p
