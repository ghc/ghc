{-# LANGUAGE MagicHash #-}

import GHC.Exts

newtype Age = Age Int

{-# NOINLINE myMap #-}
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

{-# RULES "map id2" myMap (\x -> x) = id #-}

mapId = myMap id
mapIdApp x = myMap id x

mapLamId = myMap (\x -> x)
mapLamIdApp x = myMap (\x -> x) x


same :: a -> a -> IO ()
same x y = case reallyUnsafePtrEquality# x y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"

main = do
    let l = [1,2,3]
    same (mapId l) l
    same (mapIdApp l) l
    same (mapLamId l) l
    same (mapLamIdApp l) l
