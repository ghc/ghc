module Main where


main :: IO ()
main = putStrLn . show $ qqq ['c']

qqq :: [a] -> Maybe (a, [a])
qqq ts = do { (a:b:as) <- Just ts
            ; return (a, as) }

newtype ST a b = ST (a, b)

ppp :: Maybe (ST Int Int) -> Maybe (ST Int Int)
ppp st = do { ST (x, y) <- st
            ; return $ ST (x+1, y+1)}


