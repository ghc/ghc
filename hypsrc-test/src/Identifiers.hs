module Identifiers where


foo, bar, baz :: Int -> Int -> Int
foo x y = x + x * bar y x * y + y
bar x y = y + x - baz x y - x + y
baz x y = x * y * y * y * x

quux :: Int -> Int
quux x = foo (bar x x) (bar x x)

norf :: Int -> Int -> Int -> Int
norf x y z
    | x < 0 = quux x
    | y < 0 = quux y
    | z < 0 = quux z
    | otherwise = norf (-x) (-y) (-z)


main :: IO ()
main = do
    putStrLn . show $ foo x y
    putStrLn . show $ quux z
  where
    x = 10
    y = 20
    z = 30
