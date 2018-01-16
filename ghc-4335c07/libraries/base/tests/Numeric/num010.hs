
module Main(main) where

main = sequence_ [ f x y | x <- [0,
                                 1000,
                                 1000000000000,              -- > 2^32
                                 1000000000000000000000000,  -- > 2^64
                                 -1000,
                                 -1000000000000,             -- < -2^32
                                 -1000000000000000000000000] -- < -2^64
                         , y <- [0, -10, 10] ]

f :: Integer -> Int -> IO ()
f x y = do putStrLn "------------------------"
           print x
           print y
           let d :: Double
               d = encodeFloat x y
               (xd, yd) = decodeFloat d
           let f :: Float
               f = encodeFloat x y
               (xf, yf) = decodeFloat f
           print d
           print xd
           print yd
           print f
           print xf
           print yf

