
module Main( main ) where


main :: IO ()
main = seq (error "hello world!" :: Int) (return ())


