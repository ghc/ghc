-- test the representation of literals and also explicit type annotations

module Main
where

import Language.Haskell.TH

main :: IO ()
main = mapM_ putStrLn [show an_integral, show an_int, show an_integer,
                       show an_char, show an_string, show an_fractional,
                       show an_float, show an_double]

an_integral :: Integer
an_integral = $( [| 42 |] )

an_int :: Int
an_int = $( [| 42 :: Int |] )

an_integer :: Integer
an_integer = $( [| 98765432123456789876 :: Integer |] )

an_char :: Char
an_char = $( [| 'x' |] )

an_string :: String
an_string = $( [| "A String" |] )

an_fractional :: Double
an_fractional = $( [| 1.2 |] )

an_float :: Float
an_float = $( [| 1.2 :: Float |] )

an_double :: Double
an_double = $( [| 1.2 :: Double |] )

