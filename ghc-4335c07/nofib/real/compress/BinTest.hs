module BinTest (main) where

main = putStr (show (result 1000))

result 0 = []
result n = codes_to_ascii (3077, 1192) ++ result (n-1)

codes_to_ascii (x,y)
	= x_div : ((x_rem * 16) + y_div) : [y_rem]
          where
          (x_div, x_rem) = divRem x 16
          (y_div, y_rem) = divRem y 256

divRem x y = (x `div` y, x `rem` y) -- missing from PreludeCore ?
