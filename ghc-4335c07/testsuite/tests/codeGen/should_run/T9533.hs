import Data.Word

x :: Word
x = 10

y :: Word
y = 11

test = case x - y of
         5 -> "C"
         -1 -> "A"
         _  -> "B"
main = putStrLn $ show test
