import GHC.InfoProv

hello :: String
hello = "hello"

main :: IO ()
main = whereFrom hello >>= print
