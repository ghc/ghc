
foreign import "ilxHello" unsafe ilxHello :: IO ()

main :: IO ()
main = ilxHello >> main