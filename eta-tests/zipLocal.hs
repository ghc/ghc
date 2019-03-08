import System.Environment (getArgs)

main :: IO ()
main =
  do { (z:_) <- getArgs
     ; let z' = read z
           zipWit f (x:xs) = f z' x : zipWit f xs
           zipWit _ _      = []
           list = [1..z']
     ; print (zipWit (+) list) }
