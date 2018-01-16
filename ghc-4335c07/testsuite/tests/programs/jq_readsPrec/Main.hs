module Main where

data Vertex = V Int deriving (Read, Show)

main = do
    userInput <- getContents
    (parseVertex.lines) userInput report

report::Vertex -> IO ()
report int = putStr (show int)

parseVertex::[String] -> (Vertex -> IO ()) -> IO ()
parseVertex inputLines cont
 = case inputLines of
      (l1:rest) -> case (reads l1) of
                     [(x,"")] -> cont x
                     other    -> putStr
                                      ((showString "Error - retype the edges\n".                                      shows other) "")
      _         -> putStr "No Vertex"

