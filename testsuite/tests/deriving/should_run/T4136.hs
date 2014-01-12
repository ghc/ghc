module Main where

data T = (:=:) {- | (:!=:) -} deriving (Show,Read)

main
 = do  putStrLn ("show (:=:) = " ++ show (:=:))
       putStrLn ("read (show (:=:)) :: T = " ++
                  show (read (show (:=:)) :: T)) 

