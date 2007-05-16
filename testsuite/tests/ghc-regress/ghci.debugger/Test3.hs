mymap f [] = []
mymap f (x:xs) = f x:mymap f xs

main = mapM_ putStrLn $ mymap ('a':) ["hello","bye"] 
