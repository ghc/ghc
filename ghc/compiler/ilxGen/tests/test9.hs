data Tree a = Node (Tree a) (Tree a)

left (Node x y) = x
right (Node x y) = y

choose (Node (Node _ _) (Node _ _)) = "hello world!\n"

mk f = f (mk f) (mk f)
main = putStr (choose (mk Node))

