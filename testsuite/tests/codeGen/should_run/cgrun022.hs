-- !!! tests stack stubbing: if "f" doesn't stub "ns",
-- !!! the program has a space leak.

module Main where

main = f (putStr "a")
	 (take 1000000 (repeat True))
	 (putStr "b")

f a ns b = if last ns then a else b
