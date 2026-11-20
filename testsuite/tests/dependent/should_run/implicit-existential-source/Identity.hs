module Identity (forward) where
import Filter (Hidden)

forward :: Hidden a -> Hidden a
forward xs = id xs
