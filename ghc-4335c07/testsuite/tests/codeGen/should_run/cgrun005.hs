-- !! answer: 65532

main = print foo

foo :: Int
foo = ((1 + 2 + 32767 - 4) * 6) --later? `div` 3
