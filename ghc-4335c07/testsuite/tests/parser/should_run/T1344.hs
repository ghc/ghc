-- Just new test.  This would work without the bug being fixed.

a = '\x10ffff'
b = "Hello\x000000002c\32World\o00000000000000000000000000000000041"
c = "♯\00\&00\0"

main = do print a
          putStrLn b
          print c

