module Test where

-- should fail; doesn't with happy 1.8.
f = f where b = f
            c = (b
