module T1 where
malloc :: IO Int
malloc = _casm_ ``%r = 42;''
