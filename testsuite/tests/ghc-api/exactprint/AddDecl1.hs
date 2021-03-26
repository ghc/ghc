module AddDecl where

-- Adding a declaration to an existing file

-- | Do foo
foo a b = a + b

-- | Do bar
bar x y = {- baz -} foo (x+y) x

-- end of file
