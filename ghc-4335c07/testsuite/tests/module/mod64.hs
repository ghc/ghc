-- !!! Pattern binding must bind (not an error in standard Haskell)
module M where
x = let ['a'] = "a" in 'a'
