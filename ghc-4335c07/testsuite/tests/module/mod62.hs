-- !!! Malformed binding (qualified)
module M where
x = let M.y = 'a' in M.y
