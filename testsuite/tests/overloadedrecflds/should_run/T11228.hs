{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

pattern MkS { x } = [x]
pattern MkT { x, y } = (x,y)

e :: ([Int], Int)
e = MkT { x = MkS { x = 0 }, y = 1 }

f t@(MkT { x = MkS { x = x }, y = y }) = t { x = x + y, y = y - x }
f _ = undefined

main = print (f e)
