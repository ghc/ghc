data Unboxed1 = Unboxed1 (# Int, Bool #)

data Unboxed2 = Unboxed2 (# Int, (# Int, Bool #) #)

o1 = Unboxed1 (# 5, True #)
o2 = Unboxed2 (# 6, (# 7, False #) #)

force_them :: Int
force_them = x + (if b then 1 else 2) + y + z + (if c then 3 else 4)
  where
    Unboxed1 (# x, b #) = o1
    Unboxed2 (# y, (# z, c #) #) = o2