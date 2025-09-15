-- Test that ambiguous selectors can be disambiguated by providing
-- type signatures in various places

{-# LANGUAGE DuplicateRecordFields #-}

data S = MkS { x :: Int }
data T = MkT { x :: Bool }
data U a = MkU { x :: a }

x_for_s :: S -> Int
x_for_s = x

x_for_t = x :: T -> Bool

x_for_u u = x (u :: U Int)

k :: (T -> Bool) -> Bool
k f = f (MkT True)

main = do print (x_for_s (MkS 42))
          print (k x)
