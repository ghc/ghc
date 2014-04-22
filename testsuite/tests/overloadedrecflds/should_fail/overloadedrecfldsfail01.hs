{-# LANGUAGE OverloadedRecordFields #-}

data R = MkR { w :: Bool, x :: Int, y :: Bool }
data S = MkS { w :: Bool, x :: Int, y :: Bool }
data T = MkT { x :: Int, z :: Bool }
data U = MkU { y :: Bool }

-- Straightforward ambiguous update
upd1 r = r { x = 3 }

-- No type has all these fields
upd2 r = r { x = 3, y = True, z = False }

-- User-specified type does not have these fields
upd3 r = r { w = True, x = 3, y = True } :: U

main = return ()