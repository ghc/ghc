-- Test that unambiguous constructions remain valid when
-- DuplicateRecordFields is enabled

{-# LANGUAGE DuplicateRecordFields #-}

data S = MkS { x :: Int }
  deriving Show

data T = MkT { x :: Bool, y :: Bool -> Bool, tField :: Bool }

data U a = MkU { x :: a, y :: a }

-- Construction is unambiguous
s = MkS { x = 42 }
t = MkT { x = True, y = id, tField = False }

-- Pattern matching is unambiguous
get_x MkS{x=x} = x

-- Resolving ambiguous monomorphic updates
a = t { x = False, y = not, tField = True } -- only T has all these fields
b = s { x = 3 } :: S         -- type being pushed in
c = (t :: T) { x = False }   -- type signature on record expression

-- Unambiguous selectors are in scope normally
z = tField t

main = print (get_x b)
