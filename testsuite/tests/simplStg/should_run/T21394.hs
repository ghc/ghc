{-# LANGUAGE MagicHash #-}

-- Checks that identity record updates reuse the old record when the
-- updated field already contains the nullary constructor being written
-- (see Note [Identity record updates] in GHC.Stg.IdentityUpdate), and
-- that the transformation preserves semantics: no reuse when the field
-- differs, other fields unchanged, and no forcing of the old field.

import GHC.Exts

data T = A | B | C deriving (Eq, Show)

data R = R { f1 :: T, f2 :: Int, f3 :: String }

setB :: R -> R
-- lazy prevents CPR worker/wrapper from splitting the update apart;
-- cf. the same trick in T9291.
setB r = lazy (r { f1 = B })
{-# NOINLINE setB #-}

mkR :: T -> Int -> String -> R
mkR = R
{-# NOINLINE mkR #-}

same :: a -> a -> Bool
same x y = isTrue# (reallyUnsafePtrEquality# x y)

main :: IO ()
main = do
    -- Hit: field is already B. It must be evaluated *before* the record
    -- is built, so that the field holds a tagged pointer to B's static
    -- closure; a field holding a thunk or indirection that merely
    -- evaluates to B is (conservatively) a miss.
    let !b = idT B
        !hit = mkR b 1 "one"
    let !hit' = setB hit
    print (same hit hit')                     -- True
    print (f1 hit', f2 hit', f3 hit')         -- (B,1,"one")

    -- Miss: field is A; a new record must be allocated.
    let !miss = mkR (idT A) 2 "two"
    let !miss' = setB miss
    print (same miss miss')                   -- False
    print (f1 miss', f2 miss', f3 miss')      -- (B,2,"two")

    -- The old field must not be forced by the update, even when it is
    -- a diverging thunk.
    let boom = mkR (error "forced the old field!") 3 "three"
        boom' = setB boom
    print (f1 boom', f2 boom', f3 boom')      -- (B,3,"three")

idT :: T -> T
idT t = t
{-# NOINLINE idT #-}
