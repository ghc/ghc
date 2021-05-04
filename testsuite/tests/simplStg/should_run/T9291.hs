{-# LANGUAGE MagicHash #-}
import GHC.Exts
import Unsafe.Coerce

-- The use of lazy in this module prevents Nested CPR from happening.
-- Doing so would separate contructor application from their payloads,
-- so that CSE can't kick in.
-- This is unfortunate, but this testcase is about demonstrating
-- effectiveness of STG CSE.

foo :: Either Int a -> Either Bool a
foo (Right x) = Right x
foo _ = Left True
{-# NOINLINE foo #-}

bar :: a -> (Either Int a, Either Bool a)
-- Why lazy? See comment above; the worker would return (# x, x #)
bar x = (lazy $ Right x, lazy $ Right x)
{-# NOINLINE bar #-}

nested :: Either Int (Either Int a) -> Either Bool (Either Bool a)
nested (Right (Right x)) = Right (Right x)
nested _ = Left True
{-# NOINLINE nested #-}


-- CSE in a recursive group
data Tree x = T x (Either Int (Tree x)) (Either Bool (Tree x))
rec1 :: x -> Tree x
-- Why lazy? See comment above; the worker would return (# x, t, t #)
rec1 x =
    let t = T x r1 r2
        r1 = Right t
        r2 = Right t
    in lazy t
{-# NOINLINE rec1 #-}

-- Not yet supported! (and tricky)
data Stream a b x = S x (Stream b a x)
rec2 :: x -> Stream a b x
rec2 x =
    let s1 = S x s2
        s2 = S x s1
    in s1
{-# NOINLINE rec2 #-}

test x = do
    let (T _ r6 r7) = rec1 x
    (same $! r6) $! r7
{-# NOINLINE test #-}

main = test "foo"

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"
