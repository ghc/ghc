{-# LANGUAGE MagicHash, BangPatterns, TypeOperators, GADTs #-}
import GHC.Exts
import Data.Type.Equality
import Unsafe.Coerce

foo :: Either Int a -> Maybe a
foo (Right x) = Just x
foo _ = Nothing
{-# NOINLINE foo #-}

bar :: a -> (Either Int a, Maybe a)
bar x = (Right x, Just x)
{-# NOINLINE bar #-}

data E a b = L a | R !b

foo' :: E Int a -> Maybe a
foo' (R x) = Just x
foo' _ = Nothing
{-# NOINLINE foo' #-}

baz :: [a] -> Maybe a
baz [] = Nothing
baz [a] = Just a
baz _ = Nothing
{-# NOINLINE baz #-}


nested :: Either Int (Either Int a) -> Either Bool (Maybe a)
nested (Right (Right x)) = Right (Just x)
nested _ = Left True
{-# NOINLINE nested #-}


-- CSE in a recursive group
data Tree x = T x (Either Int (Tree x)) (Maybe (Tree x))
rec1 :: x -> Tree x
rec1 x =
    let t = T x r1 r2
        r1 = Right t
        r2 = Just t
    in t
{-# NOINLINE rec1 #-}

-- Not yet supported! (and tricky)
data Stream a b x = S x (Stream b a x)
rec2 :: x -> Stream a b x
rec2 x =
    let s1 = S x s2
        s2 = S x s1
    in s1
{-# NOINLINE rec2 #-}


eq1 :: a :~: b -> [a]
eq1 Refl = []
{-# NOINLINE eq1 #-}



test x = do
    let (r1,r2) = bar x
    (same $! r1) $! r2                  -- yes
    let r3 = foo r1
    (same $! r1) $! r3                  -- yes
    let (r30, r31) = (R 'l', foo' r30)
    (same $! r30) $! r31                -- no, strictness

    let (r40, r41) = (['l'], baz r40)
    (same $! r40) $! r41                -- no, arity mismatch
    let (r42, r43) = ([], baz r42)
    (same $! r42) $! r43                -- no, WHY?
    let (r44, r45) = ("ab", baz r44)
    (same $! r44) $! r45                -- no, arity mismatch
    let (r46, r47) = (Refl, eq1 r46)
    (same $! r46) $! r47                -- no, WHY?

    let (r4,_) = bar r1
    let r5 = nested r4
    (same $! r4) $! r5                  -- yes
    let (T _ r6 r7) = rec1 x
    (same $! r6) $! r7                  -- yes
    let s1@(S _ s2) = rec2 x
    (same $! s1) $! s2                  -- no, not supported
    case r3 of
      Just b -> print ("YAY", b)
      Nothing -> print "BAD"
{-# NOINLINE test #-}

main = test "foo"

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"
{-# NOINLINE same #-}
