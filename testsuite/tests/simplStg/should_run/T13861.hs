{-# LANGUAGE MagicHash, BangPatterns #-}
import GHC.Exts
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

test x = do
    let (r1,r2) = bar x
    (same $! r1) $! r2
    let r3 = foo r1
    (same $! r1) $! r3
    let (r30, r31) = (R 'l', foo' r30)
    (same $! r30) $! r31
    
    let (r40, r41) = (['l'], baz r40)
    (same $! r40) $! r41
    let (r4,_) = bar r1
    let r5 = nested r4
    (same $! r4) $! r5
    let (T _ r6 r7) = rec1 x
    (same $! r6) $! r7
    let s1@(S _ s2) = rec2 x
    (same $! s1) $! s2
    case r3 of
      Just b -> print ("YAY", b)
      Nothing -> print "BAD"
{-# NOINLINE test #-}

main = test "foo"

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"
