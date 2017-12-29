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


data Boo = Tru | Fal | Dunno

quux True = Fal
quux False = Tru
{-# NOINLINE quux #-}

quux' Fal = True
quux' _ = False
{-# NOINLINE quux' #-}

-- the 'Dunno' and default case (i.e. 'Tru') should be lumped together
lump Fal = True
lump Dunno = unsafeCoerce Tru
lump _ = False
{-# NOINLINE lump #-}

-- the 'One' and default case should be lumped together
data Boom = Zero | One | Two | Three
lump' One = Fal
lump' Three = Tru
lump' other = unsafeCoerce other -- Zero -> Tru, Two -> Dunno
{-# NOINLINE lump' #-}


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

eq2 :: a :~: b -> b :~: a
eq2 Refl = Refl
{-# NOINLINE eq2 #-}


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
    (same $! r42) $! r43                -- yes
    let (r44, r45) = ("ab", baz r44)
    (same $! r44) $! r45                -- no, arity mismatch
    let (r46, r47) = (Refl, eq1 r46)
    (same $! r46) $! r47                -- no, GADT
    let (r48, r49) = (Refl, eq2 r48)
    (same $! r48) $! r49                -- no, GADT
    let (r50, r51) = (True, quux r50)
    (same $! r50) $! r51                -- yes, quux is STG identity
    let (r52, r53) = (Tru, quux' r52)
    (same $! r52) $! r53                -- no, quux' is not STG identity on 'Tru'
    let (r54, r55) = (Fal, quux' r54)
    (same $! r54) $! r55                -- yes, quux' is STG identity on 'Fal'
    let (r56, r57) = (Tru, lump r56)
    (same $! r56) $! r57                -- yes, lump is STG identity on 'Tru'
    let (r58, r59) = (Fal, lump r58)
    (same $! r58) $! r59                -- yes, lump is STG identity on 'Fal'
    let (r60, r61) = (Two, lump' r60)
    (same $! r60) $! r61                -- yes, lump' is STG identity on 'Two'

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
