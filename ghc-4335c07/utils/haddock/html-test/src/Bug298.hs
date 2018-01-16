-- We introduced a regression in 2.14.x where we don't consider
-- identifiers with ^ as valid. We test that the regression goes away
-- here. It's a silly typo in the parser, really. Same with ★ which is a valid
-- symbol according to the 2010 report.
module Bug298 where


(<^>) :: (a -> a) -> a -> a
x <^> y = x y

(<^) :: a -> a -> a
x <^ y = x

(^>) :: a -> a -> a
x ^> y = y

(⋆^) :: a -> a -> a
x ⋆^ y = y

-- | Links to '<^>' and '^>', '<^' and '⋆^'.
f :: ()
f = ()
