
-- Test for trac #322

module ShouldCompile where

instance (Num a) => Num (Maybe a) where
    (Just a) + (Just b) = Just (a + b)
    _ + _ = Nothing
    (Just a) - (Just b) = Just (a - b)
    _ - _ = Nothing
    (Just a) * (Just b) = Just (a * b)
    _ * _ = Nothing
    negate (Just a) = Just (negate a)
    negate _ = Nothing
    abs (Just a) = Just (abs a)
    abs _ = Nothing
    signum (Just a) = Just (signum a)
    signum _ = Nothing
    fromInteger = Just . fromInteger

f :: Maybe Int -> Int
f 1 = 1
f Nothing = 2 -- Gives bogus "Warning: Pattern match(es) are overlapped"
f _ = 3

