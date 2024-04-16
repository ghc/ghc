{-# LANGUAGE ImpredicativeTypes #-}

module T24676 where

ids :: [forall a. a -> a]
ids = take 5 (repeat id)
-- take :: Int -> [a] -> [a]
-- repeat :: b -> [b]

test :: [forall a. a->a]
test = ids ++ ids

-- typechecks with signature, without signature, and inlining "test"
f1a = test

f1b :: [forall a. a -> a]
f1b = test

-- typechecks with or without signature
f2a = Just test

f2b :: Maybe [forall a. a->a]
f2b = Just test

-- only typechecks with a signature

f3a = Just (ids ++ ids)

f3b :: Maybe [forall a. a -> a]
f3b = Just (ids ++ ids)
