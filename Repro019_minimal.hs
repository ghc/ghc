module Repro019_minimal where

unpack :: Int -> [Int]
unpack b = unpackGo b
{-# NOINLINE unpack #-}

unpackGo :: Int -> [Int]
unpackGo len = go len
  where go i = if i == 0 then [] else i : go (i - 1)
{-# INLINE unpackGo #-}

{-# RULES "unpack/late" [0] forall b. unpack b = unpackGo b #-}

conv :: [Int] -> Int
conv input = case unpack (length input) of
  [] -> undefined
  _  -> 0

opaque :: Int -> Bool
opaque x = x > 0
{-# NOINLINE opaque #-}

getUnchecked :: Int -> IO (Int, Maybe Int)
getUnchecked x = get x >>= maybe undefined pure

get :: Int -> IO (Maybe (Int, Maybe Int))
get x = do
  a <- ((if opaque x then pure Nothing else pure (Just 0)) >>= \r -> case r of
          Just _ -> pure (Just (1 :: Int))
          _      -> pure Nothing)
  b <- (if opaque x then pure Nothing else pure (Just 0))
  pure ((,) <$> a <*> Just (fmap (conv . unpack) b))
{-# INLINE get #-}

top :: IO ()
top = () <$ getUnchecked (undefined :: Int)

