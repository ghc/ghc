{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fforce-recomp -Wincomplete-patterns #-}

incomplete :: Maybe a -> Bool
incomplete ma = case (ma, ()) of
  (Nothing, _) -> False

{-# COMPLETE Pat #-}
pattern Pat :: a -> b -> (a, b)
pattern Pat a b = (a, b)

main :: IO ()
main = print $ incomplete (Just ())
