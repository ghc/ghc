{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype MaybeA a = MaybeA (Maybe a)
                 deriving (Show, Functor, Applicative)

main :: IO ()
main = print $ do
    x <- MaybeA $ Just 42
    pure x
