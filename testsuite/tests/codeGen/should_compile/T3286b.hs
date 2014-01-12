
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T3286b (LogFloat) where

newtype LogFloat = LogFloat Double
    deriving (Eq, Ord, Num, Show)

instance Fractional LogFloat where
    (/) (LogFloat x) (LogFloat y)
        |    x == 1
          && y == 1 = error "(/)"
        | otherwise                = LogFloat (x-y)
    fromRational = LogFloat . fromRational

