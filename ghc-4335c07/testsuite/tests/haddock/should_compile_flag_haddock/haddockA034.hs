{-# LANGUAGE GADTs #-}

module Hi where

-- | This is a GADT.
data Hi where
    -- | This is a GADT constructor.
    Hi :: () -> Hi
