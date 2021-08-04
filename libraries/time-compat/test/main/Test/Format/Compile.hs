-- Tests succeed if module compiles
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Format.Compile
    (
    ) where

-- Doesn't work with old time
-- 
-- import Data.Time.Compat
-- 
-- newtype WrappedUTCTime =
--     MkWrappedUTCTime UTCTime
--     deriving (FormatTime, ParseTime)
-- 
-- newtype Wrapped t =
--     MkWrapped t
--     deriving (FormatTime, ParseTime)
