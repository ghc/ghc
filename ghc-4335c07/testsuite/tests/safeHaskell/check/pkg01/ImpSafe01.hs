{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ImpSafe01 ( MyWord ) where

-- While Data.Word is safe it imports trustworthy
-- modules in base, hence base needs to be trusted.
-- Note: Worthwhile giving out better error messages for cases
-- like this if I can.
import Data.Word

type MyWord = Word

