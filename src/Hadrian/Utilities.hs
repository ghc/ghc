module Hadrian.Utilities (

    -- * String manipulation
    quote,

    -- * FilePath manipulation
    unifyPath, (-/-)
    ) where

import Development.Shake.FilePath

-- | Add single quotes around a String.
quote :: String -> String
quote s = "'" ++ s ++ "'"

-- | Normalise a path and convert all path separators to @/@, even on Windows.
unifyPath :: FilePath -> FilePath
unifyPath = toStandard . normaliseEx

-- | Combine paths with a forward slash regardless of platform.
(-/-) :: FilePath -> FilePath -> FilePath
"" -/- b = b
a  -/- b
    | last a == '/' = a ++       b
    | otherwise     = a ++ '/' : b

infixr 6 -/-
