{-# LANGUAGE NoExperimentalDependentTypes #-}

module RetryCharacters (title, lower) where

import qualified Data.Char

-- | Use the ordinary title-case function during staged evaluation.
title :: Char -> Char
title = Data.Char.toTitle

-- | Use the ordinary lower-case function during staged evaluation.
lower :: Char -> Char
lower = Data.Char.toLower
