{-# LANGUAGE TemplateHaskell #-}
module T10796b where

import Data.Set (Set, fromList)
import Language.Haskell.TH.Quote (dataToPatQ)

badPattern :: Set Char -> Set Char
badPattern s@($(dataToPatQ (const Nothing) (fromList "test"))) = s
