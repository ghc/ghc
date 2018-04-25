
module M where
import Data.Char
{-# INLINE f #-}
f = map ord . map chr
