
module M where
import Char
{-# INLINE f #-}
f = map ord . map chr
