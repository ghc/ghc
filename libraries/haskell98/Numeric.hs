{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Numeric (

        -- * Showing

        showSigned,       -- :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS

        showIntAtBase,    -- :: Integral a => a -> (a -> Char) -> a -> ShowS
        showInt,          -- :: Integral a => a -> ShowS
        showHex,          -- :: Integral a => a -> ShowS
        showOct,          -- :: Integral a => a -> ShowS

        showEFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
        showFFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
        showGFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
        showFloat,        -- :: (RealFloat a) => a -> ShowS

        floatToDigits,    -- :: (RealFloat a) => Integer -> a -> ([Int], Int)

        -- * Reading

        -- | /NB:/ 'readInt' is the \'dual\' of 'showIntAtBase',
        -- and 'readDec' is the \`dual\' of 'showInt'.
        -- The inconsistent naming is a historical accident.

        readSigned,       -- :: (Real a) => ReadS a -> ReadS a

        readInt,          -- :: (Integral a) => a -> (Char -> Bool)
                      --         -> (Char -> Int) -> ReadS a
        readDec,          -- :: (Integral a) => ReadS a
        readOct,          -- :: (Integral a) => ReadS a
        readHex,          -- :: (Integral a) => ReadS a

        readFloat,        -- :: (RealFloat a) => ReadS a

        lexDigits,        -- :: ReadS String

        -- * Miscellaneous

        fromRat,          -- :: (RealFloat a) => Rational -> a

    ) where

import "base" Numeric
