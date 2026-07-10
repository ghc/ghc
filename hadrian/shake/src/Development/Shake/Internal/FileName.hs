{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Development.Shake.Internal.FileName(
    FileName,
    fileNameFromString, fileNameFromByteString,
    fileNameToString, fileNameToByteString,
    filepathNormalise
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import Development.Shake.Classes
import qualified System.FilePath as Native
import General.Binary
import System.Info.Extra
import Data.List


---------------------------------------------------------------------
-- FileName newtype

-- | UTF8 ByteString
newtype FileName = FileName BS.ByteString
    deriving (Hashable, Binary, BinaryEx, Eq, NFData)

instance Show FileName where
    show = fileNameToString

instance BinaryEx [FileName] where
    putEx = putEx . map (\(FileName x) -> x)
    getEx = map FileName . getEx

fileNameToString :: FileName -> FilePath
fileNameToString = UTF8.toString . fileNameToByteString

fileNameToByteString :: FileName -> BS.ByteString
fileNameToByteString (FileName x) = x

fileNameFromString :: FilePath -> FileName
fileNameFromString = fileNameFromByteString . UTF8.fromString

fileNameFromByteString :: BS.ByteString -> FileName
fileNameFromByteString = FileName . filepathNormalise


---------------------------------------------------------------------
-- NORMALISATION

-- | Equivalent to @toStandard . normaliseEx@ from "Development.Shake.FilePath".
filepathNormalise :: BS.ByteString -> BS.ByteString
filepathNormalise xs
    | isWindows, Just (a,xs) <- BS.uncons xs, sep a, Just (b,_) <- BS.uncons xs, sep b = '/' `BS.cons` f xs
    | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f o = deslash o $ BS.concat $ (slash:) $ intersperse slash $ reverse $ (BS.empty:) $ g 0 $ reverse $ split o

        deslash o x
            | x == slash = case (pre,pos) of
                (True,True) -> slash
                (True,False) -> BS.pack "/."
                (False,True) -> BS.pack "./"
                (False,False) -> dot
            | otherwise = (if pre then id else BS.tail) $ (if pos then id else BS.init) x
            where pre = not (BS.null o) && sep (BS.head o)
                  pos = not (BS.null o) && sep (BS.last o)

        g i [] = replicate i dotDot
        g i (x:xs) | BS.null x = g i xs
        g i (x:xs) | x == dotDot = g (i+1) xs
        g i (x:xs) | x == dot = g i xs
        g 0 (x:xs) = x : g 0 xs
        g i (_:xs) = g (i-1) xs -- equivalent to eliminating ../x

        split = BS.splitWith sep

dotDot = BS.pack ".."
dot = BS.singleton '.'
slash = BS.singleton '/'
