{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A large and somewhat miscellaneous collection of utility functions used
-- throughout the rest of the Cabal lib and in other tools that use the Cabal
-- lib like @cabal-install@. It has a very simple set of logging actions. It
-- has low level functions for running programs, a bunch of wrappers for
-- various directory and file functions that do extra logging.

module Distribution.Utils.Generic (
        -- * reading and writing files safely
        withFileContents,
        writeFileAtomic,

        -- * Unicode

        -- ** Conversions
        fromUTF8BS,
        fromUTF8LBS,

        toUTF8BS,
        toUTF8LBS,

        -- ** File I/O
        readUTF8File,
        withUTF8FileContents,
        writeUTF8File,

        -- ** BOM
        ignoreBOM,

        -- ** Misc
        normaliseLineEndings,

        -- * generic utils
        dropWhileEndLE,
        takeWhileEndLE,
        equating,
        comparing,
        isInfixOf,
        intercalate,
        lowercase,
        isAscii,
        isAsciiAlpha,
        isAsciiAlphaNum,
        listUnion,
        listUnionRight,
        ordNub,
        ordNubBy,
        ordNubRight,
        safeTail,
        unintersperse,
        wrapText,
        wrapLine,
        unfoldrM,
        spanMaybe,
        breakMaybe,

        -- * FilePath stuff
        isAbsoluteOnAnyPlatform,
        isRelativeOnAnyPlatform,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Utils.String

import Data.List
    ( isInfixOf )
import Data.Ord
    ( comparing )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set

import qualified Data.ByteString as SBS

import System.Directory
    ( removeFile, renameFile )
import System.FilePath
    ( (<.>), splitFileName )
import System.IO
    ( withFile, withBinaryFile
    , openBinaryTempFileWithDefaultPermissions
    , IOMode(ReadMode), hGetContents, hClose )
import qualified Control.Exception as Exception

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Wraps text to the default line width. Existing newlines are preserved.
wrapText :: String -> String
wrapText = unlines
         . map (intercalate "\n"
              . map unwords
              . wrapLine 79
              . words)
         . lines

-- | Wraps a list of words to a list of lines of words of a particular width.
wrapLine :: Int -> [String] -> [[String]]
wrapLine width = wrap 0 []
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

-----------------------------------
-- Safely reading and writing files

-- | Gets the contents of a file, but guarantee that it gets closed.
--
-- The file is read lazily but if it is not fully consumed by the action then
-- the remaining input is truncated and the file is closed.
--
withFileContents :: FilePath -> (String -> NoCallStackIO a) -> NoCallStackIO a
withFileContents name action =
  withFile name ReadMode
           (\hnd -> hGetContents hnd >>= action)

-- | Writes a file atomically.
--
-- The file is either written successfully or an IO exception is raised and
-- the original file is left unchanged.
--
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
writeFileAtomic :: FilePath -> BS.ByteString -> NoCallStackIO ()
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  Exception.bracketOnError
    (openBinaryTempFileWithDefaultPermissions targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        BS.hPut handle content
        hClose handle
        renameFile tmpPath targetPath)

-- ------------------------------------------------------------
-- * Unicode stuff
-- ------------------------------------------------------------

fromUTF8BS :: SBS.ByteString -> String
fromUTF8BS = decodeStringUtf8 . SBS.unpack

fromUTF8LBS :: BS.ByteString -> String
fromUTF8LBS = decodeStringUtf8 . BS.unpack

toUTF8BS :: String -> SBS.ByteString
toUTF8BS = SBS.pack . encodeStringUtf8

toUTF8LBS :: String -> BS.ByteString
toUTF8LBS = BS.pack . encodeStringUtf8

-- | Ignore a Unicode byte order mark (BOM) at the beginning of the input
--
ignoreBOM :: String -> String
ignoreBOM ('\xFEFF':string) = string
ignoreBOM string            = string

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Reads lazily using ordinary 'readFile'.
--
readUTF8File :: FilePath -> NoCallStackIO String
readUTF8File f = (ignoreBOM . fromUTF8LBS) <$> BS.readFile f

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Same behaviour as 'withFileContents'.
--
withUTF8FileContents :: FilePath -> (String -> IO a) -> IO a
withUTF8FileContents name action =
  withBinaryFile name ReadMode
    (\hnd -> BS.hGetContents hnd >>= action . ignoreBOM . fromUTF8LBS)

-- | Writes a Unicode String as a UTF8 encoded text file.
--
-- Uses 'writeFileAtomic', so provides the same guarantees.
--
writeUTF8File :: FilePath -> String -> NoCallStackIO ()
writeUTF8File path = writeFileAtomic path . BS.pack . encodeStringUtf8

-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old OS X
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

-- ------------------------------------------------------------
-- * Common utils
-- ------------------------------------------------------------

-- | @dropWhileEndLE p@ is equivalent to @reverse . dropWhile p . reverse@, but
-- quite a bit faster. The difference between "Data.List.dropWhileEnd" and this
-- version is that the one in "Data.List" is strict in elements, but spine-lazy,
-- while this one is spine-strict but lazy in elements. That's what @LE@ stands
-- for - "lazy in elements".
--
-- Example:
--
-- >>> tail $ Data.List.dropWhileEnd (<3) [undefined, 5, 4, 3, 2, 1]
-- *** Exception: Prelude.undefined
-- ...
--
-- >>> tail $ dropWhileEndLE (<3) [undefined, 5, 4, 3, 2, 1]
-- [5,4,3]
--
-- >>> take 3 $ Data.List.dropWhileEnd (<3) [5, 4, 3, 2, 1, undefined]
-- [5,4,3]
--
-- >>> take 3 $ dropWhileEndLE (<3) [5, 4, 3, 2, 1, undefined]
-- *** Exception: Prelude.undefined
-- ...
--
dropWhileEndLE :: (a -> Bool) -> [a] -> [a]
dropWhileEndLE p = foldr (\x r -> if null r && p x then [] else x:r) []

-- | @takeWhileEndLE p@ is equivalent to @reverse . takeWhile p . reverse@, but
-- is usually faster (as well as being easier to read).
takeWhileEndLE :: (a -> Bool) -> [a] -> [a]
takeWhileEndLE p = fst . foldr go ([], False)
  where
    go x (rest, done)
      | not done && p x = (x:rest, False)
      | otherwise = (rest, True)

-- | Like 'Data.List.nub', but has @O(n log n)@ complexity instead of
-- @O(n^2)@. Code for 'ordNub' and 'listUnion' taken from Niklas Hambüchen's
-- <http://github.com/nh2/haskell-ordnub ordnub> package.
ordNub :: Ord a => [a] -> [a]
ordNub = ordNubBy id

-- | Like 'ordNub' and 'Data.List.nubBy'. Selects a key for each element and
-- takes the nub based on that key.
ordNubBy :: Ord b => (a -> b) -> [a] -> [a]
ordNubBy f l = go Set.empty l
  where
    go !_ [] = []
    go !s (x:xs)
      | y `Set.member` s = go s xs
      | otherwise        = let !s' = Set.insert y s
                            in x : go s' xs
      where
        y = f x

-- | Like "Data.List.union", but has @O(n log n)@ complexity instead of
-- @O(n^2)@.
listUnion :: (Ord a) => [a] -> [a] -> [a]
listUnion a b = a ++ ordNub (filter (`Set.notMember` aSet) b)
  where
    aSet = Set.fromList a

-- | A right-biased version of 'ordNub'.
--
-- Example:
--
-- >>> ordNub [1,2,1] :: [Int]
-- [1,2]
--
-- >>> ordNubRight [1,2,1] :: [Int]
-- [2,1]
--
ordNubRight :: (Ord a) => [a] -> [a]
ordNubRight = fst . foldr go ([], Set.empty)
  where
    go x p@(l, s) = if x `Set.member` s then p
                                        else (x:l, Set.insert x s)

-- | A right-biased version of 'listUnion'.
--
-- Example:
--
-- >>> listUnion [1,2,3,4,3] [2,1,1]
-- [1,2,3,4,3]
--
-- >>> listUnionRight [1,2,3,4,3] [2,1,1]
-- [4,3,2,1,1]
--
listUnionRight :: (Ord a) => [a] -> [a] -> [a]
listUnionRight a b = ordNubRight (filter (`Set.notMember` bSet) a) ++ b
  where
    bSet = Set.fromList b

-- | A total variant of 'tail'.
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

-- | Lower case string
--
-- >>> lowercase "Foobar"
-- "foobar"
lowercase :: String -> String
lowercase = map toLower

-- | Ascii characters
isAscii :: Char -> Bool
isAscii c = fromEnum c < 0x80

-- | Ascii letters.
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = ('a' <= c && c <= 'z')
    || ('A' <= c && c <= 'Z')

-- | Ascii letters and digits.
--
-- >>> isAsciiAlphaNum 'a'
-- True
--
-- >>> isAsciiAlphaNum 'ä'
-- False
--
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c ||  isDigit c

unintersperse :: Char -> String -> [String]
unintersperse mark = unfoldr unintersperse1 where
  unintersperse1 str
    | null str = Nothing
    | otherwise =
        let (this, rest) = break (== mark) str in
        Just (this, safeTail rest)

-- | Like 'break', but with 'Maybe' predicate
--
-- >>> breakMaybe (readMaybe :: String -> Maybe Int) ["foo", "bar", "1", "2", "quu"]
-- (["foo","bar"],Just (1,["2","quu"]))
--
-- >>> breakMaybe (readMaybe :: String -> Maybe Int) ["foo", "bar"]
-- (["foo","bar"],Nothing)
--
-- @since 2.2
--
breakMaybe :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakMaybe f = go id where
    go !acc []     = (acc [], Nothing)
    go !acc (x:xs) = case f x of
        Nothing -> go (acc . (x:)) xs
        Just b  -> (acc [], Just (b, xs))

-- | Like 'span' but with 'Maybe' predicate
--
-- >>> spanMaybe listToMaybe [[1,2],[3],[],[4,5],[6,7]]
-- ([1,3],[[],[4,5],[6,7]])
--
-- >>> spanMaybe (readMaybe :: String -> Maybe Int) ["1", "2", "foo"]
-- ([1,2],["foo"])
--
-- @since 2.2
--
spanMaybe :: (a -> Maybe b) -> [a] -> ([b],[a])
spanMaybe _ xs@[] =  ([], xs)
spanMaybe p xs@(x:xs') = case p x of
    Just y  -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
    Nothing -> ([], xs)

-- | 'unfoldr' with monadic action.
--
-- >>> take 5 $ unfoldrM (\b r -> Just (r + b, b + 1)) (1 :: Int) 2
-- [3,4,5,6,7]
--
-- @since 2.2
--
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f = go where
    go b = do
        m <- f b
        case m of
            Nothing      -> return []
            Just (a, b') -> liftM (a :) (go b')

-- ------------------------------------------------------------
-- * FilePath stuff
-- ------------------------------------------------------------

-- | 'isAbsoluteOnAnyPlatform' and 'isRelativeOnAnyPlatform' are like
-- 'System.FilePath.isAbsolute' and 'System.FilePath.isRelative' but have
-- platform independent heuristics.
-- The System.FilePath exists in two versions, Windows and Posix. The two
-- versions don't agree on what is a relative path and we don't know if we're
-- given Windows or Posix paths.
-- This results in false positives when running on Posix and inspecting
-- Windows paths, like the hackage server does.
-- System.FilePath.Posix.isAbsolute \"C:\\hello\" == False
-- System.FilePath.Windows.isAbsolute \"/hello\" == False
-- This means that we would treat paths that start with \"/\" to be absolute.
-- On Posix they are indeed absolute, while on Windows they are not.
--
-- The portable versions should be used when we might deal with paths that
-- are from another OS than the host OS. For example, the Hackage Server
-- deals with both Windows and Posix paths while performing the
-- PackageDescription checks. In contrast, when we run 'cabal configure' we
-- do expect the paths to be correct for our OS and we should not have to use
-- the platform independent heuristics.
isAbsoluteOnAnyPlatform :: FilePath -> Bool
-- C:\\directory
isAbsoluteOnAnyPlatform (drive:':':'\\':_) = isAlpha drive
-- UNC
isAbsoluteOnAnyPlatform ('\\':'\\':_) = True
-- Posix root
isAbsoluteOnAnyPlatform ('/':_) = True
isAbsoluteOnAnyPlatform _ = False

-- | @isRelativeOnAnyPlatform = not . 'isAbsoluteOnAnyPlatform'@
isRelativeOnAnyPlatform :: FilePath -> Bool
isRelativeOnAnyPlatform = not . isAbsoluteOnAnyPlatform

-- $setup
-- >>> import Data.Maybe
-- >>> import Text.Read
