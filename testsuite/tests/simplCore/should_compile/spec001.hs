{-# LANGUAGE CPP, UnboxedTuples, MagicHash, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -O #-}
{-# OPTIONS_GHC -fno-warn-amp #-}

-- In GHC 6.4, compiling this module gave a Core Lint failure following the
-- specialier, because a function was floated out that had a RULE that
-- mentioned another fuction (unpack, in fact).  but the latter wasn't
-- floated because we didn't take the RULES into account properly; result,
-- variable out of scope.

-- It's hard to cut this test down.


module Data.PackedString.Latin1 (
        -- * The @PackedString@ type
        PackedString,      -- abstract, instances: Eq, Ord, Show, Typeable

         -- * Converting to and from @PackedString@s
        pack,
        unpack,

        -- * I\/O with @PackedString@s
        hPut, hGet,

        -- * List-like manipulation functions
        nil,
        cons,
        head,
        tail,
        null,
        append,
        length,
        index,
        map,
        filter,
        reverse,
        concat,
        elem,
        substr,
        take,
        drop,
        splitAt,
        foldl,
        foldr,
        takeWhile,
        dropWhile,
        span,
        break,
        lines,
        unlines,
        words,
        unwords,
        split,
        splitWith,
        join,
--      unpackList, -- eek, otherwise it gets thrown away by the simplifier

    ) where

import qualified Prelude
import Prelude hiding (
        head,
        tail,
        null,
        length,
        (!!),
        map,
        filter,
        reverse,
        concat,
        elem,
        take,
        drop,
        foldl,
        foldr,
        splitAt,
        takeWhile,
        dropWhile,
        span,
        break,
        lines,
        unlines,
        words,
        unwords,
        join
 )

import GHC.Exts
import GHC.IO (IO(..))
import Foreign
import Data.Typeable
import Data.Char
import qualified Data.List
import System.IO

-- -----------------------------------------------------------------------------
-- PackedString type declaration

-- | A space-efficient representation of a 'String', which supports
-- various efficient operations.  A 'PackedString' contains Latin1
-- (8-bit) characters only.
data PackedString = PS {-#UNPACK#-}!Int {-#UNPACK#-}!Int
                       {-#UNPACK#-}!(ForeignPtr Word8)
        -- this is a pretty efficient representation, and can be
        -- converted to/from a StorableArray.
        -- When the ForeignPtr is unpacked, we get the Addr# stored
        -- directly in the PS constructor.

-- Perhaps making a slice should be conditional on the ratio of the
-- slice/string size to limit memory leaks.

instance Eq PackedString where
   a == b =  comparePS a b == EQ

instance Ord PackedString where
   compare = comparePS

comparePS (PS off1 len1 fp1) (PS off2 len2 fp2)
  = inlinePerformIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
        cmp (p1 `plusPtr` off1) (p2 `plusPtr` off2) len1
  where
    cmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Ordering
    cmp p1 p2 n
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          a <- peekElemOff p1 n
          b <- peekElemOff p2 n
          case a `compare` b of
                EQ -> cmp p1 p2 (n+1)
                LT -> return LT
                GT -> return GT

--instance Read PackedString: ToDo

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpack ps) r

deriving instance Typeable PackedString

-- -----------------------------------------------------------------------------
-- Constructor functions

-- | The 'nilPS' value is the empty string.
nil :: PackedString
nil = inlinePerformIO $ do
                fp <- newForeignPtr_ nullPtr
                return (PS 0 0 fp)

-- | The 'consPS' function prepends the given character to the
-- given string.
cons :: Char -> PackedString -> PackedString
cons c cs = pack (c : (unpack cs)) -- ToDo:better

-- | Convert a 'String' into a 'PackedString'
packLen :: Int -> String -> PackedString
packLen len str = inlinePerformIO $ do
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp $ \p -> do
        fill_it_in p 0 str
        return (PS 0 len fp)

fill_it_in p i [] = return ()
fill_it_in p i (c:cs) = do pokeElemOff p i (c2w c); fill_it_in p (i+1) cs

pack :: String -> PackedString
pack str = packLen (Prelude.length str) str

{-# INLINE w2c #-}
w2c :: Word8 -> Char
w2c = chr . fromIntegral
{-# INLINE c2w #-}
c2w :: Char -> Word8
c2w = fromIntegral . ord

-- -----------------------------------------------------------------------------
-- List-mimicking functions for PackedStrings

-- | The 'length' function returns the length of the input list.
-- Analogous to 'length'.
length :: PackedString -> Int
length (PS _ len _) = len

-- | The 'index' function returns the character in the string at the
-- given position.
index :: PackedString -> Int -> Char
index ps i
  | i >= 0 && i < len = unsafeIndex ps i
  | otherwise = error "Data.PackedString.Latin1.index: index out of range"
  where len = length ps

unsafeIndex :: PackedString -> Int -> Char
unsafeIndex (PS off len fp) i =
  withPackedString fp $ \p -> do
    w <- peekElemOff (p `plusPtr` off) i
    return $! w2c w

-- | The 'head' function returns the first element of a
-- 'PackedString' or throws an error if the string is empty.
head :: PackedString -> Char
head ps
  | len <= 0 = error "Data.PackedString.Latin1.head: head []"
  | otherwise = index ps 0
  where len = length ps

-- | The 'tail' function returns the tail of a 'PackedString' or throws an error
-- if the string is empty.
tail :: PackedString -> PackedString
tail ps
  | len <= 0 = error "Data.PackedString.Latin1.tail: tail []"
  | len == 1 = nil
  | otherwise  = substr ps 1 (len - 1)
  where len = length ps

-- | The 'null' function returns True iff the argument is null.
null :: PackedString -> Bool
null (PS _ l _) = l == 0

-- | The 'append' function appends the second string onto the first.
append :: PackedString -> PackedString -> PackedString
append xs ys
  | null xs = ys
  | null ys = xs
  | otherwise  = concat [xs,ys]

-- | The 'map' function applies a function to each character in the string.
map :: (Char -> Char) -> PackedString -> PackedString
map f ps = packLen (length ps) (Prelude.map f (unpack ps))

-- | The 'filter' function filters out the appropriate substring.
filter :: (Char -> Bool) -> PackedString -> PackedString {-or String?-}
filter pred ps = pack $ Prelude.filter pred $ unpack ps

-- | The 'foldl' function behaves like 'foldl' on 'PackedString's.
foldl :: (a -> Char -> a) -> a -> PackedString -> a
foldl f b ps = Prelude.foldl f b $ unpack ps

-- | The 'foldr' function behaves like 'foldr' on 'PackedString's.
foldr :: (Char -> a -> a) -> a -> PackedString -> a
foldr f v ps = Prelude.foldr f v $ unpack ps -- no intermediate list, we hope

-- | The 'take' function takes the first @n@ characters of a 'PackedString'.
take :: Int -> PackedString -> PackedString
take n ps = substr ps 0 (n-1)

-- | The 'drop' function drops the first @n@ characters of a 'PackedString'.
drop    :: Int -> PackedString -> PackedString
drop n ps = substr ps n (length ps - 1)

-- | The 'splitWith' function splits a 'PackedString' at a given index.
splitAt :: Int -> PackedString -> (PackedString, PackedString)
splitAt  n ps  = (take n ps, drop n ps)

-- | The 'takeWhile' function is analogous to the 'takeWhile' function.
takeWhile :: (Char -> Bool) -> PackedString -> PackedString
takeWhile pred ps = pack $ Prelude.takeWhile pred $ unpack ps

-- | The 'dropWhile' function is analogous to the 'dropWhile' function.
dropWhile :: (Char -> Bool) -> PackedString -> PackedString
dropWhile pred ps = pack $ Prelude.dropWhile pred $ unpack ps

-- | The 'elem' function returns True iff the given element is in the string.
elem :: Char -> PackedString -> Bool
elem c ps = c `Prelude.elem` unpack ps

-- | The 'span' function returns a pair containing the result of
-- running both 'takeWhile' and 'dropWhile'.
span :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
span  p ps = (takeWhile p ps, dropWhile p ps)

-- | The 'break' function breaks a string at the first position which
-- satisfies the predicate.
break :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
break p ps = span (not . p) ps

-- | The 'lines' function splits the input on line-breaks.
lines :: PackedString -> [PackedString]
lines ps = split '\n' ps

-- | The 'unlines' function concatenates the input list after
-- interspersing newlines.
unlines :: [PackedString] -> PackedString
unlines pss = join (pack "\n") pss

-- | The 'words' function is analogous to the 'words' function.
words :: PackedString -> [PackedString]
words ps = Prelude.filter (not.null) (splitWith isSpace ps)

-- | The 'unwords' function is analogous to the 'unwords' function.
unwords :: [PackedString] -> PackedString
unwords pss = join (pack " ") pss

-- | The 'reverse' function reverses the string.
reverse :: PackedString -> PackedString
reverse ps = pack $ Prelude.reverse $ unpack ps

-- | The 'concat' function concatenates a list of 'PackedString's.
concat :: [PackedString] -> PackedString
concat pss = pack $ Prelude.concat $ Prelude.map unpack pss

------------------------------------------------------------

-- | The 'join' function takes a 'PackedString' and a list of 'PackedString's
-- and concatenates the list after interspersing the first argument between
-- each element of the list.
join :: PackedString -> [PackedString] -> PackedString
join filler pss = concat (splice pss)
 where
  splice []  = []
  splice [x] = [x]
  splice (x:y:xs) = x:filler:splice (y:xs)

-- ToDo: the obvious generalisation
{-
  Some properties that hold:

  * split x ls = ls'
      where False = any (map (x `elem`) ls')

  * join (pack [x]) (split x ls) = ls
-}

-- | The 'split' function splits the input string on each occurrence of the given 'Char'.
split :: Char -> PackedString -> [PackedString]
split c = splitWith (== c)

splitWith :: (Char -> Bool) -> PackedString -> [PackedString]
splitWith pred (PS off 0 fp) = []
splitWith pred (PS off len fp) = splitWith' pred off len fp

splitWith' pred off len fp =
  withPackedString fp $ \p -> splitLoop pred p 0 off len fp

splitLoop pred p idx off len fp
        | p `seq` idx `seq` off `seq` fp `seq` False = undefined
splitLoop pred p idx off len fp
        | idx >= len  = return [PS off idx fp]
        | otherwise = do
                w <- peekElemOff p (off+idx)
                if pred (w2c w)
                   then return (PS off idx fp :
                                  splitWith' pred (off+idx+1) (len-idx-1) fp)
                   else splitLoop pred p (idx+1) off len fp

-- -----------------------------------------------------------------------------
-- Local utility functions

-- The definition of @_substr@ is essentially:
-- @take (end - begin + 1) (drop begin str)@.

-- | The 'substr' function takes a 'PackedString' and two indices
-- and returns the substring of the input string between (and including)
-- these indices.
substr :: PackedString -> Int -> Int -> PackedString
substr (PS off len fp) begin end = PS (off+begin) (end-begin+1) fp

-- -----------------------------------------------------------------------------
-- hPut

-- | Outputs a 'PackedString' to the specified 'Handle'.
--
-- NOTE: the string will be output directly in Latin-1.
--
hPut :: Handle -> PackedString -> IO ()
hPut h (PS off l fp) =
  withForeignPtr fp $ \p ->
    hPutBuf h (p `plusPtr` off) l

-- -----------------------------------------------------------------------------
-- hGet

-- | Read a 'PackedString' directly from the specified 'Handle'.
-- This is far more efficient than reading the characters into a 'String'
-- and then using 'pack'.
--
-- NOTE: as with 'hPut', the string representation in the file is
-- assumed to be Latin-1.
hGet :: Handle -> Int -> IO PackedString
hGet h i = do
  fp <- mallocForeignPtrBytes i
  withForeignPtr fp $ \p -> do
    l <- hGetBuf h p i
    return (PS 0 l fp)

-- -----------------------------------------------------------------------------
-- unpacking

{-# INLINE unpack #-}
unpack :: PackedString -> String
unpack ps = build (unpackFoldr ps)

{-# RULES
"unpack-list"  [1]  forall p  . unpackFoldr p (:) [] = unpackList p
 #-}

unpackList :: PackedString -> [Char]
unpackList (PS off len fp) =
   withPackedString fp $ \p -> do
      let loop p (-1) acc = return acc
          loop p n acc = do
             a <- peekElemOff p n
             loop p (n-1) (w2c a : acc)
      loop (p `plusPtr` off) (len-1) []

{-# INLINE [0] unpackFoldr #-}
unpackFoldr :: PackedString -> (Char -> a -> a) -> a -> a
unpackFoldr (PS off len fp) f c =
   withPackedString fp $ \p -> do
      let loop p (-1) acc = return acc
          loop p n acc = do
             a <- peekElemOff p n
             loop p (n-1) (w2c a `f` acc)
      loop (p `plusPtr` off) (len-1) c

-- -----------------------------------------------------------------------------
-- Utils

-- Just like unsafePerformIO, but we inline it.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

withPackedString :: ForeignPtr a -> (Ptr a -> IO b) -> b
withPackedString fp io = inlinePerformIO (withForeignPtr fp io)
