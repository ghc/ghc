{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, MagicHash, CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE TypeFamilies #-}
#endif

-- |
-- Module      : Data.Text.Lazy
-- Copyright   : (c) 2009, 2010, 2012 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text using
-- lists of packed arrays.
--
-- /Note/: Read below the synopsis for important notes on the use of
-- this module.
--
-- The representation used by this module is suitable for high
-- performance use and for streaming large quantities of data.  It
-- provides a means to manipulate a large body of text without
-- requiring that the entire content be resident in memory.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons',
-- have better time complexity than their "Data.Text" equivalents, due
-- to the underlying representation being a list of chunks. For other
-- operations, lazy 'Text's are usually within a few percent of strict
-- ones, but often with better heap usage if used in a streaming
-- fashion. For data larger than available memory, or if you have
-- tight memory constraints, this module will be the only option.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Text.Lazy as L

module Data.Text.Lazy
    (
    -- * Fusion
    -- $fusion

    -- * Acceptable data
    -- $replacement

    -- * Types
      Text

    -- * Creation and elimination
    , pack
    , unpack
    , singleton
    , empty
    , fromChunks
    , toChunks
    , toStrict
    , fromStrict
    , foldrChunks
    , foldlChunks

    -- * Basic interface
    , cons
    , snoc
    , append
    , uncons
    , unsnoc
    , head
    , last
    , tail
    , init
    , null
    , length
    , compareLength

    -- * Transformations
    , map
    , intercalate
    , intersperse
    , transpose
    , reverse
    , replace

    -- ** Case conversion
    -- $case
    , toCaseFold
    , toLower
    , toUpper
    , toTitle

    -- ** Justification
    , justifyLeft
    , justifyRight
    , center

    -- * Folds
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

    -- ** Special folds
    , concat
    , concatMap
    , any
    , all
    , maximum
    , minimum

    -- * Construction

    -- ** Scans
    , scanl
    , scanl1
    , scanr
    , scanr1

    -- ** Accumulating maps
    , mapAccumL
    , mapAccumR

    -- ** Generation and unfolding
    , repeat
    , replicate
    , cycle
    , iterate
    , unfoldr
    , unfoldrN

    -- * Substrings

    -- ** Breaking strings
    , take
    , takeEnd
    , drop
    , dropEnd
    , takeWhile
    , takeWhileEnd
    , dropWhile
    , dropWhileEnd
    , dropAround
    , strip
    , stripStart
    , stripEnd
    , splitAt
    , span
    , breakOn
    , breakOnEnd
    , break
    , group
    , groupBy
    , inits
    , tails

    -- ** Breaking into many substrings
    -- $split
    , splitOn
    , split
    , chunksOf
    -- , breakSubstring

    -- ** Breaking into lines and words
    , lines
    , words
    , unlines
    , unwords

    -- * Predicates
    , isPrefixOf
    , isSuffixOf
    , isInfixOf

    -- ** View patterns
    , stripPrefix
    , stripSuffix
    , commonPrefixes

    -- * Searching
    , filter
    , find
    , breakOnAll
    , partition

    -- , findSubstring

    -- * Indexing
    , index
    , count

    -- * Zipping and unzipping
    , zip
    , zipWith

    -- -* Ordered text
    -- , sort
    ) where

import Prelude (Char, Bool(..), Maybe(..), String,
                Eq(..), Ord(..), Ordering(..), Read(..), Show(..),
                (&&), (||), (+), (-), (.), ($), (++),
                error, flip, fmap, fromIntegral, not, otherwise, quot)
import qualified Prelude as P
#if defined(HAVE_DEEPSEQ)
import Control.DeepSeq (NFData(..))
#endif
import Data.Int (Int64)
import qualified Data.List as L
import Data.Char (isSpace)
import Data.Data (Data(gfoldl, toConstr, gunfold, dataTypeOf), constrIndex,
                  Constr, mkConstr, DataType, mkDataType, Fixity(Prefix))
import Data.Binary (Binary(get, put))
import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Internal.Fusion.Common as S
import qualified Data.Text.Unsafe as T
import qualified Data.Text.Internal.Lazy.Fusion as S
import Data.Text.Internal.Fusion.Types (PairS(..))
import Data.Text.Internal.Lazy.Fusion (stream, unstream)
import Data.Text.Internal.Lazy (Text(..), chunk, empty, foldlChunks,
                                foldrChunks, smallChunkSize)
import Data.Text.Internal (firstf, safe, text)
import Data.Text.Lazy.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Text.Internal.Functions as F
import Data.Text.Internal.Lazy.Search (indices)
#if __GLASGOW_HASKELL__ >= 702
import qualified GHC.CString as GHC
#else
import qualified GHC.Base as GHC
#endif
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as Exts
#endif
import GHC.Prim (Addr#)
#if MIN_VERSION_base(4,7,0)
import Text.Printf (PrintfArg, formatArg, formatString)
#endif

-- $fusion
--
-- Most of the functions in this module are subject to /fusion/,
-- meaning that a pipeline of such functions will usually allocate at
-- most one 'Text' value.
--
-- As an example, consider the following pipeline:
--
-- > import Data.Text.Lazy as T
-- > import Data.Text.Lazy.Encoding as E
-- > import Data.ByteString.Lazy (ByteString)
-- >
-- > countChars :: ByteString -> Int
-- > countChars = T.length . T.toUpper . E.decodeUtf8
--
-- From the type signatures involved, this looks like it should
-- allocate one 'ByteString' value, and two 'Text' values. However,
-- when a module is compiled with optimisation enabled under GHC, the
-- two intermediate 'Text' values will be optimised away, and the
-- function will be compiled down to a single loop over the source
-- 'ByteString'.
--
-- Functions that can be fused by the compiler are documented with the
-- phrase \"Subject to fusion\".

-- $replacement
--
-- A 'Text' value is a sequence of Unicode scalar values, as defined
-- in &#xa7;3.9, definition D76 of the Unicode 5.2 standard:
-- <http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35>. As
-- such, a 'Text' cannot contain values in the range U+D800 to U+DFFF
-- inclusive. Haskell implementations admit all Unicode code points
-- (&#xa7;3.4, definition D10) as 'Char' values, including code points
-- from this invalid range.  This means that there are some 'Char'
-- values that are not valid Unicode scalar values, and the functions
-- in this module must handle those cases.
--
-- Within this module, many functions construct a 'Text' from one or
-- more 'Char' values. Those functions will substitute 'Char' values
-- that are not valid Unicode scalar values with the replacement
-- character \"&#xfffd;\" (U+FFFD).  Functions that perform this
-- inspection and replacement are documented with the phrase
-- \"Performs replacement on invalid scalar values\".
--
-- (One reason for this policy of replacement is that internally, a
-- 'Text' value is represented as packed UTF-16 data. Values in the
-- range U+D800 through U+DFFF are used by UTF-16 to denote surrogate
-- code points, and so cannot be represented. The functions replace
-- invalid scalar values, instead of dropping them, as a security
-- measure. For details, see Unicode Technical Report 36, &#xa7;3.5:
-- <http://unicode.org/reports/tr36#Deletion_of_Noncharacters>)

equal :: Text -> Text -> Bool
equal Empty Empty = True
equal Empty _     = False
equal _ Empty     = False
equal (Chunk a as) (Chunk b bs) =
    case compare lenA lenB of
      LT -> a == (T.takeWord16 lenA b) &&
            as `equal` Chunk (T.dropWord16 lenA b) bs
      EQ -> a == b && as `equal` bs
      GT -> T.takeWord16 lenB a == b &&
            Chunk (T.dropWord16 lenB a) as `equal` bs
  where lenA = T.lengthWord16 a
        lenB = T.lengthWord16 b

instance Eq Text where
    (==) = equal
    {-# INLINE (==) #-}

instance Ord Text where
    compare = compareText

compareText :: Text -> Text -> Ordering
compareText Empty Empty = EQ
compareText Empty _     = LT
compareText _     Empty = GT
compareText (Chunk a0 as) (Chunk b0 bs) = outer a0 b0
 where
  outer ta@(T.Text arrA offA lenA) tb@(T.Text arrB offB lenB) = go 0 0
   where
    go !i !j
      | i >= lenA = compareText as (chunk (T.Text arrB (offB+j) (lenB-j)) bs)
      | j >= lenB = compareText (chunk (T.Text arrA (offA+i) (lenA-i)) as) bs
      | a < b     = LT
      | a > b     = GT
      | otherwise = go (i+di) (j+dj)
      where T.Iter a di = T.iter ta i
            T.Iter b dj = T.iter tb j

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

#if MIN_VERSION_base(4,9,0)
-- Semigroup orphan instances for older GHCs are provided by
-- 'semigroups` package

instance Semigroup Text where
    (<>) = append
#endif

instance Monoid Text where
    mempty  = empty
#if MIN_VERSION_base(4,9,0)
    mappend = (<>) -- future-proof definition
#else
    mappend = append
#endif
    mconcat = concat

instance IsString Text where
    fromString = pack

#if __GLASGOW_HASKELL__ >= 708
instance Exts.IsList Text where
    type Item Text = Char
    fromList       = pack
    toList         = unpack
#endif

#if defined(HAVE_DEEPSEQ)
instance NFData Text where
    rnf Empty        = ()
    rnf (Chunk _ ts) = rnf ts
#endif

instance Binary Text where
    put t = put (encodeUtf8 t)
    get   = do
      bs <- get
      case decodeUtf8' bs of
        P.Left exn -> P.fail (P.show exn)
        P.Right a -> P.return a

-- | This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.
--
-- This instance was created by copying the updated behavior of
-- @"Data.Text".@'Data.Text.Text'
instance Data Text where
  gfoldl f z txt = z pack `f` (unpack txt)
  toConstr _     = packConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z pack)
    _ -> error "Data.Text.Lazy.Text.gunfold"
  dataTypeOf _   = textDataType

#if MIN_VERSION_base(4,7,0)
-- | Only defined for @base-4.7.0.0@ and later
instance PrintfArg Text where
  formatArg txt = formatString $ unpack txt
#endif

packConstr :: Constr
packConstr = mkConstr textDataType "pack" [] Prefix

textDataType :: DataType
textDataType = mkDataType "Data.Text.Lazy.Text" [packConstr]

-- | /O(n)/ Convert a 'String' into a 'Text'.
--
-- Subject to fusion.  Performs replacement on invalid scalar values.
pack :: String -> Text
pack = unstream . S.streamList . L.map safe
{-# INLINE [1] pack #-}

-- | /O(n)/ Convert a 'Text' into a 'String'.
-- Subject to fusion.
unpack :: Text -> String
unpack t = S.unstreamList (stream t)
{-# INLINE [1] unpack #-}

-- | /O(n)/ Convert a literal string into a Text.
unpackCString# :: Addr# -> Text
unpackCString# addr# = unstream (S.streamCString# addr#)
{-# NOINLINE unpackCString# #-}

{-# RULES "TEXT literal" forall a.
    unstream (S.streamList (L.map safe (GHC.unpackCString# a)))
      = unpackCString# a #-}

{-# RULES "TEXT literal UTF8" forall a.
    unstream (S.streamList (L.map safe (GHC.unpackCStringUtf8# a)))
      = unpackCString# a #-}

{-# RULES "LAZY TEXT empty literal"
    unstream (S.streamList (L.map safe []))
      = Empty #-}

{-# RULES "LAZY TEXT empty literal" forall a.
    unstream (S.streamList (L.map safe [a]))
      = Chunk (T.singleton a) Empty #-}

-- | /O(1)/ Convert a character into a Text.  Subject to fusion.
-- Performs replacement on invalid scalar values.
singleton :: Char -> Text
singleton c = Chunk (T.singleton c) Empty
{-# INLINE [1] singleton #-}

{-# RULES
"LAZY TEXT singleton -> fused" [~1] forall c.
    singleton c = unstream (S.singleton c)
"LAZY TEXT singleton -> unfused" [1] forall c.
    unstream (S.singleton c) = singleton c
  #-}

-- | /O(c)/ Convert a list of strict 'T.Text's into a lazy 'Text'.
fromChunks :: [T.Text] -> Text
fromChunks cs = L.foldr chunk Empty cs

-- | /O(n)/ Convert a lazy 'Text' into a list of strict 'T.Text's.
toChunks :: Text -> [T.Text]
toChunks cs = foldrChunks (:) [] cs

-- | /O(n)/ Convert a lazy 'Text' into a strict 'T.Text'.
toStrict :: Text -> T.Text
toStrict t = T.concat (toChunks t)
{-# INLINE [1] toStrict #-}

-- | /O(c)/ Convert a strict 'T.Text' into a lazy 'Text'.
fromStrict :: T.Text -> Text
fromStrict t = chunk t Empty
{-# INLINE [1] fromStrict #-}

-- -----------------------------------------------------------------------------
-- * Basic functions

-- | /O(1)/ Adds a character to the front of a 'Text'.  Subject to fusion.
cons :: Char -> Text -> Text
cons c t = Chunk (T.singleton c) t
{-# INLINE [1] cons #-}

infixr 5 `cons`

{-# RULES
"LAZY TEXT cons -> fused" [~1] forall c t.
    cons c t = unstream (S.cons c (stream t))
"LAZY TEXT cons -> unfused" [1] forall c t.
    unstream (S.cons c (stream t)) = cons c t
 #-}

-- | /O(n)/ Adds a character to the end of a 'Text'.  This copies the
-- entire array in the process, unless fused.  Subject to fusion.
snoc :: Text -> Char -> Text
snoc t c = foldrChunks Chunk (singleton c) t
{-# INLINE [1] snoc #-}

{-# RULES
"LAZY TEXT snoc -> fused" [~1] forall t c.
    snoc t c = unstream (S.snoc (stream t) c)
"LAZY TEXT snoc -> unfused" [1] forall t c.
    unstream (S.snoc (stream t) c) = snoc t c
 #-}

-- | /O(n\/c)/ Appends one 'Text' to another.  Subject to fusion.
append :: Text -> Text -> Text
append xs ys = foldrChunks Chunk ys xs
{-# INLINE [1] append #-}

{-# RULES
"LAZY TEXT append -> fused" [~1] forall t1 t2.
    append t1 t2 = unstream (S.append (stream t1) (stream t2))
"LAZY TEXT append -> unfused" [1] forall t1 t2.
    unstream (S.append (stream t1) (stream t2)) = append t1 t2
 #-}

-- | /O(1)/ Returns the first character and rest of a 'Text', or
-- 'Nothing' if empty. Subject to fusion.
uncons :: Text -> Maybe (Char, Text)
uncons Empty        = Nothing
uncons (Chunk t ts) = Just (T.unsafeHead t, ts')
  where ts' | T.compareLength t 1 == EQ = ts
            | otherwise                 = Chunk (T.unsafeTail t) ts
{-# INLINE uncons #-}

-- | /O(1)/ Returns the first character of a 'Text', which must be
-- non-empty.  Subject to fusion.
head :: Text -> Char
head t = S.head (stream t)
{-# INLINE head #-}

-- | /O(1)/ Returns all characters after the head of a 'Text', which
-- must be non-empty.  Subject to fusion.
tail :: Text -> Text
tail (Chunk t ts) = chunk (T.tail t) ts
tail Empty        = emptyError "tail"
{-# INLINE [1] tail #-}

{-# RULES
"LAZY TEXT tail -> fused" [~1] forall t.
    tail t = unstream (S.tail (stream t))
"LAZY TEXT tail -> unfused" [1] forall t.
    unstream (S.tail (stream t)) = tail t
 #-}

-- | /O(n\/c)/ Returns all but the last character of a 'Text', which must
-- be non-empty.  Subject to fusion.
init :: Text -> Text
init (Chunk t0 ts0) = go t0 ts0
    where go t (Chunk t' ts) = Chunk t (go t' ts)
          go t Empty         = chunk (T.init t) Empty
init Empty = emptyError "init"
{-# INLINE [1] init #-}

{-# RULES
"LAZY TEXT init -> fused" [~1] forall t.
    init t = unstream (S.init (stream t))
"LAZY TEXT init -> unfused" [1] forall t.
    unstream (S.init (stream t)) = init t
 #-}

-- | /O(n\/c)/ Returns the 'init' and 'last' of a 'Text', or 'Nothing' if
-- empty.
--
-- * It is no faster than using 'init' and 'last'.
unsnoc :: Text -> Maybe (Text, Char)
unsnoc Empty          = Nothing
unsnoc ts@(Chunk _ _) = Just (init ts, last ts)
{-# INLINE unsnoc #-}

-- | /O(1)/ Tests whether a 'Text' is empty or not.  Subject to
-- fusion.
null :: Text -> Bool
null Empty = True
null _     = False
{-# INLINE [1] null #-}

{-# RULES
"LAZY TEXT null -> fused" [~1] forall t.
    null t = S.null (stream t)
"LAZY TEXT null -> unfused" [1] forall t.
    S.null (stream t) = null t
 #-}

-- | /O(1)/ Tests whether a 'Text' contains exactly one character.
-- Subject to fusion.
isSingleton :: Text -> Bool
isSingleton = S.isSingleton . stream
{-# INLINE isSingleton #-}

-- | /O(n\/c)/ Returns the last character of a 'Text', which must be
-- non-empty.  Subject to fusion.
last :: Text -> Char
last Empty        = emptyError "last"
last (Chunk t ts) = go t ts
    where go _ (Chunk t' ts') = go t' ts'
          go t' Empty         = T.last t'
{-# INLINE [1] last #-}

{-# RULES
"LAZY TEXT last -> fused" [~1] forall t.
    last t = S.last (stream t)
"LAZY TEXT last -> unfused" [1] forall t.
    S.last (stream t) = last t
  #-}

-- | /O(n)/ Returns the number of characters in a 'Text'.
-- Subject to fusion.
length :: Text -> Int64
length = foldlChunks go 0
    where go l t = l + fromIntegral (T.length t)
{-# INLINE [1] length #-}

{-# RULES
"LAZY TEXT length -> fused" [~1] forall t.
    length t = S.length (stream t)
"LAZY TEXT length -> unfused" [1] forall t.
    S.length (stream t) = length t
 #-}

-- | /O(n)/ Compare the count of characters in a 'Text' to a number.
-- Subject to fusion.
--
-- This function gives the same answer as comparing against the result
-- of 'length', but can short circuit if the count of characters is
-- greater than the number, and hence be more efficient.
compareLength :: Text -> Int64 -> Ordering
compareLength t n = S.compareLengthI (stream t) n
{-# INLINE [1] compareLength #-}

-- We don't apply those otherwise appealing length-to-compareLength
-- rewrite rules here, because they can change the strictness
-- properties of code.

-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@.  Subject to fusion.  Performs replacement on
-- invalid scalar values.
map :: (Char -> Char) -> Text -> Text
map f t = unstream (S.map (safe . f) (stream t))
{-# INLINE [1] map #-}

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Text -> [Text] -> Text
intercalate t = concat . (F.intersperse t)
{-# INLINE intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'.  Subject to fusion.  Performs
-- replacement on invalid scalar values.
intersperse :: Char -> Text -> Text
intersperse c t = unstream (S.intersperse (safe c) (stream t))
{-# INLINE intersperse #-}

-- | /O(n)/ Left-justify a string to the given length, using the
-- specified fill character on the right. Subject to fusion.  Performs
-- replacement on invalid scalar values.
--
-- Examples:
--
-- > justifyLeft 7 'x' "foo"    == "fooxxxx"
-- > justifyLeft 3 'x' "foobar" == "foobar"
justifyLeft :: Int64 -> Char -> Text -> Text
justifyLeft k c t
    | len >= k  = t
    | otherwise = t `append` replicateChar (k-len) c
  where len = length t
{-# INLINE [1] justifyLeft #-}

{-# RULES
"LAZY TEXT justifyLeft -> fused" [~1] forall k c t.
    justifyLeft k c t = unstream (S.justifyLeftI k c (stream t))
"LAZY TEXT justifyLeft -> unfused" [1] forall k c t.
    unstream (S.justifyLeftI k c (stream t)) = justifyLeft k c t
  #-}

-- | /O(n)/ Right-justify a string to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- > justifyRight 7 'x' "bar"    == "xxxxbar"
-- > justifyRight 3 'x' "foobar" == "foobar"
justifyRight :: Int64 -> Char -> Text -> Text
justifyRight k c t
    | len >= k  = t
    | otherwise = replicateChar (k-len) c `append` t
  where len = length t
{-# INLINE justifyRight #-}

-- | /O(n)/ Center a string to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- > center 8 'x' "HS" = "xxxHSxxx"
center :: Int64 -> Char -> Text -> Text
center k c t
    | len >= k  = t
    | otherwise = replicateChar l c `append` t `append` replicateChar r c
  where len = length t
        d   = k - len
        r   = d `quot` 2
        l   = d - r
{-# INLINE center #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'Text' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
transpose :: [Text] -> [Text]
transpose ts = L.map (\ss -> Chunk (T.pack ss) Empty)
                     (L.transpose (L.map unpack ts))
-- TODO: make this fast

-- | /O(n)/ 'reverse' @t@ returns the elements of @t@ in reverse order.
reverse :: Text -> Text
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk t ts) = rev (Chunk (T.reverse t) a) ts

-- | /O(m+n)/ Replace every non-overlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
--
-- This function behaves as though it was defined as follows:
--
-- @
-- replace needle replacement haystack =
--   'intercalate' replacement ('splitOn' needle haystack)
-- @
--
-- As this suggests, each occurrence is replaced exactly once.  So if
-- @needle@ occurs in @replacement@, that occurrence will /not/ itself
-- be replaced recursively:
--
-- > replace "oo" "foo" "oo" == "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- > replace "ofo" "bar" "ofofo" == "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
replace :: Text
        -- ^ @needle@ to search for.  If this string is empty, an
        -- error will occur.
        -> Text
        -- ^ @replacement@ to replace @needle@ with.
        -> Text
        -- ^ @haystack@ in which to search.
        -> Text
replace s d = intercalate d . splitOn s
{-# INLINE replace #-}

-- ----------------------------------------------------------------------------
-- ** Case conversions (folds)

-- $case
--
-- With Unicode text, it is incorrect to use combinators like @map
-- toUpper@ to case convert each character of a string individually.
-- Instead, use the whole-string case conversion functions from this
-- module.  For correctness in different writing systems, these
-- functions may map one input character to two or three output
-- characters.

-- | /O(n)/ Convert a string to folded case.  Subject to fusion.
--
-- This function is mainly useful for performing caseless (or case
-- insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature men now (U+FB13) is case folded to the
-- bigram men now (U+0574 U+0576), while the micro sign (U+00B5) is
-- case folded to the Greek small letter letter mu (U+03BC) instead of
-- itself.
toCaseFold :: Text -> Text
toCaseFold t = unstream (S.toCaseFold (stream t))
{-# INLINE [0] toCaseFold #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.  Subject to fusion.
--
-- The result string may be longer than the input string.  For
-- instance, the Latin capital letter I with dot above (U+0130) maps
-- to the sequence Latin small letter i (U+0069) followed by combining
-- dot above (U+0307).
toLower :: Text -> Text
toLower t = unstream (S.toLower (stream t))
{-# INLINE toLower #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.  Subject to fusion.
--
-- The result string may be longer than the input string.  For
-- instance, the German eszett (U+00DF) maps to the two-letter
-- sequence SS.
toUpper :: Text -> Text
toUpper t = unstream (S.toUpper (stream t))
{-# INLINE toUpper #-}


-- | /O(n)/ Convert a string to title case, using simple case
-- conversion.  Subject to fusion.
--
-- The first letter of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
toTitle :: Text -> Text
toTitle t = unstream (S.toTitle (stream t))
{-# INLINE toTitle #-}

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from left to right.
-- Subject to fusion.
foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z t = S.foldl f z (stream t)
{-# INLINE foldl #-}

-- | /O(n)/ A strict version of 'foldl'.
-- Subject to fusion.
foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z t = S.foldl' f z (stream t)
{-# INLINE foldl' #-}

-- | /O(n)/ A variant of 'foldl' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.  Subject to fusion.
foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)
{-# INLINE foldl1 #-}

-- | /O(n)/ A strict version of 'foldl1'.  Subject to fusion.
foldl1' :: (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)
{-# INLINE foldl1' #-}

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from right to left.
-- Subject to fusion.
foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z t = S.foldr f z (stream t)
{-# INLINE foldr #-}

-- | /O(n)/ A variant of 'foldr' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.  Subject to
-- fusion.
foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)
{-# INLINE foldr1 #-}

-- | /O(n)/ Concatenate a list of 'Text's.
concat :: [Text] -> Text
concat = to
  where
    go Empty        css = to css
    go (Chunk c cs) css = Chunk c (go cs css)
    to []               = Empty
    to (cs:css)         = go cs css
{-# INLINE concat #-}

-- | /O(n)/ Map a function over a 'Text' that results in a 'Text', and
-- concatenate the results.
concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . foldr ((:) . f) []
{-# INLINE concatMap #-}

-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'Text' @t@ satisfies the predicate @p@. Subject to fusion.
any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)
{-# INLINE any #-}

-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'Text' @t@ satisfy the predicate @p@. Subject to fusion.
all :: (Char -> Bool) -> Text -> Bool
all p t = S.all p (stream t)
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'Text', which
-- must be non-empty. Subject to fusion.
maximum :: Text -> Char
maximum t = S.maximum (stream t)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'Text', which
-- must be non-empty. Subject to fusion.
minimum :: Text -> Char
minimum t = S.minimum (stream t)
{-# INLINE minimum #-}

-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left. Subject to fusion.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl g z (stream t))
    where g a b = safe (f a b)
{-# INLINE scanl #-}

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument.  Subject to fusion.  Performs replacement on
-- invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f t0 = case uncons t0 of
                Nothing -> empty
                Just (t,ts) -> scanl f t ts
{-# INLINE scanl1 #-}

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f v = reverse . scanl g v . reverse
    where g a b = safe (f b a)

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument.  Performs replacement on invalid scalar values.
scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f t | null t    = empty
           | otherwise = scanr f (last t) (init t)

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Text', passing an accumulating
-- parameter from left to right, and returns a final 'Text'.  Performs
-- replacement on invalid scalar values.
mapAccumL :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumL f = go
  where
    go z (Chunk c cs)    = (z'', Chunk c' cs')
        where (z',  c')  = T.mapAccumL f z c
              (z'', cs') = go z' cs
    go z Empty           = (z, Empty)
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'Text', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'Text'.  Performs replacement on invalid scalar values.
mapAccumR :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumR f = go
  where
    go z (Chunk c cs)   = (z'', Chunk c' cs')
        where (z'', c') = T.mapAccumR f z' c
              (z', cs') = go z cs
    go z Empty          = (z, Empty)
{-# INLINE mapAccumR #-}

-- | @'repeat' x@ is an infinite 'Text', with @x@ the value of every
-- element.
repeat :: Char -> Text
repeat c = let t = Chunk (T.replicate smallChunkSize (T.singleton c)) t
            in t

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'Text' consisting of the input
-- @t@ repeated @n@ times.
replicate :: Int64 -> Text -> Text
replicate n t
    | null t || n <= 0 = empty
    | isSingleton t    = replicateChar n (head t)
    | otherwise        = concat (rep 0)
    where rep !i | i >= n    = []
                 | otherwise = t : rep (i+1)
{-# INLINE [1] replicate #-}

-- | 'cycle' ties a finite, non-empty 'Text' into a circular one, or
-- equivalently, the infinite repetition of the original 'Text'.
cycle :: Text -> Text
cycle Empty = emptyError "cycle"
cycle t     = let t' = foldrChunks Chunk t' t
               in t'

-- | @'iterate' f x@ returns an infinite 'Text' of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
iterate :: (Char -> Char) -> Char -> Text
iterate f c = let t c' = Chunk (T.singleton c') (t (f c'))
               in t c

-- | /O(n)/ 'replicateChar' @n@ @c@ is a 'Text' of length @n@ with @c@ the
-- value of every element. Subject to fusion.
replicateChar :: Int64 -> Char -> Text
replicateChar n c = unstream (S.replicateCharI n (safe c))
{-# INLINE replicateChar #-}

{-# RULES
"LAZY TEXT replicate/singleton -> replicateChar" [~1] forall n c.
    replicate n (singleton c) = replicateChar n c
  #-}

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Text' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Text', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production.  Performs
-- replacement on invalid scalar values.
unfoldr :: (a -> Maybe (Char,a)) -> a -> Text
unfoldr f s = unstream (S.unfoldr (firstf safe . f) s)
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Text' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
-- Performs replacement on invalid scalar values.
unfoldrN :: Int64 -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n (firstf safe . f) s)
{-# INLINE unfoldrN #-}

-- | /O(n)/ 'take' @n@, applied to a 'Text', returns the prefix of the
-- 'Text' of length @n@, or the 'Text' itself if @n@ is greater than
-- the length of the Text. Subject to fusion.
take :: Int64 -> Text -> Text
take i _ | i <= 0 = Empty
take i t0         = take' i t0
  where take' 0 _            = Empty
        take' _ Empty        = Empty
        take' n (Chunk t ts)
            | n < len   = Chunk (T.take (fromIntegral n) t) Empty
            | otherwise = Chunk t (take' (n - len) ts)
            where len = fromIntegral (T.length t)
{-# INLINE [1] take #-}

{-# RULES
"LAZY TEXT take -> fused" [~1] forall n t.
    take n t = unstream (S.take n (stream t))
"LAZY TEXT take -> unfused" [1] forall n t.
    unstream (S.take n (stream t)) = take n t
  #-}

-- | /O(n)/ 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- > takeEnd 3 "foobar" == "bar"
takeEnd :: Int64 -> Text -> Text
takeEnd n t0
    | n <= 0    = empty
    | otherwise = takeChunk n empty . L.reverse . toChunks $ t0
  where takeChunk _ acc [] = acc
        takeChunk i acc (t:ts)
          | i <= l    = chunk (T.takeEnd (fromIntegral i) t) acc
          | otherwise = takeChunk (i-l) (Chunk t acc) ts
          where l = fromIntegral (T.length t)

-- | /O(n)/ 'drop' @n@, applied to a 'Text', returns the suffix of the
-- 'Text' after the first @n@ characters, or the empty 'Text' if @n@
-- is greater than the length of the 'Text'. Subject to fusion.
drop :: Int64 -> Text -> Text
drop i t0
    | i <= 0    = t0
    | otherwise = drop' i t0
  where drop' 0 ts           = ts
        drop' _ Empty        = Empty
        drop' n (Chunk t ts)
            | n < len   = Chunk (T.drop (fromIntegral n) t) ts
            | otherwise = drop' (n - len) ts
            where len   = fromIntegral (T.length t)
{-# INLINE [1] drop #-}

{-# RULES
"LAZY TEXT drop -> fused" [~1] forall n t.
    drop n t = unstream (S.drop n (stream t))
"LAZY TEXT drop -> unfused" [1] forall n t.
    unstream (S.drop n (stream t)) = drop n t
  #-}

-- | /O(n)/ 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@.
--
-- Examples:
--
-- > dropEnd 3 "foobar" == "foo"
dropEnd :: Int64 -> Text -> Text
dropEnd n t0
    | n <= 0    = t0
    | otherwise = dropChunk n . L.reverse . toChunks $ t0
  where dropChunk _ [] = empty
        dropChunk m (t:ts)
          | m >= l    = dropChunk (m-l) ts
          | otherwise = fromChunks . L.reverse $
                        T.dropEnd (fromIntegral m) t : ts
          where l = fromIntegral (T.length t)

-- | /O(n)/ 'dropWords' @n@ returns the suffix with @n@ 'Word16'
-- values dropped, or the empty 'Text' if @n@ is greater than the
-- number of 'Word16' values present.
dropWords :: Int64 -> Text -> Text
dropWords i t0
    | i <= 0    = t0
    | otherwise = drop' i t0
  where drop' 0 ts           = ts
        drop' _ Empty        = Empty
        drop' n (Chunk (T.Text arr off len) ts)
            | n < len'  = chunk (text arr (off+n') (len-n')) ts
            | otherwise = drop' (n - len') ts
            where len'  = fromIntegral len
                  n'    = fromIntegral n

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Text',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.  Subject to fusion.
takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t0 = takeWhile' t0
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n | n > 0     -> Chunk (T.take n t) Empty
                   | otherwise -> Empty
            Nothing            -> Chunk t (takeWhile' ts)
{-# INLINE [1] takeWhile #-}

{-# RULES
"LAZY TEXT takeWhile -> fused" [~1] forall p t.
    takeWhile p t = unstream (S.takeWhile p (stream t))
"LAZY TEXT takeWhile -> unfused" [1] forall p t.
    unstream (S.takeWhile p (stream t)) = takeWhile p t
  #-}
-- | /O(n)/ 'takeWhileEnd', applied to a predicate @p@ and a 'Text',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- > takeWhileEnd (=='o') "foo" == "oo"
takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = takeChunk empty . L.reverse . toChunks
  where takeChunk acc []     = acc
        takeChunk acc (t:ts)
          | T.lengthWord16 t' < T.lengthWord16 t
                             = chunk t' acc
          | otherwise        = takeChunk (Chunk t' acc) ts
          where t' = T.takeWhileEnd p t
{-# INLINE takeWhileEnd #-}

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.  Subject to fusion.
dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t0 = dropWhile' t0
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n  -> Chunk (T.drop n t) ts
            Nothing -> dropWhile' ts
{-# INLINE [1] dropWhile #-}

{-# RULES
"LAZY TEXT dropWhile -> fused" [~1] forall p t.
    dropWhile p t = unstream (S.dropWhile p (stream t))
"LAZY TEXT dropWhile -> unfused" [1] forall p t.
    unstream (S.dropWhile p (stream t)) = dropWhile p t
  #-}

-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- > dropWhileEnd (=='.') "foo..." == "foo"
dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = go
  where go Empty = Empty
        go (Chunk t Empty) = if T.null t'
                             then Empty
                             else Chunk t' Empty
            where t' = T.dropWhileEnd p t
        go (Chunk t ts) = case go ts of
                            Empty -> go (Chunk t Empty)
                            ts' -> Chunk t ts'
{-# INLINE dropWhileEnd #-}

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.  Subject to fusion.
dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p
{-# INLINE [1] dropAround #-}

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
stripStart :: Text -> Text
stripStart = dropWhile isSpace
{-# INLINE [1] stripStart #-}

-- | /O(n)/ Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
stripEnd :: Text -> Text
stripEnd = dropWhileEnd isSpace
{-# INLINE [1] stripEnd #-}

-- | /O(n)/ Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
strip :: Text -> Text
strip = dropAround isSpace
{-# INLINE [1] strip #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: Int64 -> Text -> (Text, Text)
splitAt = loop
  where loop _ Empty      = (empty, empty)
        loop n t | n <= 0 = (empty, t)
        loop n (Chunk t ts)
             | n < len   = let (t',t'') = T.splitAt (fromIntegral n) t
                           in (Chunk t' Empty, Chunk t'' ts)
             | otherwise = let (ts',ts'') = loop (n - len) ts
                           in (Chunk t ts', ts'')
             where len = fromIntegral (T.length t)

-- | /O(n)/ 'splitAtWord' @n t@ returns a strict pair whose first
-- element is a prefix of @t@ whose chunks contain @n@ 'Word16'
-- values, and whose second is the remainder of the string.
splitAtWord :: Int64 -> Text -> PairS Text Text
splitAtWord _ Empty = empty :*: empty
splitAtWord x (Chunk c@(T.Text arr off len) cs)
    | y >= len  = let h :*: t = splitAtWord (x-fromIntegral len) cs
                  in  Chunk c h :*: t
    | otherwise = chunk (text arr off y) empty :*:
                  chunk (text arr (off+y) (len-y)) cs
    where y = fromIntegral x

-- | /O(n+m)/ Find the first instance of @needle@ (which must be
-- non-'null') in @haystack@.  The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- > breakOn "::" "a::b::c" ==> ("a", "::b::c")
-- > breakOn "/" "foobar"   ==> ("foobar", "")
--
-- Laws:
--
-- > append prefix match == haystack
-- >   where (prefix, match) = breakOn needle haystack
--
-- If you need to break a string by a substring repeatedly (e.g. you
-- want to break on every instance of a substring), use 'breakOnAll'
-- instead, as it has lower startup overhead.
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
breakOn :: Text -> Text -> (Text, Text)
breakOn pat src
    | null pat  = emptyError "breakOn"
    | otherwise = case indices pat src of
                    []    -> (src, empty)
                    (x:_) -> let h :*: t = splitAtWord x src
                             in  (h, t)

-- | /O(n+m)/ Similar to 'breakOn', but searches from the end of the string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- > breakOnEnd "::" "a::b::c" ==> ("a::b::", "c")
breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd pat src = let (a,b) = breakOn (reverse pat) (reverse src)
                   in  (reverse b, reverse a)
{-# INLINE breakOnEnd #-}

-- | /O(n+m)/ Find all non-overlapping instances of @needle@ in
-- @haystack@.  Each element of the returned list consists of a pair:
--
-- * The entire string prior to the /k/th match (i.e. the prefix)
--
-- * The /k/th match, followed by the remainder of the string
--
-- Examples:
--
-- > breakOnAll "::" ""
-- > ==> []
-- > breakOnAll "/" "a/b/c/"
-- > ==> [("a", "/b/c/"), ("a/b", "/c/"), ("a/b/c", "/")]
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- The @needle@ parameter may not be empty.
breakOnAll :: Text              -- ^ @needle@ to search for
           -> Text              -- ^ @haystack@ in which to search
           -> [(Text, Text)]
breakOnAll pat src
    | null pat  = emptyError "breakOnAll"
    | otherwise = go 0 empty src (indices pat src)
  where
    go !n p s (x:xs) = let h :*: t = splitAtWord (x-n) s
                           h'      = append p h
                       in (h',t) : go x h' t xs
    go _  _ _ _      = []

-- | /O(n)/ 'break' is like 'span', but the prefix returned is over
-- elements that fail the predicate @p@.
break :: (Char -> Bool) -> Text -> (Text, Text)
break p t0 = break' t0
  where break' Empty          = (empty, empty)
        break' c@(Chunk t ts) =
          case T.findIndex p t of
            Nothing      -> let (ts', ts'') = break' ts
                            in (Chunk t ts', ts'')
            Just n | n == 0    -> (Empty, c)
                   | otherwise -> let (a,b) = T.splitAt n t
                                  in (Chunk a Empty, Chunk b ts)

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the list.
span :: (Char -> Bool) -> Text -> (Text, Text)
span p = break (not . p)
{-# INLINE span #-}

-- | The 'group' function takes a 'Text' and returns a list of 'Text's
-- such that the concatenation of the result is equal to the argument.
-- Moreover, each sublist in the result contains only equal elements.
-- For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test.
group :: Text -> [Text]
group =  groupBy (==)
{-# INLINE group #-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy _  Empty        = []
groupBy eq (Chunk t ts) = cons x ys : groupBy eq zs
                          where (ys,zs) = span (eq x) xs
                                x  = T.unsafeHead t
                                xs = chunk (T.unsafeTail t) ts

-- | /O(n)/ Return all initial segments of the given 'Text',
-- shortest first.
inits :: Text -> [Text]
inits = (Empty :) . inits'
  where inits' Empty        = []
        inits' (Chunk t ts) = L.map (\t' -> Chunk t' Empty) (L.tail (T.inits t))
                           ++ L.map (Chunk t) (inits' ts)

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
tails :: Text -> [Text]
tails Empty         = Empty : []
tails ts@(Chunk t ts')
  | T.length t == 1 = ts : tails ts'
  | otherwise       = ts : tails (Chunk (T.unsafeTail t) ts')

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Text's that
-- are slices of the original.

-- | /O(m+n)/ Break a 'Text' into pieces separated by the first 'Text'
-- argument (which cannot be an empty string), consuming the
-- delimiter. An empty delimiter is invalid, and will cause an error
-- to be raised.
--
-- Examples:
--
-- > splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
-- > splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
-- > splitOn "x"    "x"                == ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
--
-- (Note: the string @s@ to split on above cannot be empty.)
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
splitOn :: Text
        -- ^ String to split on. If this string is empty, an error
        -- will occur.
        -> Text
        -- ^ Input text.
        -> [Text]
splitOn pat src
    | null pat        = emptyError "splitOn"
    | isSingleton pat = split (== head pat) src
    | otherwise       = go 0 (indices pat src) src
  where
    go  _ []     cs = [cs]
    go !i (x:xs) cs = let h :*: t = splitAtWord (x-i) cs
                      in  h : go (x+l) xs (dropWords l t)
    l = foldlChunks (\a (T.Text _ _ b) -> a + fromIntegral b) 0 pat
{-# INLINE [1] splitOn #-}

{-# RULES
"LAZY TEXT splitOn/singleton -> split/==" [~1] forall c t.
    splitOn (singleton c) t = split (==c) t
  #-}

-- | /O(n)/ Splits a 'Text' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > split (=='a') "aabbaca" == ["","","bb","c",""]
-- > split (=='a') []        == [""]
split :: (Char -> Bool) -> Text -> [Text]
split _ Empty = [Empty]
split p (Chunk t0 ts0) = comb [] (T.split p t0) ts0
  where comb acc (s:[]) Empty        = revChunks (s:acc) : []
        comb acc (s:[]) (Chunk t ts) = comb (s:acc) (T.split p t) ts
        comb acc (s:ss) ts           = revChunks (s:acc) : comb [] ss ts
        comb _   []     _            = impossibleError "split"
{-# INLINE split #-}

-- | /O(n)/ Splits a 'Text' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- > chunksOf 3 "foobarbaz"   == ["foo","bar","baz"]
-- > chunksOf 4 "haskell.org" == ["hask","ell.","org"]
chunksOf :: Int64 -> Text -> [Text]
chunksOf k = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b
{-# INLINE chunksOf #-}

-- | /O(n)/ Breaks a 'Text' up into a list of 'Text's at
-- newline 'Char's. The resulting strings do not contain newlines.
lines :: Text -> [Text]
lines Empty = []
lines t = let (l,t') = break ((==) '\n') t
          in l : if null t' then []
                 else lines (tail t')

-- | /O(n)/ Breaks a 'Text' up into a list of words, delimited by 'Char's
-- representing white space.
words :: Text -> [Text]
words = L.filter (not . null) . split isSpace
{-# INLINE words #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: [Text] -> Text
unlines = concat . L.map (`snoc` '\n')
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'Text's and returns
-- 'True' iff the first is a prefix of the second.  Subject to fusion.
isPrefixOf :: Text -> Text -> Bool
isPrefixOf Empty _  = True
isPrefixOf _ Empty  = False
isPrefixOf (Chunk x xs) (Chunk y ys)
    | lx == ly  = x == y  && isPrefixOf xs ys
    | lx <  ly  = x == yh && isPrefixOf xs (Chunk yt ys)
    | otherwise = xh == y && isPrefixOf (Chunk xt xs) ys
  where (xh,xt) = T.splitAt ly x
        (yh,yt) = T.splitAt lx y
        lx = T.length x
        ly = T.length y
{-# INLINE [1] isPrefixOf #-}

{-# RULES
"LAZY TEXT isPrefixOf -> fused" [~1] forall s t.
    isPrefixOf s t = S.isPrefixOf (stream s) (stream t)
"LAZY TEXT isPrefixOf -> unfused" [1] forall s t.
    S.isPrefixOf (stream s) (stream t) = isPrefixOf s t
  #-}

-- | /O(n)/ The 'isSuffixOf' function takes two 'Text's and returns
-- 'True' iff the first is a suffix of the second.
isSuffixOf :: Text -> Text -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y
{-# INLINE isSuffixOf #-}
-- TODO: a better implementation

-- | /O(n+m)/ The 'isInfixOf' function takes two 'Text's and returns
-- 'True' iff the first is contained, wholly and intact, anywhere
-- within the second.
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
isInfixOf :: Text -> Text -> Bool
isInfixOf needle haystack
    | null needle        = True
    | isSingleton needle = S.elem (head needle) . S.stream $ haystack
    | otherwise          = not . L.null . indices needle $ haystack
{-# INLINE [1] isInfixOf #-}

{-# RULES
"LAZY TEXT isInfixOf/singleton -> S.elem/S.stream" [~1] forall n h.
    isInfixOf (singleton n) h = S.elem n (S.stream h)
  #-}

-------------------------------------------------------------------------------
-- * View patterns

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string.
--
-- Examples:
--
-- > stripPrefix "foo" "foobar" == Just "bar"
-- > stripPrefix ""    "baz"    == Just "baz"
-- > stripPrefix "foo" "quux"   == Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text.Lazy as T
-- >
-- > fnordLength :: Text -> Int
-- > fnordLength (stripPrefix "fnord" -> Just suf) = T.length suf
-- > fnordLength _                                 = -1
stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p t
    | null p    = Just t
    | otherwise = case commonPrefixes p t of
                    Just (_,c,r) | null c -> Just r
                    _                     -> Nothing

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match.
--
-- If the strings do not have a common prefix or either one is empty,
-- this function returns 'Nothing'.
--
-- Examples:
--
-- > commonPrefixes "foobar" "fooquux" == Just ("foo","bar","quux")
-- > commonPrefixes "veeble" "fetzer"  == Nothing
-- > commonPrefixes "" "baz"           == Nothing
commonPrefixes :: Text -> Text -> Maybe (Text,Text,Text)
commonPrefixes Empty _ = Nothing
commonPrefixes _ Empty = Nothing
commonPrefixes a0 b0   = Just (go a0 b0 [])
  where
    go t0@(Chunk x xs) t1@(Chunk y ys) ps
        = case T.commonPrefixes x y of
            Just (p,a,b)
              | T.null a  -> go xs (chunk b ys) (p:ps)
              | T.null b  -> go (chunk a xs) ys (p:ps)
              | otherwise -> (fromChunks (L.reverse (p:ps)),chunk a xs, chunk b ys)
            Nothing       -> (fromChunks (L.reverse ps),t0,t1)
    go t0 t1 ps = (fromChunks (L.reverse ps),t0,t1)

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- > stripSuffix "bar" "foobar" == Just "foo"
-- > stripSuffix ""    "baz"    == Just "baz"
-- > stripSuffix "foo" "quux"   == Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text.Lazy as T
-- >
-- > quuxLength :: Text -> Int
-- > quuxLength (stripSuffix "quux" -> Just pre) = T.length pre
-- > quuxLength _                                = -1
stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p t = reverse `fmap` stripPrefix (reverse p) (reverse t)

-- | /O(n)/ 'filter', applied to a predicate and a 'Text',
-- returns a 'Text' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
filter p t = unstream (S.filter p (stream t))
{-# INLINE filter #-}

-- | /O(n)/ The 'find' function takes a predicate and a 'Text', and
-- returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.findBy p (stream t)
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate and a 'Text',
-- and returns the pair of 'Text's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)
{-# INLINE partition #-}

-- | /O(n)/ 'Text' index (subscript) operator, starting from 0.
index :: Text -> Int64 -> Char
index t n = S.index (stream t) n
{-# INLINE index #-}

-- | /O(n+m)/ The 'count' function returns the number of times the
-- query string appears in the given 'Text'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
count :: Text -> Text -> Int64
count pat src
    | null pat        = emptyError "count"
    | otherwise       = go 0 (indices pat src)
  where go !n []     = n
        go !n (_:xs) = go (n+1) xs
{-# INLINE [1] count #-}

{-# RULES
"LAZY TEXT count/singleton -> countChar" [~1] forall c t.
    count (singleton c) t = countChar c t
  #-}

-- | /O(n)/ The 'countChar' function returns the number of times the
-- query element appears in the given 'Text'.  Subject to fusion.
countChar :: Char -> Text -> Int64
countChar c t = S.countChar c (stream t)

-- | /O(n)/ 'zip' takes two 'Text's and returns a list of
-- corresponding pairs of bytes. If one input 'Text' is short,
-- excess elements of the longer 'Text' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Text -> Text -> [(Char,Char)]
zip a b = S.unstreamList $ S.zipWith (,) (stream a) (stream b)
{-# INLINE [0] zip #-}

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith g (stream t1) (stream t2))
    where g a b = safe (f a b)
{-# INLINE [0] zipWith #-}

revChunks :: [T.Text] -> Text
revChunks = L.foldl' (flip chunk) Empty

emptyError :: String -> a
emptyError fun = P.error ("Data.Text.Lazy." ++ fun ++ ": empty input")

impossibleError :: String -> a
impossibleError fun = P.error ("Data.Text.Lazy." ++ fun ++ ": impossible case")
