{-# OPTIONS_HADDOCK prune #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Split.Internals
-- Copyright   :  (c) Brent Yorgey, Louis Wasserman 2008-2012
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
-- Stability   :  stable
-- Portability :  Haskell 2010
--
-- Implementation module for "Data.List.Split", a combinator library
-- for splitting lists.  See the "Data.List.Split" documentation for
-- more description and examples.
--
-----------------------------------------------------------------------------

module Data.List.Split.Internals where

import           Data.List (genericSplitAt)

-- * Types and utilities

-- | A splitting strategy.
data Splitter a = Splitter { delimiter        :: Delimiter a
                               -- ^ What delimiter to split on
                           , delimPolicy      :: DelimPolicy
                               -- ^ What to do with delimiters (drop
                               --   from output, keep as separate
                               --   elements in output, or merge with
                               --   previous or following chunks)
                           , condensePolicy   :: CondensePolicy
                               -- ^ What to do with multiple
                               --   consecutive delimiters
                           , initBlankPolicy  :: EndPolicy
                               -- ^ Drop an initial blank?
                           , finalBlankPolicy :: EndPolicy
                               -- ^ Drop a final blank?
                           }

-- | The default splitting strategy: keep delimiters in the output
--   as separate chunks, don't condense multiple consecutive
--   delimiters into one, keep initial and final blank chunks.
--   Default delimiter is the constantly false predicate.
--
--   Note that 'defaultSplitter' should normally not be used; use
--   'oneOf', 'onSublist', or 'whenElt' instead, which are the same as
--   the 'defaultSplitter' with just the delimiter overridden.
--
--   The 'defaultSplitter' strategy with any delimiter gives a
--   maximally information-preserving splitting strategy, in the sense
--   that (a) taking the 'concat' of the output yields the original
--   list, and (b) given only the output list, we can reconstruct a
--   'Splitter' which would produce the same output list again given
--   the original input list.  This default strategy can be overridden
--   to allow discarding various sorts of information.
defaultSplitter :: Splitter a
defaultSplitter = Splitter { delimiter        = Delimiter [const False]
                           , delimPolicy      = Keep
                           , condensePolicy   = KeepBlankFields
                           , initBlankPolicy  = KeepBlank
                           , finalBlankPolicy = KeepBlank
                           }

-- | A delimiter is a list of predicates on elements, matched by some
--   contiguous subsequence of a list.
newtype Delimiter a = Delimiter [a -> Bool]

-- | Try to match a delimiter at the start of a list, either failing
--   or decomposing the list into the portion which matched the delimiter
--   and the remainder.
matchDelim :: Delimiter a -> [a] -> Maybe ([a],[a])
matchDelim (Delimiter []) xs = Just ([],xs)
matchDelim (Delimiter _)  [] = Nothing
matchDelim (Delimiter (p:ps)) (x:xs)
  | p x       = matchDelim (Delimiter ps) xs >>= \(h,t) -> Just (x:h,t)
  | otherwise = Nothing

-- | What to do with delimiters?
data DelimPolicy = Drop      -- ^ Drop delimiters from the output.
                 | Keep      -- ^ Keep delimiters as separate chunks
                             --   of the output.
                 | KeepLeft  -- ^ Keep delimiters in the output,
                             --   prepending them to the following
                             --   chunk.
                 | KeepRight -- ^ Keep delimiters in the output,
                             --   appending them to the previous chunk.
  deriving (Eq, Show)

-- | What to do with multiple consecutive delimiters?
data CondensePolicy = Condense         -- ^ Condense into a single delimiter.
                    | DropBlankFields  -- ^ Keep consecutive
                                       --   delimiters separate, but
                                       --   don't insert blank chunks in
                                       --   between them.
                    | KeepBlankFields  -- ^ Insert blank chunks
                                       --   between consecutive
                                       --   delimiters.
  deriving (Eq, Show)

-- | What to do with a blank chunk at either end of the list
--   (/i.e./ when the list begins or ends with a delimiter).
data EndPolicy = DropBlank | KeepBlank
  deriving (Eq, Show)

-- | Tag chunks as delimiters or text.
data Chunk a = Delim [a] | Text [a]
  deriving (Show, Eq)

-- | Internal representation of a split list that tracks which pieces
--   are delimiters and which aren't.
type SplitList a = [Chunk a]

-- | Untag a 'Chunk'.
fromElem :: Chunk a -> [a]
fromElem (Text as) = as
fromElem (Delim as) = as

-- | Test whether a 'Chunk' is a delimiter.
isDelim :: Chunk a -> Bool
isDelim (Delim _) = True
isDelim _ = False

-- | Test whether a 'Chunk' is text.
isText :: Chunk a -> Bool
isText (Text _) = True
isText _ = False

-- * Implementation

-- | Given a delimiter to use, split a list into an internal
--   representation with chunks tagged as delimiters or text.  This
--   transformation is lossless; in particular,
--
-- @
--   'concatMap' 'fromElem' ('splitInternal' d l) == l.
-- @
splitInternal :: Delimiter a -> [a] -> SplitList a
splitInternal _ [] = []
splitInternal d xxs
  | null xs   = toSplitList match
  | otherwise = Text xs : toSplitList match
 where
  (xs,match) = breakDelim d xxs

  toSplitList Nothing             = []
  toSplitList (Just ([],r:rs))    = Delim [] : Text [r] : splitInternal d rs
  toSplitList (Just (delim,rest)) = Delim delim : splitInternal d rest

breakDelim :: Delimiter a -> [a] -> ([a],Maybe ([a],[a]))
breakDelim (Delimiter []) xs         = ([],Just ([],xs))
breakDelim _              []         = ([],Nothing)
breakDelim d              xxs@(x:xs) =
  case matchDelim d xxs of
      Nothing    -> let (ys,match) = breakDelim d xs in (x:ys,match)
      Just match -> ([], Just match)

-- | Given a split list in the internal tagged representation, produce
--   a new internal tagged representation corresponding to the final
--   output, according to the strategy defined by the given
--   'Splitter'.
postProcess :: Splitter a -> SplitList a -> SplitList a
postProcess s = dropFinal (finalBlankPolicy s)
              . dropInitial (initBlankPolicy s)
              . doMerge (delimPolicy s)
              . doDrop (delimPolicy s)
              . insertBlanks (condensePolicy s)
              . doCondense (condensePolicy s)

-- | Drop delimiters if the 'DelimPolicy' is 'Drop'.
doDrop :: DelimPolicy -> SplitList a -> SplitList a
doDrop Drop l = [ c | c@(Text _) <- l ]
doDrop _ l = l

-- | Condense multiple consecutive delimiters into one if the
--   'CondensePolicy' is 'Condense'.
doCondense :: CondensePolicy -> SplitList a -> SplitList a
doCondense Condense ls = condense' ls
  where condense' [] = []
        condense' (c@(Text _) : l) = c : condense' l
        condense' l = (Delim $ concatMap fromElem ds) : condense' rest
          where (ds,rest) = span isDelim l
doCondense _ ls = ls

-- | Insert blank chunks between any remaining consecutive delimiters
--   (unless the condense policy is 'DropBlankFields'), and at the
--   beginning or end if the first or last element is a delimiter.
insertBlanks :: CondensePolicy -> SplitList a -> SplitList a
insertBlanks _ [] = [Text []]
insertBlanks cp (d@(Delim _) : l) = Text [] : insertBlanks' cp (d:l)
insertBlanks cp l = insertBlanks' cp l

-- | Insert blank chunks between consecutive delimiters.
insertBlanks' :: CondensePolicy -> SplitList a -> SplitList a
insertBlanks' _ [] = []
insertBlanks' cp@DropBlankFields (d1@(Delim _) : d2@(Delim _) : l)
  = d1           : insertBlanks' cp (d2:l)
insertBlanks' cp (d1@(Delim _) : d2@(Delim _) : l)
  = d1 : Text [] : insertBlanks' cp (d2:l)
insertBlanks' _ [d@(Delim _)] = [d, Text []]
insertBlanks' cp (c : l) = c : insertBlanks' cp l

-- | Merge delimiters into adjacent chunks according to the 'DelimPolicy'.
doMerge :: DelimPolicy -> SplitList a -> SplitList a
doMerge KeepLeft = mergeLeft
doMerge KeepRight = mergeRight
doMerge _ = id

-- | Merge delimiters with adjacent chunks to the right (yes, that's
--   not a typo: the delimiters should end up on the left of the
--   chunks, so they are merged with chunks to their right).
mergeLeft :: SplitList a -> SplitList a
mergeLeft [] = []
mergeLeft ((Delim d) : (Text c) : l) = Text (d++c) : mergeLeft l
mergeLeft (c : l) = c : mergeLeft l

-- | Merge delimiters with adjacent chunks to the left.
mergeRight :: SplitList a -> SplitList a
mergeRight [] = []
-- below fanciness is with the goal of laziness: we want to start returning
-- stuff before we've necessarily discovered a delimiter, in case we're
-- processing some infinite list with no delimiter
mergeRight ((Text c) : l) = Text (c++d) : mergeRight lTail
  where (d, lTail) = case l of
                     Delim d' : l' -> (d', l')
                     _ -> ([], l)
mergeRight (c : l) = c : mergeRight l

-- | Drop an initial blank chunk according to the given 'EndPolicy'.
dropInitial :: EndPolicy -> SplitList a -> SplitList a
dropInitial DropBlank (Text [] : l) = l
dropInitial _ l = l

-- | Drop a final blank chunk according to the given 'EndPolicy'.
dropFinal :: EndPolicy -> SplitList a -> SplitList a
dropFinal _         [] = []
dropFinal DropBlank l  = dropFinal' l
  where dropFinal' []        = []
        dropFinal' [Text []] = []
        dropFinal' (x:xs)    = x:dropFinal' xs
dropFinal _         l  = l

-- * Combinators

-- | Split a list according to the given splitting strategy.  This is
--   how to \"run\" a 'Splitter' that has been built using the other
--   combinators.
split :: Splitter a -> [a] -> [[a]]
split s = map fromElem . postProcess s . splitInternal (delimiter s)

-- ** Basic strategies
--
-- $ All these basic strategies have the same parameters as the
-- 'defaultSplitter' except for the delimiters.

-- | A splitting strategy that splits on any one of the given
--   elements.  For example:
--
-- > split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
oneOf :: Eq a => [a] -> Splitter a
oneOf elts = defaultSplitter { delimiter = Delimiter [(`elem` elts)] }

-- | A splitting strategy that splits on the given list, when it is
--   encountered as an exact subsequence.  For example:
--
-- > split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
--
--   Note that splitting on the empty list is a special case, which
--   splits just before every element of the list being split.  For example:
--
-- > split (onSublist "") "abc" == ["","","a","","b","","c"]
-- > split (dropDelims . dropBlanks $ onSublist "") "abc" == ["a","b","c"]
--
--   However, if you want to break a list into singleton elements like
--   this, you are better off using @'chunksOf' 1@, or better yet,
--   @'map' (:[])@.
onSublist :: Eq a => [a] -> Splitter a
onSublist lst = defaultSplitter { delimiter = Delimiter (map (==) lst) }

-- | A splitting strategy that splits on any elements that satisfy the
--   given predicate.  For example:
--
-- > split (whenElt (<0)) [2,4,-3,6,-9,1] == [[2,4],[-3],[6],[-9],[1]]
whenElt :: (a -> Bool) -> Splitter a
whenElt p = defaultSplitter { delimiter = Delimiter [p] }

-- ** Strategy transformers

-- | Drop delimiters from the output (the default is to keep
--   them). For example,
--
-- > split (oneOf ":") "a:b:c" == ["a", ":", "b", ":", "c"]
-- > split (dropDelims $ oneOf ":") "a:b:c" == ["a", "b", "c"]
dropDelims :: Splitter a -> Splitter a
dropDelims s = s { delimPolicy = Drop }

-- | Keep delimiters in the output by prepending them to adjacent
--   chunks.  For example:
--
-- > split (keepDelimsL $ oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
keepDelimsL :: Splitter a -> Splitter a
keepDelimsL s = s { delimPolicy = KeepLeft }

-- | Keep delimiters in the output by appending them to adjacent
--   chunks. For example:
--
-- > split (keepDelimsR $ oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
keepDelimsR :: Splitter a -> Splitter a
keepDelimsR s = s { delimPolicy = KeepRight }

-- | Condense multiple consecutive delimiters into one.  For example:
--
-- > split (condense $ oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","xyz","c","x","d"]
-- > split (dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","","","c","d"]
-- > split (condense . dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","c","d"]
condense :: Splitter a -> Splitter a
condense s = s { condensePolicy = Condense }

-- | Don't generate a blank chunk if there is a delimiter at the
--   beginning.  For example:
--
-- > split (oneOf ":") ":a:b" == ["",":","a",":","b"]
-- > split (dropInitBlank $ oneOf ":") ":a:b" == [":","a",":","b"]
dropInitBlank :: Splitter a -> Splitter a
dropInitBlank s = s { initBlankPolicy = DropBlank }

-- | Don't generate a blank chunk if there is a delimiter at the end.
--   For example:
--
-- > split (oneOf ":") "a:b:" == ["a",":","b",":",""]
-- > split (dropFinalBlank $ oneOf ":") "a:b:" == ["a",":","b",":"]
dropFinalBlank :: Splitter a -> Splitter a
dropFinalBlank s = s { finalBlankPolicy = DropBlank }

-- | Don't generate blank chunks between consecutive delimiters.
--   For example:
--
-- > split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- > split (dropInnerBlanks $ oneOf ":") "::b:::a" == ["", ":",":","b",":",":",":","a"]
dropInnerBlanks :: Splitter a -> Splitter a
dropInnerBlanks s = s { condensePolicy = DropBlankFields }

-- ** Derived combinators

-- | Drop all blank chunks from the output, and condense consecutive
--   delimiters into one.  Equivalent to @'dropInitBlank'
--   . 'dropFinalBlank' . 'condense'@.  For example:
--
-- > split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- > split (dropBlanks $ oneOf ":") "::b:::a" == ["::","b",":::","a"]
dropBlanks :: Splitter a -> Splitter a
dropBlanks = dropInitBlank . dropFinalBlank . condense

-- | Make a strategy that splits a list into chunks that all start
--   with the given subsequence (except possibly the first).
--   Equivalent to @'dropInitBlank' . 'keepDelimsL' . 'onSublist'@.
--   For example:
--
-- > split (startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
startsWith :: Eq a => [a] -> Splitter a
startsWith = dropInitBlank . keepDelimsL . onSublist

-- | Make a strategy that splits a list into chunks that all start
--   with one of the given elements (except possibly the first).
--   Equivalent to @'dropInitBlank' . 'keepDelimsL' . 'oneOf'@.  For
--   example:
--
-- > split (startsWithOneOf ['A'..'Z']) "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
startsWithOneOf :: Eq a => [a] -> Splitter a
startsWithOneOf = dropInitBlank . keepDelimsL . oneOf

-- | Make a strategy that splits a list into chunks that all end with
--   the given subsequence, except possibly the last.  Equivalent to
--   @'dropFinalBlank' . 'keepDelimsR' . 'onSublist'@.  For example:
--
-- > split (endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
endsWith :: Eq a => [a] -> Splitter a
endsWith = dropFinalBlank . keepDelimsR . onSublist

-- | Make a strategy that splits a list into chunks that all end with
--   one of the given elements, except possibly the last.  Equivalent
--   to @'dropFinalBlank' . 'keepDelimsR' . 'oneOf'@.  For example:
--
-- > split (condense $ endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
endsWithOneOf :: Eq a => [a] -> Splitter a
endsWithOneOf = dropFinalBlank . keepDelimsR . oneOf

-- ** Convenience functions
--
-- These functions implement some common splitting strategies.  Note
-- that all of the functions in this section drop delimiters from
-- the final output, since that is a more common use case even
-- though it is not the default.

-- | Split on any of the given elements.  Equivalent to @'split'
--   . 'dropDelims' . 'oneOf'@.  For example:
--
-- > splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf = split . dropDelims . oneOf

-- | Split on the given sublist.  Equivalent to @'split'
--   . 'dropDelims' . 'onSublist'@.  For example:
--
-- > splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
--
--   In some parsing combinator frameworks this is also known as
--   @sepBy@.
--
--   Note that this is the right inverse of the 'Data.List.intercalate' function
--   from "Data.List", that is,
--
--   > intercalate x . splitOn x === id
--
--   @'splitOn' x . 'Data.List.intercalate' x@ is the identity on
--   certain lists, but it is tricky to state the precise conditions
--   under which this holds.  (For example, it is not enough to say
--   that @x@ does not occur in any elements of the input list.
--   Working out why is left as an exercise for the reader.)
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn   = split . dropDelims . onSublist

-- | Split on elements satisfying the given predicate.  Equivalent to
--   @'split' . 'dropDelims' . 'whenElt'@.  For example:
--
-- > splitWhen (<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = split . dropDelims . whenElt

{-# DEPRECATED sepBy "Use splitOn." #-}
sepBy :: Eq a => [a] -> [a] -> [[a]]
sepBy = splitOn

{-# DEPRECATED sepByOneOf "Use splitOneOf." #-}
sepByOneOf :: Eq a => [a] -> [a] -> [[a]]
sepByOneOf = splitOneOf

-- | Split into chunks terminated by the given subsequence.
--   Equivalent to @'split' . 'dropFinalBlank' . 'dropDelims'
--   . 'onSublist'@.  For example:
--
-- > endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
--
--   Note also that the 'lines' function from "Data.List" is equivalent
--   to @'endBy' \"\\n\"@.
endBy :: Eq a => [a] -> [a] -> [[a]]
endBy = split . dropFinalBlank . dropDelims . onSublist

-- | Split into chunks terminated by one of the given elements.
--   Equivalent to @'split' . 'dropFinalBlank' . 'dropDelims'
--   . 'oneOf'@. For example:
--
-- > endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
endByOneOf :: Eq a => [a] -> [a] -> [[a]]
endByOneOf = split . dropFinalBlank . dropDelims . oneOf

{-# DEPRECATED unintercalate "Use splitOn." #-}
unintercalate :: Eq a => [a] -> [a] -> [[a]]
unintercalate = splitOn

-- | Split into \"words\", with word boundaries indicated by the given
--   predicate.  Satisfies @'Data.List.words' === wordsBy
--   'Data.Char.isSpace'@; equivalent to @'split' . 'dropBlanks'
--   . 'dropDelims' . 'whenElt'@.  For example:
--
-- > wordsBy (=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy = split . dropBlanks . dropDelims . whenElt

-- | Split into \"lines\", with line boundaries indicated by the given
--   predicate. Satisfies @'lines' === linesBy (=='\n')@; equivalent to
--   @'split' . 'dropFinalBlank' . 'dropDelims' . 'whenElt'@.  For example:
--
-- > linesBy (=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy = split . dropFinalBlank . dropDelims . whenElt

-- * Other splitting methods

-- | Standard build function, specialized to building lists.
--
--   Usually build is given the rank-2 type
--
--   > build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
--
--   but since we only use it when @(b ~ [a])@, we give it the more
--   restricted type signature in order to avoid needing a
--   non-Haskell2010 extension.
--
--   Note that the 0.1.4.3 release of this package did away with a
--   custom @build@ implementation in favor of importing one from
--   "GHC.Exts", which was (reportedly) faster for some applications.
--   However, in the interest of simplicity and complete Haskell2010
--   compliance as @split@ is being included in the Haskel Platform,
--   version 0.2.1.0 has gone back to defining @build@ manually.  This
--   is in line with @split@'s design philosophy of having efficiency
--   as a non-goal.
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

-- | @'chunksOf' n@ splits a list into length-n pieces.  The last
--   piece will be shorter if @n@ does not evenly divide the length of
--   the list.  If @n <= 0@, @'chunksOf' n l@ returns an infinite list
--   of empty lists.  For example:
--
--   Note that @'chunksOf' n []@ is @[]@, not @[[]]@.  This is
--   intentional, and is consistent with a recursive definition of
--   'chunksOf'; it satisfies the property that
--
--   @chunksOf n xs ++ chunksOf n ys == chunksOf n (xs ++ ys)@
--
--   whenever @n@ evenly divides the length of @xs@.
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

{-# DEPRECATED chunk "Use chunksOf." #-}
chunk :: Int -> [e] -> [[e]]
chunk = chunksOf

{-# DEPRECATED splitEvery "Use chunksOf." #-}
splitEvery :: Int -> [e] -> [[e]]
splitEvery = chunksOf

-- | Split a list into chunks of the given lengths. For example:
--
-- > splitPlaces [2,3,4] [1..20] == [[1,2],[3,4,5],[6,7,8,9]]
-- > splitPlaces [4,9] [1..10] == [[1,2,3,4],[5,6,7,8,9,10]]
-- > splitPlaces [4,9,3] [1..10] == [[1,2,3,4],[5,6,7,8,9,10]]
--
--   If the input list is longer than the total of the given lengths,
--   then the remaining elements are dropped. If the list is shorter
--   than the total of the given lengths, then the result may contain
--   fewer chunks than requested, and the last chunk may be shorter
--   than requested.
splitPlaces :: Integral a => [a] -> [e] -> [[e]]
splitPlaces is ys = build (splitPlacer is ys) where
  splitPlacer :: Integral i => [i] -> [b] -> ([b] -> t -> t) -> t -> t
  splitPlacer [] _ _ n      = n
  splitPlacer _ [] _ n      = n
  splitPlacer (l:ls) xs c n = let (x1, x2) = genericSplitAt l xs
                              in  x1 `c` splitPlacer ls x2 c n

-- | Split a list into chunks of the given lengths. Unlike
--   'splitPlaces', the output list will always be the same length as
--   the first input argument. If the input list is longer than the
--   total of the given lengths, then the remaining elements are
--   dropped. If the list is shorter than the total of the given
--   lengths, then the last several chunks will be shorter than
--   requested or empty. For example:
--
-- > splitPlacesBlanks [2,3,4] [1..20] == [[1,2],[3,4,5],[6,7,8,9]]
-- > splitPlacesBlanks [4,9] [1..10] == [[1,2,3,4],[5,6,7,8,9,10]]
-- > splitPlacesBlanks [4,9,3] [1..10] == [[1,2,3,4],[5,6,7,8,9,10],[]]
--
--   Notice the empty list in the output of the third example, which
--   differs from the behavior of 'splitPlaces'.
splitPlacesBlanks :: Integral a => [a] -> [e] -> [[e]]
splitPlacesBlanks is ys = build (splitPlacer is ys) where
  splitPlacer :: Integral i => [i] -> [b] -> ([b] -> t -> t) -> t -> t
  splitPlacer [] _ _ n      = n
  splitPlacer (l:ls) xs c n = let (x1, x2) = genericSplitAt l xs
                              in  x1 `c` splitPlacer ls x2 c n

-- | A useful recursion pattern for processing a list to produce a new
--   list, often used for \"chopping\" up the input list.  Typically
--   chop is called with some function that will consume an initial
--   prefix of the list and produce a value and the rest of the list.
--
--   For example, many common Prelude functions can be implemented in
--   terms of @chop@:
--
-- > group :: (Eq a) => [a] -> [[a]]
-- > group = chop (\ xs@(x:_) -> span (==x) xs)
-- >
-- > words :: String -> [String]
-- > words = filter (not . null) . chop (span (not . isSpace) . dropWhile isSpace)

chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
  where (b, as') = f as

-- | Divides up an input list into a set of sublists, according to 'n' and 'm'
--   input specifications you provide. Each sublist will have 'n' items, and the
--   start of each sublist will be offset by 'm' items from the previous one.
--
-- > divvy 5 5 [1..20] == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
--
--   In the case where a source list's trailing elements do no fill an entire
--   sublist, those trailing elements will be dropped.
--
-- > divvy 5 2 [1..10] == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
--
--   As an example, you can generate a moving average over a list of prices:
-- 
-- > type Prices = [Float]
-- > type AveragePrices = [Float]
-- > 
-- > average :: [Float] -> Float
-- > average xs = sum xs / (fromIntegral $ length xs)
-- > 
-- > simpleMovingAverage :: Prices -> AveragePrices
-- > simpleMovingAverage priceList =
-- >   map average divvyedPrices
-- >     where divvyedPrices = divvy 20 1 priceList

divvy :: Int -> Int -> [a] -> [[a]]
divvy _ _ [] = []
divvy n m lst = filter (\ws -> (n == length ws)) choppedl
  where choppedl = chop (\xs -> (take n xs , drop m xs)) lst

