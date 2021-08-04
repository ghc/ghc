{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import           Data.List.Split.Internals
import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Control.Monad
import           System.Environment
import           Text.Printf

import           Data.Char
import           Data.Functor
import           Data.List                 (genericTake, group, intercalate,
                                            isInfixOf, isPrefixOf, isSuffixOf,
                                            tails)
import           Data.Maybe                (isJust)

newtype Elt = Elt { unElt :: Char }
  deriving (Eq)

instance Show Elt where
  show (Elt c) = show c

instance Arbitrary Elt where
  arbitrary = elements (map Elt "abcde")

instance CoArbitrary Elt where
  coarbitrary = coarbitrary . ord . unElt

instance Function Elt where
  function = functionMap unElt Elt

deriving instance Show (Splitter Elt)

instance Show (Delimiter Elt) where
  show (Delimiter ps) = show (map function ps)

instance (Arbitrary a, CoArbitrary a, Function a) => Arbitrary (Delimiter a) where
  arbitrary = (Delimiter . map apply) <$> arbitrary

instance Arbitrary a => Arbitrary (Chunk a) where
  arbitrary = oneof [ liftM Text (listOf arbitrary)
                    , liftM Delim (listOf arbitrary)
                    ]

instance Arbitrary DelimPolicy where
  arbitrary = elements [Drop, Keep, KeepLeft, KeepRight]

instance Arbitrary CondensePolicy where
  arbitrary = elements [Condense, KeepBlankFields]

instance Arbitrary EndPolicy where
  arbitrary = elements [DropBlank, KeepBlank]

instance (Arbitrary a, CoArbitrary a, Function a) => Arbitrary (Splitter a) where
  arbitrary = liftM5 Splitter arbitrary arbitrary arbitrary arbitrary arbitrary

type Delim a = [Fun a Bool]

unDelim :: Delim a -> Delimiter a
unDelim = Delimiter . map apply

main :: IO ()
main = do
    results <- mapM (\(s,t) -> printf "%-40s" s >> t) tests
    when (not . all isSuccess $ results) $ fail "Not all tests passed!"
 where
    isSuccess (Success{}) = True
    isSuccess _ = False
    qc x = quickCheckWithResult (stdArgs { maxSuccess = 200 }) x
    tests = [ ("default/id",                    qc prop_default_id)
            , ("match/decompose",               qc prop_match_decompose)
            , ("match/yields delim",            qc prop_match_yields_delim)
            , ("splitInternal/lossless",        qc prop_splitInternal_lossless)
            , ("splitInternal/yields delims",   qc prop_splitInternal_yields_delims)
            , ("splitInternal/text",            qc prop_splitInternal_text_not_delims)
            , ("doCondense/no consec delims",   qc prop_doCondense_no_consec_delims)
            , ("insBlanks/no consec delims",    qc prop_insBlanks_no_consec_delims)
            , ("insBlanks/fl not delims",       qc prop_insBlanks_fl_not_delim)
            , ("mergeL/no delims",              qc prop_mergeL_no_delims)
            , ("mergeR/no delims",              qc prop_mergeR_no_delims)
            , ("oneOf",                         qc prop_oneOf)
            , ("oneOf/not text",                qc prop_oneOf_not_text)
            , ("onSublist",                     qc prop_onSublist)
            , ("onSublist/not text",            qc prop_onSublist_not_text)
            , ("whenElt",                       qc prop_whenElt)
            , ("whenElt/not text",              qc prop_whenElt_not_text)
            , ("process/dropDelims",            qc prop_dropDelims)
            , ("process/keepDelimsL no delims", qc prop_keepDelimsL_no_delims)
            , ("process/keepDelimsR no delims", qc prop_keepDelimsR_no_delims)
            , ("process/keepDelimsL match",     qc prop_keepDelimsL_match)
            , ("process/keepDelimsR match",     qc prop_keepDelimsR_match)
            , ("condense/no consec delims",     qc prop_condense_no_consec_delims)
            , ("condense/all delims",           qc prop_condense_all_delims)
            , ("dropInitBlank",                 qc prop_dropInitBlank)
            , ("dropFinalBlank",                qc prop_dropFinalBlank)
            , ("dropBlanks",                    qc prop_dropBlanks)
            , ("startsWith",                    qc prop_startsWith)
            , ("startsWithOneOf",               qc prop_startsWithOneOf)
            , ("endsWith",                      qc prop_endsWith)
            , ("endsWithOneOf",                 qc prop_endsWithOneOf)
            , ("splitOn/right inv",             qc prop_splitOn_right_inv)
            , ("splitOn/idem",                  qc prop_splitOn_intercalate_idem)
            , ("splitOn/empty delim",           qc prop_splitOn_empty_delim)
            , ("split/empty delim",             qc prop_split_empty_delim_drop)
            , ("chunksOf/lengths",              qc prop_chunksOf_all_n)
            , ("chunksOf/last <= n",            qc prop_chunksOf_last_less_n)
            , ("chunksOf/preserve",             qc prop_chunksOf_preserve)
            , ("splitPlaces/lengths",           qc prop_splitPlaces_lengths)
            , ("splitPlaces/last <= n",         qc prop_splitPlaces_last_less_n)
            , ("splitPlaces/preserve",          qc prop_splitPlaces_preserve)
            , ("splitPlaces/chunksOf",          qc prop_splitPlaces_chunksOf)
            , ("splitPlacesB/length",           qc prop_splitPlacesB_length)
            , ("splitPlacesB/last <= n",        qc prop_splitPlacesB_last_less_n)
            , ("splitPlacesB/preserve",         qc prop_splitPlacesB_preserve)
            , ("lines",                         qc prop_lines)
            , ("wordsBy/words",                 qc prop_wordsBy_words)
            , ("linesBy/lines",                 qc prop_linesBy_lines)
            , ("chop/group",                    qc prop_chop_group)
            , ("chop/words",                    qc prop_chop_words)
            , ("divvy/evenly",                  qc prop_divvy_evenly)
            , ("divvy/discard_remainder",  qc prop_divvy_discard_remainder)
            , ("divvy/outputlists_allsame_length", qc prop_divvy_outputlists_allsame_length)
            , ("divvy/output_are_sublists", qc prop_divvy_output_are_sublists)
            , ("divvy/heads", qc prop_divvy_heads)
            ]

prop_default_id :: [Elt] -> Bool
prop_default_id l = split defaultSplitter l == [l]

prop_match_decompose :: Delim Elt -> [Elt] -> Bool
prop_match_decompose d l = maybe True ((==l) . uncurry (++)) $ matchDelim (unDelim d) l

isDelimMatch :: Delim Elt -> [Elt] -> Bool
isDelimMatch d l = matchDelim (unDelim d) l == Just (l,[])

prop_match_yields_delim :: Delim Elt -> [Elt] -> Bool
prop_match_yields_delim d l =
    case matchDelim (unDelim d) l of
      Nothing -> True
      Just (del,rest) -> isDelimMatch d del

prop_splitInternal_lossless :: Delim Elt -> [Elt] -> Bool
prop_splitInternal_lossless d l = concatMap fromElem (splitInternal (unDelim d) l) == l

prop_splitInternal_yields_delims :: Delim Elt -> [Elt] -> Bool
prop_splitInternal_yields_delims d l =
    all (isDelimMatch d) $ [ del | (Delim del) <- splitInternal d' l ]
  where d' = unDelim d

prop_splitInternal_text_not_delims :: Delim Elt -> [Elt] -> Bool
prop_splitInternal_text_not_delims d l =
    all (not . isDelimMatch d) $ [ ch | (Text ch) <- splitInternal d' l ]
  where d' = unDelim d

noConsecDelims :: SplitList Elt -> Bool
noConsecDelims [] = True
noConsecDelims [x] = True
noConsecDelims (Delim _ : Delim _ : _) = False
noConsecDelims (_ : xs) = noConsecDelims xs

prop_doCondense_no_consec_delims :: SplitList Elt -> Bool
prop_doCondense_no_consec_delims l = noConsecDelims $ doCondense Condense l

prop_insBlanks_no_consec_delims :: SplitList Elt -> Bool
prop_insBlanks_no_consec_delims l = noConsecDelims $ insertBlanks Condense l

prop_insBlanks_fl_not_delim :: SplitList Elt -> Bool
prop_insBlanks_fl_not_delim l =
    case insertBlanks Condense l of
      [] -> True
      xs -> (not . isDelim $ head xs) && (not . isDelim $ last xs)

prop_mergeL_no_delims :: SplitList Elt -> Bool
prop_mergeL_no_delims = all (not . isDelim) . mergeLeft . insertBlanks Condense

prop_mergeR_no_delims :: SplitList Elt -> Bool
prop_mergeR_no_delims = all (not . isDelim) . mergeRight . insertBlanks Condense

getDelims :: Splitter Elt -> [Elt] -> [[Elt]]
getDelims s l = [ d | Delim d <- splitInternal (delimiter s) l ]

getTexts :: Splitter Elt -> [Elt] -> [[Elt]]
getTexts s l = [ c | Text c <- splitInternal (delimiter s) l ]

prop_oneOf :: [Elt] -> [Elt] -> Bool
prop_oneOf elts l = all ((==1) . length) ds && all ((`elem` elts) . head) ds
  where ds = getDelims (oneOf elts) l

prop_oneOf_not_text :: [Elt] -> [Elt] -> Bool
prop_oneOf_not_text elts l = all (not . (`elem` elts)) (concat cs)
  where cs = getTexts (oneOf elts) l

prop_onSublist :: [Elt] -> [Elt] -> Bool
prop_onSublist sub l = all (==sub) $ getDelims (onSublist sub) l

prop_onSublist_not_text :: [Elt] -> [Elt] -> Property
prop_onSublist_not_text sub l =
    (not . null $ sub) ==>
      all (not . isInfixOf sub) $ getTexts (onSublist sub) l

prop_whenElt :: (Fun Elt Bool) -> [Elt] -> Bool
prop_whenElt (Fun _ p) l = all ((==1) . length) ds && all (p . head) ds
  where ds = getDelims (whenElt p) l

prop_whenElt_not_text :: (Fun Elt Bool) -> [Elt] -> Bool
prop_whenElt_not_text (Fun _ p) l = all (not . p) (concat cs)
  where cs = getTexts (whenElt p) l

process :: Splitter Elt -> [Elt] -> SplitList Elt
process s = postProcess s . splitInternal (delimiter s)

prop_dropDelims :: Splitter Elt -> [Elt] -> Bool
prop_dropDelims s l = all (not . isDelim) (process (dropDelims s) l)

prop_keepDelimsL_no_delims :: Splitter Elt -> [Elt] -> Bool
prop_keepDelimsL_no_delims s l = all (not . isDelim) (process (keepDelimsL s) l)

prop_keepDelimsL_match :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_keepDelimsL_match s (NonEmpty l) =
  all (isJust . matchDelim (delimiter s)) [ c | Text c <- tail p ]
    where p = process (keepDelimsL s) l

prop_keepDelimsR_no_delims :: Splitter Elt -> [Elt] -> Bool
prop_keepDelimsR_no_delims s l = all (not . isDelim) (process (keepDelimsR s) l)

prop_keepDelimsR_match :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_keepDelimsR_match s (NonEmpty l) =
  all (any (isJust . matchDelim (delimiter s)) . tails)
    [ c | Text c <- init p ]
      where p = process (keepDelimsR s) l

prop_condense_no_consec_delims :: Splitter Elt -> [Elt] -> Bool
prop_condense_no_consec_delims s l = noConsecDelims $ process (condense s) l

prop_condense_all_delims :: Splitter Elt -> [Elt] -> Bool
prop_condense_all_delims s l = all allDelims p
  where p = [ d | Delim d <- process (condense s) l ]
        allDelims t = all isDelim (splitInternal (delimiter s) t)

prop_dropInitBlank :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_dropInitBlank s (NonEmpty l) = head p /= Text []
  where p = process (dropInitBlank $ s { delimPolicy = Keep } ) l

prop_dropFinalBlank :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_dropFinalBlank s (NonEmpty l) = last p /= Text []
  where p = process (dropFinalBlank $ s { delimPolicy = Keep } ) l

prop_dropBlanks :: Splitter Elt -> [Elt] -> Bool
prop_dropBlanks s = null . filter (== (Text [])) . process (dropBlanks s)

prop_startsWith :: [Elt] -> NonEmptyList Elt -> Bool
prop_startsWith s (NonEmpty l) = all (s `isPrefixOf`) (tail $ split (startsWith s) l)

prop_startsWithOneOf :: [Elt] -> NonEmptyList Elt -> Bool
prop_startsWithOneOf elts (NonEmpty l) = all ((`elem` elts) . head) (tail $ split (startsWithOneOf elts) l)

prop_endsWith :: [Elt] -> NonEmptyList Elt -> Bool
prop_endsWith s (NonEmpty l) = all (s `isSuffixOf`) (init $ split (endsWith s) l)

prop_endsWithOneOf :: [Elt] -> NonEmptyList Elt -> Bool
prop_endsWithOneOf elts (NonEmpty l) = all ((`elem` elts) . last) (init $ split (endsWithOneOf elts) l)

prop_splitOn_right_inv :: [Elt] -> [Elt] -> Bool
prop_splitOn_right_inv x l = intercalate x (splitOn x l) == l

{- This property fails: for example,

      splitOn "dd" (intercalate "dd" ["d",""]) == ["","d"]

   so it's not enough just to say that the delimiter is not an infix of
   any elements of l!


prop_splitOn_left_inv :: [Elt] -> NonEmptyList [Elt] -> Property
prop_splitOn_left_inv x (NonEmpty ls) = not (any (x `isInfixOf`) ls) ==>
                                        splitOn x (intercalate x ls) == ls
-}

-- Note, the below property is in fact logically entailed by
-- prop_splitOn_right_inv, but we keep it here just for kicks.
prop_splitOn_intercalate_idem :: [Elt] -> [[Elt]] -> Bool
prop_splitOn_intercalate_idem x ls = f (f ls) == f ls
  where f = splitOn x . intercalate x

prop_splitOn_empty_delim :: [Elt] -> Bool
prop_splitOn_empty_delim ls = splitOn [] ls == [] : map (:[]) ls

prop_split_empty_delim_drop :: [Elt] -> Bool
prop_split_empty_delim_drop ls
  = split (dropDelims . dropBlanks $ onSublist []) ls == map (:[]) ls

prop_chunksOf_all_n :: Positive Int -> NonEmptyList Elt -> Bool
prop_chunksOf_all_n (Positive n) (NonEmpty l) = all ((==n) . length) (init $ chunksOf n l)

prop_chunksOf_last_less_n :: Positive Int -> NonEmptyList Elt -> Bool
prop_chunksOf_last_less_n (Positive n) (NonEmpty l) = (<=n) . length . last $ chunksOf n l

prop_chunksOf_preserve :: Positive Int -> [Elt] -> Bool
prop_chunksOf_preserve (Positive n) l = concat (chunksOf n l) == l

prop_splitPlaces_lengths :: [NonNegative Int] -> [Elt] -> Bool
prop_splitPlaces_lengths ps = and . mInit . zipWith (==) ps' . map length . splitPlaces ps'
  where ps' = map unNN ps

prop_splitPlaces_last_less_n :: NonEmptyList (NonNegative Int) -> NonEmptyList Elt -> Bool
prop_splitPlaces_last_less_n (NonEmpty ps) (NonEmpty l) = (head $ drop (length l' - 1) ps') >= length (last l')
  where l' = splitPlaces ps' l
        ps' = map unNN ps

prop_splitPlaces_preserve :: [NonNegative Integer] -> [Elt] -> Bool
prop_splitPlaces_preserve ps l = concat (splitPlaces ps' l) == genericTake (sum ps') l
  where ps' = map unNN ps

prop_splitPlaces_chunksOf :: Positive Int -> [Elt] -> Bool
prop_splitPlaces_chunksOf (Positive n) l = splitPlaces (repeat n) l == chunksOf n l

prop_splitPlacesB_length :: [NonNegative Int] -> [Elt] -> Bool
prop_splitPlacesB_length ps xs = length ps' == length (splitPlacesBlanks ps' xs)
  where ps' = map unNN ps

prop_splitPlacesB_last_less_n :: NonEmptyList (NonNegative Int) -> NonEmptyList Elt -> Bool
prop_splitPlacesB_last_less_n (NonEmpty ps) (NonEmpty l) = (head $ drop (length l' - 1) ps') >= length (last l')
  where l' = splitPlacesBlanks ps' l
        ps' = map unNN ps

prop_splitPlacesB_preserve :: [NonNegative Integer] -> [Elt] -> Bool
prop_splitPlacesB_preserve ps l = concat (splitPlacesBlanks ps' l) == genericTake (sum ps') l
  where ps' = map unNN ps

unNN :: NonNegative a -> a
unNN (NonNegative x) = x

mInit :: [a] -> [a]
mInit [] = []
mInit [x] = []
mInit (x:xs) = x : init xs

newtype EltWS = EltWS { unEltWS :: Char }
  deriving (Eq, Show)

instance Arbitrary EltWS where
  arbitrary = elements (map EltWS "abcde \n")

prop_lines :: [EltWS] -> Bool
prop_lines s = lines s' == endBy "\n" s'
  where s' = map unEltWS s

prop_wordsBy_words :: [EltWS] -> Bool
prop_wordsBy_words s = words s' == wordsBy isSpace s'
  where s' = map unEltWS s

prop_linesBy_lines :: [EltWS] -> Bool
prop_linesBy_lines s = lines s' == linesBy (=='\n') s'
  where s' = map unEltWS s

prop_chop_group :: [Elt] -> Bool
prop_chop_group s = chop (\xs@(x:_) -> span (==x) xs) s == group s

prop_chop_words :: [EltWS] -> Bool
prop_chop_words s = words s' == (filter (not . null) . chop (span (not . isSpace) . dropWhile isSpace) $ s')
  where s' = map unEltWS s

prop_divvy_evenly :: [Elt] -> Positive Int -> Bool
prop_divvy_evenly elems (Positive n) = concat (divvy n n elems') == elems'
  where
    -- Chop off the smallest possible tail of elems to make the length
    -- evenly divisible by n.  This property used to have a
    -- precondition (length elemens `mod` n == 0), but that led to too
    -- many discarded test cases and occasional test suite failures.
    elems' = take ((length elems `div` n) * n) elems

prop_divvy_discard_remainder :: [Elt] -> Positive Int -> Bool
prop_divvy_discard_remainder elems (Positive n) =
  concat (divvy n n elems) == (reverse . drop (length elems `mod` n) . reverse $ elems)

prop_divvy_outputlists_allsame_length :: [Elt] -> Positive Int -> Positive Int -> Bool
prop_divvy_outputlists_allsame_length elems (Positive n) (Positive m) = allSame xs
  where
    allSame :: [Int] -> Bool
    allSame [] = True
    allSame zs = and $ map (== head zs) (tail zs)
    xs = map length (divvy n m elems)

prop_divvy_output_are_sublists :: [Elt] -> Positive Int -> Positive Int -> Bool
prop_divvy_output_are_sublists elems (Positive n) (Positive m) = and $ map (\x -> isInfixOf x elems) xs
  where xs = divvy n m elems

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n lst = (map head . chunksOf n) $ lst

initNth :: Int -> [a] -> [a]
initNth _ [] = []
initNth n lst = (reverse . drop n . reverse) $ lst

prop_divvy_heads :: [Elt] -> Positive Int -> Positive Int -> Bool
prop_divvy_heads [] _ _ = True
prop_divvy_heads elems (Positive n) (Positive m) = hds1 == hds2
  where hds1 = takeEvery m (initNth (n - 1) elems)
        hds2 = map head $ divvy n m elems

