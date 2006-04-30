#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.QuickCheck.Batch
import Test.QuickCheck
import Text.Show.Functions

import Data.List
import Data.Char
import Data.Word
import Data.Maybe

import Text.Printf

import System.Environment
import System.IO
import System.Random

import Data.ByteString.Char8 (ByteString, pack , unpack)
import qualified Data.ByteString.Char8 as P

instance Arbitrary Char where
  arbitrary = oneof $ map return
                (['a'..'z']++['A'..'Z']++['1'..'9']++['\n','\t','0','~','.',',','-','/'])
  coarbitrary c = variant (ord c `rem` 16)

instance Arbitrary Word8 where
  arbitrary = choose (minBound, maxBound)
  coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 16))

instance Random Word8 where
  randomR (a,b) g = case randomR (fromIntegral a :: Integer
                                 ,fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x :: Word8, g)

  random g        = randomR (minBound,maxBound) g

instance Arbitrary ByteString where
  arbitrary = P.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (P.unpack s)

------------------------------------------------------------------------
-- Simon's functions:

prop_pack string = string == P.unpack (P.pack string)

prop_nil1 = P.length P.empty == 0
prop_nil2 = P.unpack P.empty == ""

prop_cons c xs = P.unpack (P.cons c (P.pack xs)) == (c:xs)

prop_headS xs = not (P.null xs) ==> P.head xs == head (P.unpack xs)

prop_tailS xs = not (P.null xs) ==> P.tail xs == P.pack (tail (P.unpack xs))

prop_null xs = null (P.unpack xs) == P.null xs

prop_append xs ys = P.append xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_lengthS xs = length xs == P.length (P.pack xs)

prop_index xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.index` i
  where indices = choose (0, length xs -1)

prop_unsafeIndex xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_mapS f xs = P.map f (P.pack xs) == P.pack (map f xs)

prop_filter f xs = P.filter f (P.pack xs) == P.pack (filter f xs)

prop_reverseS xs = P.reverse (P.pack xs) == P.pack (reverse xs)

prop_concat xss = P.concat (map P.pack xss) == P.pack (concat xss)

prop_elemS x xs = P.elem x (P.pack xs) == elem x xs

prop_takeS i xs = P.take i (P.pack xs) == P.pack (take i xs)

prop_dropS i xs = P.drop i (P.pack xs) == P.pack (drop i xs)

prop_splitAtS i xs = collect (i >= 0 && i < length xs) $
    P.splitAt i (P.pack xs) ==
    let (a,b) = splitAt i xs in (P.pack a, P.pack b)

prop_foldl f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_foldr f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_takeWhileS f xs = P.takeWhile f (P.pack xs) == P.pack (takeWhile f xs)

prop_dropWhileS f xs = P.dropWhile f (P.pack xs) == P.pack (dropWhile f xs)

prop_spanS f xs = P.span f (P.pack xs) ==
    let (a,b) = span f xs in (P.pack a, P.pack b)

prop_breakS f xs = P.break f (P.pack xs) ==
    let (a,b) = break f xs in (P.pack a, P.pack b)

prop_linesS xs = P.lines (P.pack xs) == map P.pack (lines xs)

prop_unlinesS xss = P.unlines (map P.pack xss) == P.pack (unlines xss)

prop_wordsS xs = P.words (P.pack xs) == map P.pack (words xs)

prop_unwordsS xss = P.unwords (map P.pack xss) == P.pack (unwords xss)

prop_splitWith f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map P.length splits) == P.length xs - l2
  where splits = P.splitWith f xs
        l1 = length splits
        l2 = P.length (P.filter f xs)

prop_joinsplit c xs = P.join (P.pack [c]) (P.split c xs) == xs

prop_linessplit xs =
    (not . P.null) xs ==>
    P.lines' xs == P.split '\n' xs

prop_linessplit2 xs =
    P.lines xs == P.split '\n' xs ++ (if P.last xs == '\n' then [P.empty] else [])

prop_splitsplitWith c xs = P.split c xs == P.splitWith (== c) xs

prop_bijection  c = (P.w2c . P.c2w) c == id c
prop_bijection' w = (P.c2w . P.w2c) w == id w

------------------------------------------------------------------------
-- at first we just check the correspondence to List functions

prop_eq1 xs      = xs            == (unpack . pack $ xs)

prop_compare1 xs  = (pack xs         `compare` pack xs) == EQ
prop_compare2 xs  = (pack (xs++"X")  `compare` pack xs) == GT
prop_compare3 xs  = (pack xs  `compare` pack (xs++"X")) == LT
prop_compare4 xs  = (not (null xs)) ==> (pack xs  `compare` P.empty) == GT
prop_compare5 xs  = (not (null xs)) ==> (P.empty `compare` pack xs) == LT
prop_compare6 xs ys= (not (null ys)) ==> (pack (xs++ys)  `compare` pack xs) == GT

-- prop_nil1 xs = (null xs) ==> pack xs == P.empty
-- prop_nil2 xs = (null xs) ==> xs == unpack P.empty

prop_cons1 xs = 'X' : xs == unpack ('X' `P.cons` (pack xs))

prop_cons2 xs c = c : xs == unpack (c `P.cons` (pack xs))

prop_snoc1 xs c = xs ++ [c] == unpack ((pack xs) `P.snoc` c)

prop_head xs     = (not (null xs)) ==> head  xs  == (P.head . pack) xs
prop_head1 xs    = (not (null xs)) ==> head xs == (P.unsafeHead . pack) xs

prop_tail xs     = (not (null xs)) ==> tail xs    == (unpack . P.tail . pack) xs
prop_tail1 xs    = (not (null xs)) ==> tail xs    == (unpack . P.unsafeTail. pack) xs

prop_init xs     =
    (not (null xs)) ==>
    init xs    == (unpack . P.init . pack) xs

-- prop_null xs = (null xs) ==> null xs == (nullPS (pack xs))

prop_length xs = length xs == P.length (pack xs)

prop_append1 xs = (xs ++ xs) == (unpack $ pack xs `P.append` pack xs)
prop_append2 xs ys = (xs ++ ys) == (unpack $ pack xs `P.append` pack ys)

prop_map   xs = map toLower xs == (unpack . (P.map toLower) .  pack) xs

prop_filter1 xs   = (filter (=='X') xs) == (unpack $ P.filter (=='X') (pack xs))
prop_filter2 xs c = (filter (==c) xs) == (unpack $ P.filter (==c) (pack xs))

prop_find xs c = find (==c) xs == P.find (==c) (pack xs)

prop_foldl1 xs = ((foldl (\x c -> if c == 'a' then x else c:x) [] xs)) ==
                (unpack $ P.foldl (\x c -> if c == 'a' then x else c `P.cons` x) P.empty (pack xs))

prop_foldl2 xs = P.foldl (\xs c -> c `P.cons` xs) P.empty (pack xs) == P.reverse (pack xs)

prop_foldr1 xs = ((foldr (\c x -> if c == 'a' then x else c:x) [] xs)) ==
                (unpack $ P.foldr (\c x -> if c == 'a' then x else c `P.cons` x)
                    P.empty (pack xs))

prop_foldr2 xs = P.foldr (\c xs -> c `P.cons` xs) P.empty (pack xs) == (pack xs)

prop_foldl1_1 xs =
    (not . P.null) xs ==>
    P.foldl1 (\x c -> if c > x then c else x)      xs ==
    P.foldl  (\x c -> if c > x then c else x) '\0' xs

prop_foldl1_2 xs =
    (not . P.null) xs ==>
    P.foldl1 const xs == P.head xs

prop_foldl1_3 xs =
    (not . P.null) xs ==>
    P.foldl1 (flip const) xs == P.last xs

prop_foldr1_1 xs =
    (not . P.null) xs ==>
    P.foldr1 (\c x -> if c > x then c else x)      xs ==
    P.foldr  (\c x -> if c > x then c else x) '\0' xs

prop_foldr1_2 xs =
    (not . P.null) xs ==>
    P.foldr1 (flip const) xs == P.last xs

prop_foldr1_3 xs =
    (not . P.null) xs ==>
    P.foldr1 const xs == P.head xs

prop_takeWhile xs = (takeWhile (/= 'X') xs) == (unpack . (P.takeWhile (/= 'X')) . pack) xs

prop_dropWhile xs = (dropWhile (/= 'X') xs) == (unpack . (P.dropWhile (/= 'X')) . pack) xs

prop_take xs = (take 10 xs) == (unpack . (P.take 10) . pack) xs

prop_drop xs = (drop 10 xs) == (unpack . (P.drop 10) . pack) xs

prop_splitAt i xs = collect (i >= 0 && i < length xs) $
    splitAt i xs ==
    let (x,y) = P.splitAt i (pack xs) in (unpack x, unpack y)

prop_span xs = (span (/='X') xs) == (let (x,y) = P.span (/='X') (pack xs)
                                     in (unpack x, unpack y))

prop_break xs = (break (/='X') xs) == (let (x,y) = P.break (/='X') (pack xs)
                                       in (unpack x, unpack y))

prop_reverse xs = (reverse xs) == (unpack . P.reverse . pack) xs

prop_elem xs = ('X' `elem` xs) == ('X' `P.elem` (pack xs))

prop_notElem c xs = P.notElem c (P.pack xs) == notElem c xs

-- should try to stress it
prop_concat1 xs = (concat [xs,xs]) == (unpack $ P.concat [pack xs, pack xs])

prop_concat2 xs = (concat [xs,[]]) == (unpack $ P.concat [pack xs, pack []])

prop_any xs = (any (== 'X') xs) == (P.any (== 'X') (pack xs))
prop_all xs = (all (== 'X') xs) == (P.all (== 'X') (pack xs))

prop_lines xs = (lines xs) == ((map unpack) . P.lines . pack) xs

prop_unlines xs = (unlines.lines) xs == (unpack. P.unlines . P.lines .pack) xs

prop_words xs = (words xs) == ((map unpack) . P.words . pack) xs
prop_wordstokens xs = P.words xs == P.tokens isSpace xs

prop_unwords xs = (pack.unwords.words) xs == (P.unwords . P.words .pack) xs

prop_join xs = (concat . (intersperse "XYX") . lines) xs ==
               (unpack $ P.join (pack "XYX") (P.lines (pack xs)))

prop_elemIndex1 xs   = (elemIndex 'X' xs) == (P.elemIndex 'X' (pack xs))
prop_elemIndex2 xs c = (elemIndex c xs) == (P.elemIndex c (pack xs))

prop_lineIndices1 xs = P.elemIndices '\n' xs == P.lineIndices xs

prop_count c xs = length (P.elemIndices c xs) == P.count c xs

prop_elemIndexLast1 c xs = (P.elemIndexLast c (pack xs)) ==
                           (case P.elemIndex c (pack (reverse xs)) of
                                Nothing -> Nothing
                                Just i  -> Just (length xs -1 -i))

prop_elemIndexLast2 c xs = (P.elemIndexLast c (pack xs)) ==
                           ((-) (length xs - 1) `fmap` P.elemIndex c (pack $ reverse xs))

prop_elemIndices xs c = elemIndices c xs == P.elemIndices c (pack xs)

prop_findIndex xs = (findIndex (=='X') xs) == (P.findIndex (=='X') (pack xs))

prop_findIndicies xs c = (findIndices (==c) xs) == (P.findIndices (==c) (pack xs))

-- example properties from QuickCheck.Batch
prop_sort1 xs = sort xs == (unpack . P.sort . pack) xs
prop_sort2 xs = (not (null xs)) ==> (P.head . P.sort . pack $ xs) == minimum xs
prop_sort3 xs = (not (null xs)) ==> (P.last . P.sort . pack $ xs) == maximum xs
prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.head . P.sort) (P.append (pack xs) (pack ys)) == min (minimum xs) (minimum ys)
prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.last . P.sort) (P.append (pack xs) (pack ys)) == max (maximum xs) (maximum ys)

prop_intersperse c xs = (intersperse c xs) == (unpack $ P.intersperse c (pack xs))

prop_transpose xs = (transpose xs) == ((map unpack) . P.transpose . (map pack)) xs

prop_maximum xs = (not (null xs)) ==> (maximum xs) == (P.maximum ( pack xs ))
prop_minimum xs = (not (null xs)) ==> (minimum xs) == (P.minimum ( pack xs ))

------------------------------------------------------------------------

prop_dropSpace xs    = dropWhile isSpace xs == unpack (P.dropSpace (pack xs))
prop_dropSpaceEnd xs = (P.reverse . (P.dropWhile isSpace) . P.reverse) (pack xs) ==
                       (P.dropSpaceEnd (pack xs))

prop_breakSpace xs = (let (x,y) = P.breakSpace (pack xs)
                      in (unpack x, unpack y)) == (break isSpace xs)

prop_spanEnd xs =
        (P.spanEnd (not . isSpace) (pack xs)) ==
        (let (x,y) = P.span (not.isSpace) (P.reverse (pack xs)) in (P.reverse y,P.reverse x))

prop_breakChar c xs =
        (break (==c) xs) ==
        (let (x,y) = P.breakChar c (pack xs) in (unpack x, unpack y))

prop_breakFirst c xs = (let (x,y) = break (==c) xs
                        in if null y then Nothing
                                     else Just (pack x, pack $ drop 1 y)) ==
                       (P.breakFirst c (pack xs))

prop_breakLast c xs = (let (x,y) = break (==c) (reverse xs)
                       in if null y then Nothing
                                    else Just (pack (reverse $ drop 1 y), pack (reverse x))) ==
                       (P.breakLast c (pack xs))

prop_words' xs = (unpack . P.unwords  . P.words' . pack) xs ==
                 (map (\c -> if isSpace c then ' ' else c) xs)
prop_lines' xs = (unpack . P.unlines' . P.lines' . pack) xs == (xs)

prop_unfoldr c =
    (P.unfoldrN 100 (\x -> Just (x, chr (ord x + 1))) c) ==
    (pack $ take 100 $ unfoldr (\x -> Just (x, chr (ord x + 1))) c)

prop_prefix xs ys = isPrefixOf xs ys == (P.pack xs `P.isPrefixOf` P.pack ys)
prop_suffix xs ys = isSuffixOf xs ys == (P.pack xs `P.isSuffixOf` P.pack ys)

prop_copy xs = let p = P.pack xs in P.copy p == p

prop_inits xs = inits xs == map P.unpack (P.inits (P.pack xs))

prop_tails xs = tails xs == map P.unpack (P.tails (P.pack xs))

prop_findSubstrings s x l
    = P.findSubstrings (P.pack p) (P.pack s) == naive_findSubstrings p s
    where
    -- we look for some random substring of the test string
    p = take (abs l) $ drop (abs x) s
    -- naive reference implementation
    naive_findSubstrings :: String -> String -> [Int]
    naive_findSubstrings p s = [x | x <- [0..length s], p `isPrefixOf` drop x s]

prop_replicate1 n c =
    (n >= 0) ==>
    unpack (P.replicate n c) == replicate n c

prop_replicate2 n c =
    (n >= 0) ==>
    P.replicate n c == P.unfoldrN n (\u -> Just (u,u)) c

prop_replicate3 c = unpack (P.replicate 0 c) == replicate 0 c

prop_readint n = (fst . fromJust . P.readInt . pack . show) n == (n :: Int)

prop_readint2 s =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in P.readInt (pack s') == Nothing

prop_filterChar1 c xs = (filter (==c) xs) == ((P.unpack . P.filterChar c . P.pack) xs)
prop_filterChar2 c xs = (P.filter (==c) (P.pack xs)) == (P.filterChar c (P.pack xs))
prop_filterChar3 c xs = P.filterChar c xs == P.replicate (P.count c xs) c

prop_filterNotChar1 c xs = (filter (/=c) xs) == ((P.unpack . P.filterNotChar c . P.pack) xs)
prop_filterNotChar2 c xs = (P.filter (/=c) (P.pack xs)) == (P.filterNotChar c (P.pack xs))

prop_joinjoinpath xs ys = P.joinWithChar ' ' xs ys == P.join (P.packChar ' ') [xs,ys]

prop_zip  xs ys = zip xs ys == P.zip (pack xs) (pack ys)
prop_zip1 xs ys = P.zip xs ys == zip (P.unpack xs) (P.unpack ys)

prop_zipWith xs ys = P.zipWith (,) xs ys == P.zip xs ys

prop_unzip x = let (xs,ys) = unzip x in (pack xs, pack ys) == P.unzip x

------------------------------------------------------------------------

main = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-15s: " s >> a n) tests
  where
    tests = [    ("bijection",       mytest prop_bijection)
            ,    ("bijection'",       mytest prop_bijection')
            ,    ("eq1",       mytest prop_eq1)
            ,    ("compare1",       mytest prop_compare1)
            ,    ("compare2",       mytest prop_compare2)
            ,    ("compare3",       mytest prop_compare3)
            ,    ("compare4",       mytest prop_compare4)
            ,    ("compare5",       mytest prop_compare5)
            ,    ("compare6",       mytest prop_compare6)
            ,    ("cons1",       mytest prop_cons1)
            ,    ("cons2",       mytest prop_cons2)
            ,    ("snoc1",       mytest prop_snoc1)
            ,    ("head",       mytest prop_head)
            ,    ("head1",       mytest prop_head1)
            ,    ("tail",       mytest prop_tail)
            ,    ("tail1",       mytest prop_tail1)
            ,    ("init",       mytest prop_init)
            ,    ("length",       mytest prop_length)
            ,    ("append1",       mytest prop_append1)
            ,    ("append2",       mytest prop_append2)
            ,    ("map",       mytest prop_map)
            ,    ("filter1",       mytest prop_filter1)
            ,    ("filter2",       mytest prop_filter2)
            ,    ("foldl1",       mytest prop_foldl1)
            ,    ("foldl2",       mytest prop_foldl2)
            ,    ("foldr1",       mytest prop_foldr1)
            ,    ("foldr2",       mytest prop_foldr2)

            ,    ("foldl1_1",     mytest prop_foldl1_1)
            ,    ("foldl1_2",     mytest prop_foldl1_2)
            ,    ("foldl1_3",     mytest prop_foldl1_3)

            ,    ("foldr1_1",     mytest prop_foldr1_1)
            ,    ("foldr1_2",     mytest prop_foldr1_2)
            ,    ("foldr1_3",     mytest prop_foldr1_3)

            ,    ("all",       mytest prop_all)
            ,    ("take",       mytest prop_take)
            ,    ("drop",       mytest prop_drop)
            ,    ("takeWhile",       mytest prop_takeWhile)
            ,    ("dropWhile",       mytest prop_dropWhile)
            ,    ("splitAt",       mytest prop_splitAt)
            ,    ("span",       mytest prop_span)
            ,    ("break",       mytest prop_break)
            ,    ("reverse",       mytest prop_reverse)
            ,    ("elem",       mytest prop_elem)
            ,    ("notElem",       mytest prop_notElem)
            ,    ("concat1",       mytest prop_concat1)
            ,    ("concat2",       mytest prop_concat2)
            ,    ("any",       mytest prop_any)
            ,    ("lines",       mytest prop_lines)
            ,    ("unlines",       mytest prop_unlines)
            ,    ("words",       mytest prop_words)
            ,    ("unwords",       mytest prop_unwords)
            ,    ("join",       mytest prop_join)
            ,    ("elemIndex1",       mytest prop_elemIndex1)
            ,    ("elemIndex2",       mytest prop_elemIndex2)
            ,    ("findIndex",       mytest prop_findIndex)
            ,    ("findIndicies",       mytest prop_findIndicies)
            ,    ("elemIndices",       mytest prop_elemIndices)
            ,    ("find",       mytest prop_find)
            ,    ("sort1",       mytest prop_sort1)
            ,    ("sort2",       mytest prop_sort2)
            ,    ("sort3",       mytest prop_sort3)
            ,    ("sort4",       mytest prop_sort4)
            ,    ("sort5",       mytest prop_sort5)
            ,    ("intersperse",       mytest prop_intersperse)
            ,    ("maximum",       mytest prop_maximum)
            ,    ("minimum",       mytest prop_minimum)
            ,    ("breakChar",       mytest prop_breakChar)
            ,    ("breakSpace",       mytest prop_breakSpace)
            ,    ("dropSpace",       mytest prop_dropSpace)
            ,    ("spanEnd",       mytest prop_spanEnd)
            ,    ("breakFirst",       mytest prop_breakFirst)
            ,    ("breakLast",       mytest prop_breakLast)
            ,    ("elemIndexLast1",       mytest prop_elemIndexLast1)
            ,    ("elemIndexLast2",       mytest prop_elemIndexLast2)
            ,    ("words'",       mytest prop_words')
            ,    ("lines'",       mytest prop_lines')
            ,    ("dropSpaceEnd",       mytest prop_dropSpaceEnd)
            ,    ("unfoldr",       mytest prop_unfoldr)
            ,    ("prefix",       mytest prop_prefix)
            ,    ("suffix",       mytest prop_suffix)
            ,    ("copy",       mytest prop_copy)
            ,    ("inits",       mytest prop_inits)
            ,    ("tails",       mytest prop_tails)
            ,    ("findSubstrings",       mytest prop_findSubstrings)
            ,    ("replicate1",       mytest prop_replicate1)
            ,    ("replicate2",       mytest prop_replicate2)
            ,    ("replicate3",       mytest prop_replicate3)
            ,    ("readint",       mytest prop_readint)
            ,    ("readint2",       mytest prop_readint2)
            ,    ("filterChar1",       mytest prop_filterChar1)
            ,    ("filterChar2",       mytest prop_filterChar2)
            ,    ("filterChar3",       mytest prop_filterChar3)
            ,    ("filterNotChar1",       mytest prop_filterNotChar1)
            ,    ("filterNotChar2",       mytest prop_filterNotChar2)
            ,    ("pack",       mytest prop_pack)
            ,    ("nil1",       mytest prop_nil1)
            ,    ("nil2",       mytest prop_nil2)
            ,    ("cons",       mytest prop_cons)
            ,    ("headS",       mytest prop_headS)
            ,    ("lengthS",       mytest prop_lengthS)
            ,    ("tailS",       mytest prop_tailS)
            ,    ("null",       mytest prop_null)
            ,    ("append",       mytest prop_append)
            ,    ("index",       mytest prop_index)
            ,    ("unsafeIndex",       mytest prop_unsafeIndex)
            ,    ("mapS",       mytest prop_mapS)
            ,    ("filter",       mytest prop_filter)
            ,    ("reverseS",       mytest prop_reverseS)
            ,    ("concat",       mytest prop_concat)
            ,    ("elemS",       mytest prop_elemS)
            ,    ("takeS",       mytest prop_takeS)
            ,    ("dropS",       mytest prop_dropS)
            ,    ("splitAtS",       mytest prop_splitAtS)
            ,    ("foldl",       mytest prop_foldl)
            ,    ("foldr",       mytest prop_foldr)
            ,    ("takeWhileS",       mytest prop_takeWhileS)
            ,    ("dropWhileS",       mytest prop_dropWhileS)
            ,    ("spanS",       mytest prop_spanS)
            ,    ("breakS",       mytest prop_breakS)
            ,    ("linesS",       mytest prop_linesS)
            ,    ("unlinesS",       mytest prop_unlinesS)
            ,    ("wordsS",       mytest prop_wordsS)
            ,    ("unwordsS",       mytest prop_unwordsS)
            ,    ("wordstokens",       mytest prop_wordstokens)
            ,    ("splitWith",       mytest prop_splitWith)
            ,    ("joinsplit",       mytest prop_joinsplit)
            ,    ("lineIndices1",       mytest prop_lineIndices1)
            ,    ("count",       mytest prop_count)
            ,    ("linessplit",       mytest prop_linessplit)
            ,    ("splitsplitWith",       mytest prop_splitsplitWith)
            ,    ("joinjoinpath",       mytest prop_joinjoinpath)
            ,    ("zip",       mytest prop_zip)
            ,    ("zip1",       mytest prop_zip1)
            ,    ("zipWith",       mytest prop_zipWith)
            ,    ("unzip",       mytest prop_unzip)
            ]

mytest :: Testable a => a -> Int -> IO ()
mytest a n = mycheck defaultConfig{configMaxTest=n} a

mycheck :: Testable a => Config -> a -> IO ()
mycheck config a =
  do let rnd = mkStdGen 99
     mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
mytests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK," ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps =
  do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"

