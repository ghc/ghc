-----------------------------------------------------------------------------
-- Module      :  HughesPJQuickCheck
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
--
-- QuickChecks for HughesPJ pretty printer.
-- 
-- 1) Testing laws (blackbox)
--    - CDoc (combinator datatype)
-- 2) Testing invariants (whitebox)
-- 3) Testing bug fixes (whitebox)
--
-----------------------------------------------------------------------------
import PrettyTestVersion
import TestGenerators
import TestStructures

import UnitLargeDoc
import UnitPP1
import UnitT3911
import UnitT32

import Control.Monad
import Data.Char (isSpace)
import Data.List (intersperse)
import Debug.Trace

import Test.QuickCheck

main :: IO ()
main = do
    -- quickcheck tests
    check_laws
    check_invariants
    check_improvements
    check_non_prims -- hpc full coverage
    check_rendering
    check_list_def
    
    -- unit tests
    testPP1
    testT3911
    testT32
    testLargeDoc

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Utility functions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- tweaked to perform many small tests
myConfig :: Int -> Int -> Args
myConfig d n = stdArgs { maxSize = d, maxDiscardRatio = n*5 }

maxTests :: Int
maxTests = 1000

myTest :: (Testable a) => String -> a -> IO ()
myTest = myTest' 15 maxTests

myTest' :: (Testable a) => Int -> Int -> String -> a -> IO ()
myTest' d n msg t = do
    putStrLn (" * " ++ msg)
    r <- quickCheckWithResult (myConfig d n) t
    case r of
        (Failure {}) -> error "Failed testing!"
        _            -> return ()

myAssert :: String -> Bool -> IO ()
myAssert msg b = putStrLn $ (if b then "Ok, passed " else "Failed test:\n  ") ++ msg

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Quickcheck tests
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Equalities on Documents
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- compare text details
tdEq :: TextDetails -> TextDetails -> Bool
tdEq td1 td2 = (tdToStr td1) == (tdToStr td2)

-- algebraic equality on reduced docs
docEq :: RDoc () -> RDoc () -> Bool
docEq rd1 rd2 = case (rd1, rd2) of
    (Empty, Empty) -> True
    (NoDoc, NoDoc) -> True
    (NilAbove ds1, NilAbove ds2) -> docEq ds1 ds2
    (TextBeside td1 ds1, TextBeside td2 ds2) | annotToTd td1 `tdEq` annotToTd td2 -> docEq ds1 ds2
    (Nest k1 d1, Nest k2 d2) | k1 == k2 -> docEq d1 d2
    (Union d11 d12, Union d21 d22) -> docEq d11 d21 && docEq d12 d22
    (d1,d2) -> False
    
-- algebraic equality, with text reduction
deq :: Doc () -> Doc () -> Bool
deq d1 d2 = docEq (reduceDoc' d1) (reduceDoc' d2) where
    reduceDoc' = mergeTexts . reduceDoc
deqs :: [Doc ()] -> [Doc ()] -> Bool
deqs ds1 ds2 = 
    case zipE ds1 ds2 of
        Nothing    -> False
        (Just zds) -> all (uncurry deq) zds

        
zipLayouts :: Doc () -> Doc () -> Maybe [(Doc (),Doc ())]
zipLayouts d1 d2 = zipE (reducedDocs d1) (reducedDocs d2)
    where reducedDocs = map mergeTexts . flattenDoc

zipE :: [Doc ()] -> [Doc ()] -> Maybe [(Doc (), Doc ())]
zipE l1 l2 | length l1 == length l2 = Just $ zip l1 l2
           | otherwise              = Nothing

-- algebraic equality for layouts (without permutations)
lseq :: Doc () -> Doc () -> Bool
lseq d1 d2 = maybe False id . fmap (all (uncurry docEq)) $ zipLayouts d1 d2

-- abstract render equality for layouts
-- should only be performed if the number of layouts is reasonably small
rdeq :: Doc () -> Doc () -> Bool
rdeq d1 d2 = maybe False id . fmap (all (uncurry layoutEq)) $ zipLayouts d1 d2
    where layoutEq d1 d2 = (abstractLayout d1) == (abstractLayout d2)

layoutsCountBounded :: Int -> [Doc ()] -> Bool
layoutsCountBounded k docs = isBoundedBy k (concatMap flattenDoc docs)
  where
    isBoundedBy k [] = True
    isBoundedBy 0 (x:xs) = False
    isBoundedBy k (x:xs) = isBoundedBy (k-1) xs

layoutCountBounded :: Int -> Doc () -> Bool
layoutCountBounded k doc = layoutsCountBounded k [doc]

maxLayouts :: Int
maxLayouts = 64

infix 4 `deq`
infix 4 `lseq`
infix 4 `rdeq`

debugRender :: Int -> Doc () -> IO ()
debugRender k = putStr . visibleSpaces . renderStyle (Style PageMode k 1)
visibleSpaces = unlines . map (map visibleSpace) . lines

visibleSpace :: Char -> Char
visibleSpace ' ' = '.'
visibleSpace '.' = error "dot in visibleSpace (avoid confusion, please)"
visibleSpace  c  = c


-- (1) QuickCheck Properties: Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{-
Monoid laws for <>,<+>,$$,$+$
~~~~~~~~~~~~~
<a,b 1> (x * y) * z   = x * (y * z)
<a,b 2> empty * x     = x
<a,b 3> x * empty     = x
-}
prop_1 op x y z = classify (any isEmpty [x,y,z]) "empty x, y or z" $
                   ((x `op` y) `op` z) `deq` (x `op` (y `op` z))
prop_2 op x     = classify (isEmpty x) "empty" $ (empty `op` x) `deq` x
prop_3 op x     = classify (isEmpty x) "empty" $ x `deq` (empty `op` x)

check_monoid = do
    putStrLn " = Monoid Laws ="
    mapM_ (myTest' 5 maxTests "Associativity") [ liftDoc3 (prop_1 op) | op <- allops ]
    mapM_ (myTest "Left neutral") [ prop_2 op . buildDoc | op <- allops ]
    mapM_ (myTest "Right neutral") [ prop_3 op . buildDoc | op <- allops ]
    where
    allops = [ (<>), (<+>) ,($$) , ($+$) ]

{-
Laws for text
~~~~~~~~~~~~~
<t1>    text s <> text t        = text (s++t)
<t2>    text "" <> x            = x, if x non-empty [only true if x does not start with nest, because of <n6> ]
-}
prop_t1 s t = text' s <> text' t `deq` text (unText s ++  unText t)
prop_t2  x   = not (isEmpty x) ==> text "" <> x `deq` x
prop_t2_a x   = not (isEmpty x) && not (isNest x) ==> text "" <> x `deq` x

isNest :: Doc () -> Bool
isNest d = case reduceDoc d of
    (Nest _ _) -> True
    (Union d1 d2) -> isNest d1 || isNest d2
    _ -> False

check_t = do
    putStrLn " = Text laws ="
    myTest "t1" prop_t1
    myTest "t2_a (precondition: x does not start with nest)" (prop_t2_a . buildDoc)
    myTest "t_2 (Known to fail)" (expectFailure . prop_t2 . buildDoc)

{-
Laws for nest
~~~~~~~~~~~~~
<n1>    nest 0 x                = x
<n2>    nest k (nest k' x)      = nest (k+k') x
<n3>    nest k (x <> y)         = nest k z <> nest k y
<n4>    nest k (x $$ y)         = nest k x $$ nest k y
<n5>    nest k empty            = empty
<n6>    x <> nest k y           = x <> y, if x non-empty
-}
prop_n1 x      = nest 0 x                `deq` x
prop_n2 k k' x = nest k (nest k' x)      `deq` nest (k+k') x
prop_n3 k k' x  = nest k (nest k' x)      `deq` nest (k+k') x 
prop_n4 k x y  = nest k (x $$ y)         `deq` nest k x $$ nest k y
prop_n5 k     =  nest k empty            `deq` empty
prop_n6 x k y =  not (isEmpty x) ==>  
                 x <> nest k y           `deq` x <> y
check_n = do
    putStrLn "Nest laws"
    myTest "n1" (prop_n1 . buildDoc)
    myTest "n2" (\k k' -> prop_n2 k k' . buildDoc)
    myTest "n3" (\k k' -> prop_n3 k k' . buildDoc)
    myTest "n4" (\k -> liftDoc2 (prop_n4 k))
    myTest "n5" prop_n5
    myTest "n6" (\k -> liftDoc2 (\x -> prop_n6 x k))

{-
<m1>    (text s <> x) $$ y = text s <> ((text "" <> x)) $$ 
                                         nest (-length s) y)

<m2>    (x $$ y) <> z = x $$ (y <> z)
        if y non-empty
-}    
prop_m1 s x y = (text' s <> x) $$ y `deq` text' s <> ((text "" <> x) $$ 
                 nest (-length (unText s)) y)
prop_m2 x y z = not (isEmpty y) ==>
                (x $$ y) <> z      `deq` x $$ (y <> z)
check_m = do
    putStrLn "Misc laws"
    myTest "m1" (\s -> liftDoc2 (prop_m1 s))
    myTest' 10 maxTests "m2" (liftDoc3 prop_m2)


{-
Laws for list versions
~~~~~~~~~~~~~~~~~~~~~~
<l1>    sep (ps++[empty]++qs)   = sep (ps ++ qs)
        ...ditto hsep, hcat, vcat, fill...
[ Fails for fill ! ]
<l2>    nest k (sep ps) = sep (map (nest k) ps)
        ...ditto hsep, hcat, vcat, fill...
-}    
prop_l1 sp ps qs = 
    sp (ps++[empty]++qs)   `rdeq` sp (ps ++ qs)
prop_l2 sp k ps  = nest k (sep ps)        `deq` sep (map (nest k) ps)


prop_l1' sp cps cqs =
    let [ps,qs] = map buildDocList [cps,cqs] in 
    layoutCountBounded maxLayouts (sp (ps++qs)) ==> prop_l1 sp ps qs
prop_l2' sp k  ps = prop_l2 sp k (buildDocList ps)
check_l = do
    allCats $ myTest "l1" . prop_l1'
    allCats $ myTest "l2" . prop_l2'
    where
    allCats = flip mapM_ [ sep, hsep, cat, hcat, vcat, fsep, fcat ]
prop_l1_fail_1 = [ text "a" ]
prop_l1_fail_2 = [ text "a" $$  text "b" ]

{-
Laws for oneLiner
~~~~~~~~~~~~~~~~~
<o1>    oneLiner (nest k p) = nest k (oneLiner p)
<o2>    oneLiner (x <> y)   = oneLiner x <> oneLiner y 

[One liner only takes reduced arguments]
-}    
oneLinerR = oneLiner . reduceDoc
prop_o1 k p = oneLinerR (nest k p) `deq` nest k (oneLinerR p)
prop_o2 x y = oneLinerR (x <> y) `deq` oneLinerR x <> oneLinerR y 

check_o = do
    putStrLn "oneliner laws"
    myTest "o1 (RDoc arg)" (\k p -> prop_o1 k (buildDoc p))
    myTest "o2 (RDoc arg)" (liftDoc2 prop_o2)

{-
Definitions of list versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
<ldef1> vcat = foldr ($$) empty
<ldef2> hcat = foldr (<>) empty
<ldef3> hsep = foldr (<+>) empty
-}
prop_hcat :: [Doc ()] -> Bool
prop_hcat ds = hcat ds `deq` (foldr (<>) empty) ds

prop_hsep :: [Doc ()] -> Bool
prop_hsep ds = hsep ds `deq` (foldr (<+>) empty) ds

prop_vcat :: [Doc ()] -> Bool
prop_vcat ds = vcat ds `deq` (foldr ($$) empty) ds

{-
Update (pretty-1.1.0):
*failing* definition of sep: oneLiner (hsep ps) `union` vcat ps
<ldef4> ?
-}
prop_sep :: [Doc ()] -> Bool
prop_sep ds = sep ds `rdeq` (sepDef ds)

sepDef :: [Doc ()] -> Doc ()
sepDef docs = let ds = filter (not . isEmpty) docs in
              case ds of
                  [] -> empty
                  [d] -> d
                  ds -> reduceDoc (oneLiner (reduceDoc $ hsep ds) 
                                    `Union`
                                  (reduceDoc $ foldr ($+$) empty ds))

check_list_def = do 
    myTest "hcat def" (prop_hcat . buildDocList) 
    myTest "hsep def" (prop_hsep . buildDocList) 
    myTest "vcat def" (prop_vcat . buildDocList) 
    -- XXX: Not sure if this is meant to fail (I added the expectFailure [DT])
    myTest "sep def" (expectFailure . prop_sep . buildDocList)

{-
Definition of fill (fcat/fsep)
-- Specification: 
--   fill []  = empty
--   fill [p] = p
--   fill (p1:p2:ps) = oneLiner p1 <#> nest (length p1) 
--                                          (fill (oneLiner p2 : ps))
--                     `union`
--                      p1 $$ fill ps
-- Revised Specification:
--   fill g docs = fillIndent 0 docs
--
--   fillIndent k [] = []
--   fillIndent k [p] = p
--   fillIndent k (p1:p2:ps) =
--      oneLiner p1 <g> fillIndent (k + length p1 + g ? 1 : 0) (remove_nests (oneLiner p2) : ps)
--       `Union`
--      (p1 $*$ nest (-k) (fillIndent 0 ps)) 
--
-- $*$ is defined for layouts (not Docs) as 
-- layout1 $*$ layout2 | isOneLiner layout1 = layout1 $+$ layout2
--                     | otherwise          = layout1 $$ layout2
--
-- Old implementation ambiguities/problems:
-- ========================================
-- Preserving nesting:
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- fcat [cat[ text "b", text "a"], nest 2 ( text "" $$  text "a")]
-- ==> fcat [ text "b" $$ text "a", nest 2 (text "" $$ text "a")]   // cat: union right
-- ==> (text "b" $$ text "a" $$ nest 2 (text "" $$ text "a"))       // fcat: union right with overlap
-- ==> (text "ab" $$ nest 2 (text "" $$ text "a"))
-- ==> "b\na\n..a"
-- Bug #1337:
-- ~~~~~~~~~~
-- > fcat [ nest 1 $ text "a", nest 2 $ text "b", text "c"]
-- ==> [second alternative] roughly (a <#> b $#$ c)
-- " ab"
-- "c  "
-- expected: (nest 1; text "a"; text "b"; nest -3; "c")
-- actual  : (nest 1; text "a"; text "b"; nest -5; "c")
-- === (nest 1; text a) <> (fill (-2) (p2:ps))
-- ==>                     (nest 2 (text "b") $+$ text "c")    
-- ==>                     (nest 2 (text "b") `nilabove` nest (-3) (text "c"))
-- ==> (nest 1; text a; text b; nest -5 c)

-}
prop_fcat_vcat :: [Doc ()] -> Bool
prop_fcat_vcat ds = last (flattenDoc $ fcat ds) `deq` last (flattenDoc $ vcat ds)

prop_fcat :: [Doc ()] -> Bool
prop_fcat ds = fcat ds `rdeq` fillDef False (filter (not . isEmpty) ds)

prop_fsep :: [Doc ()] -> Bool
prop_fsep ds = fsep ds `rdeq` fillDef True (filter (not . isEmpty) ds)

prop_fcat_old :: [Doc ()] -> Bool
prop_fcat_old ds = fillOld2 False ds `rdeq` fillDef False (filter (not . isEmpty) ds)

prop_fcat_old_old :: [Doc ()] -> Bool
prop_fcat_old_old ds = fillOld2 False ds `rdeq` fillDefOld False (filter (not . isEmpty) ds)

prop_restrict_sz :: (Testable a) => Int -> ([Doc ()] -> a) -> ([Doc ()] -> Property) 
prop_restrict_sz k p ds = layoutCountBounded k (fsep ds) ==> p ds

prop_restrict_ol :: (Testable a) => ([Doc ()] -> a) -> ([Doc ()] -> Property)
prop_restrict_ol p ds = (all isOneLiner . map normalize $ ds) ==> p ds

prop_restrict_no_nest_start :: (Testable a) => ([Doc ()] -> a) -> ([Doc ()] -> Property)
prop_restrict_no_nest_start p ds = (all (not .isNest) ds) ==> p ds

fillDef :: Bool -> [Doc ()] -> Doc ()
fillDef g = normalize . fill' 0 . filter (not.isEmpty) . map reduceDoc
  where
    fill' _ [] = Empty
    fill' _ [x] = x    
    fill' k (p1:p2:ps) =
        reduceDoc (oneLiner p1 `append` (fill' (k + firstLineLength p1 + (if g then 1 else 0)) $ (oneLiner' p2) : ps))
            `union`
        reduceDoc (p1 $*$ (nest (-k) (fillDef g (p2:ps))))

    union = Union

    append = if g then (<+>) else (<>)    

    oneLiner' (Nest k d) = oneLiner' d
    oneLiner' d          = oneLiner d

($*$) :: RDoc () -> RDoc () -> RDoc ()
($*$) p ps = case flattenDoc p of
    [] -> NoDoc
    ls -> foldr1 Union (map combine ls) 
    where
    combine p | isOneLiner p = p $+$ ps
              | otherwise    = p $$  ps

fillDefOld :: Bool -> [Doc ()] -> Doc ()
fillDefOld g = normalize . fill' . filter (not.isEmpty) . map normalize where 
    fill' [] = Empty
    fill' [p1] = p1
    fill' (p1:p2:ps) = (normalize (oneLiner p1 `append` nest (firstLineLength p1) 
                                         (fill' (oneLiner p2 : ps))))
                    `union`
                     (p1 $$ fill' (p2:ps))
    append = if g then (<+>) else (<>)
    union = Union

check_fill_prop :: Testable a => String -> ([Doc ()] -> a) -> IO ()
check_fill_prop msg p = myTest msg (prop_restrict_sz maxLayouts p . buildDocList)

check_fill_def_fail :: IO ()
check_fill_def_fail = do 
    check_fill_prop "fcat defOld vs fcatOld (ol)" (prop_restrict_ol prop_fcat_old_old)
    check_fill_prop "fcat defOld vs fcatOld" prop_fcat_old_old

    check_fill_prop "fcat def (ol) vs fcatOld" (prop_restrict_ol prop_fcat_old)
    check_fill_prop "fcat def vs fcatOld" prop_fcat_old 

check_fill_def_ok :: IO ()
check_fill_def_ok = do
    check_fill_prop "fcat def (not nest start) vs fcatOld" (prop_restrict_no_nest_start prop_fcat_old)

    check_fill_prop "fcat def (not nest start) vs fcat" (prop_restrict_no_nest_start prop_fcat)
    -- XXX: These all fail now with the change of pretty to GHC behaviour.
    check_fill_prop "fcat def (ol) vs fcat" (expectFailure . prop_restrict_ol prop_fcat)
    check_fill_prop "fcat def vs fcat" (expectFailure . prop_fcat)
    check_fill_prop "fsep def vs fsep" (expectFailure . prop_fsep)


check_fill_def_laws :: IO ()
check_fill_def_laws = do
    check_fill_prop "lastLayout (fcat ps) == vcat ps" prop_fcat_vcat

check_fill_def :: IO ()
check_fill_def = check_fill_def_fail >> check_fill_def_ok
{-
text "ac"; nilabove; nest -1; text "a"; empty
text "ac"; nilabove; nest -2; text "a"; empty
-}

{-
Zero width text (Neil)

Here it would be convenient to generate functions (or replace empty / text bz z-w-t)
-}
-- TODO
{- 
All laws: monoid, text, nest, misc, list versions, oneLiner, list def
-}
check_laws :: IO ()
check_laws = do
    check_fill_def_ok
    check_monoid
    check_t
    check_n
    check_m
    check_l
    check_o
    check_list_def

-- (2) QuickCheck Properties: Invariants (whitebox)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- strategies: synthesize with stop condition
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stop :: a -> (a, Bool)
stop a = (a,False)

recurse :: a -> (a, Bool)
recurse a = (a,True)
-- strategy: generic synthesize with stop condition 
-- terms are combined top-down, left-right (latin text order)
genericProp :: (a -> a -> a) -> (Doc () -> (a,Bool)) -> Doc () -> a
genericProp c q doc =
    case q doc of
        (v,False) -> v
        (v,True)  -> foldl c v (subs doc)
    where
        rec = genericProp c q
        subs d = case d of
            Empty            -> []
            NilAbove d       -> [rec d]
            TextBeside _ d   -> [rec d]
            Nest _ d         -> [rec d]
            Union d1 d2      -> [rec d1, rec d2]
            NoDoc            -> []
            Beside d1 _ d2   -> subs (reduceDoc d)
            Above d1 _ d2    -> subs (reduceDoc d)


{-
 * The argument of NilAbove is never Empty. Therefore
    a NilAbove occupies at least two lines.
-}
prop_inv1 :: Doc () -> Bool
prop_inv1 d = genericProp (&&) nilAboveNotEmpty d where
    nilAboveNotEmpty (NilAbove Empty) = stop False
    nilAboveNotEmpty _ = recurse True

{-
  * The argument of @TextBeside@ is never @Nest@.  
-}
prop_inv2 :: Doc () -> Bool
prop_inv2 = genericProp (&&) textBesideNotNest where
    textBesideNotNest (TextBeside _ (Nest _ _)) = stop False
    textBesideNotNest _ = recurse True
{-
  * The layouts of the two arguments of @Union@ both flatten to the same 
    string 
-}
prop_inv3 :: Doc () -> Bool
prop_inv3 = genericProp (&&) unionsFlattenSame where
    unionsFlattenSame (Union d1 d2) = stop (pairwiseEqual (extractTexts d1 ++ extractTexts d2))
    unionsFlattenSame _ = recurse True
pairwiseEqual (x:y:zs) = x==y && pairwiseEqual (y:zs)
pairwiseEqual _ = True


{-
  * The arguments of @Union@ are either @TextBeside@, or @NilAbove@.
-}
prop_inv4 :: Doc () -> Bool
prop_inv4 = genericProp (&&) unionArgs where
    unionArgs (Union d1 d2) | goodUnionArg d1 && goodUnionArg d2 = recurse True
                            | otherwise = stop False
    unionArgs _ = recurse True
    goodUnionArg (TextBeside _ _) = True
    goodUnionArg (NilAbove _) = True
    goodUnionArg _ = False
  
{-
  * A @NoDoc@ may only appear on the first line of the left argument of
    an union. Therefore, the right argument of an union can never be equivalent
    to the empty set.
-}
prop_inv5 :: Doc () -> Bool
prop_inv5 = genericProp (&&) unionArgs . reduceDoc where
    unionArgs NoDoc = stop False
    unionArgs (Union d1 d2) = stop $ genericProp (&&) noDocIsFirstLine d1 && nonEmptySet (reduceDoc d2)
    unionArgs _ = (True,True) -- recurse
    noDocIsFirstLine (NilAbove d)    = stop $ genericProp (&&) unionArgs d
    noDocIsFirstLine _               = recurse True

{-
  * An empty document is always represented by @Empty@.  It can't be
    hidden inside a @Nest@, or a @Union@ of two @Empty@s.
-}
prop_inv6 :: Doc () -> Bool
prop_inv6 d | not (prop_inv1 d) || not (prop_inv2 d) = False
            | not (isEmptyDoc d) = True
            | otherwise = case d of Empty -> True ; _ -> False

isEmptyDoc :: Doc () -> Bool
isEmptyDoc d = case emptyReduction d of Empty -> True; _ -> False

{-
  * Consistency
  If all arguments of one of the list versions are empty documents, the list is an empty document
-}
prop_inv6a :: ([Doc ()] -> Doc ()) -> Property
prop_inv6a sep = forAll emptyDocListGen $
    \ds -> isEmptyRepr (sep $ buildDocList ds)
  where
      isEmptyRepr Empty = True
      isEmptyRepr _     = False

{-
  * The first line of every layout in the left argument of @Union@ is
    longer than the first line of any layout in the right argument.
    (1) ensures that the left argument has a first line.  In view of
    (3), this invariant means that the right argument must have at
    least two lines.
-}
counterexample_inv7 = cat [ text " ", nest 2 ( text "a") ]

prop_inv7 :: Doc () -> Bool
prop_inv7 = genericProp (&&) firstLonger where
    firstLonger (Union d1 d2) = (firstLineLength d1 >= firstLineLength d2, True)
    firstLonger _ = (True, True)

{- 
   * If we take as precondition: the arguments of cat,sep,fill do not start with Nest, invariant 7 holds
-}
prop_inv7_pre :: CDoc -> Bool
prop_inv7_pre cdoc = nestStart True cdoc where
  nestStart nestOk doc = 
    case doc of
        CList sep ds     -> all (nestStart False) ds
        CBeside _ d1 d2  -> nestStart nestOk d1 && nestStart (not . isEmpty $ buildDoc d1) d2
        CAbove _ d1 d2   -> nestStart nestOk d1 && nestStart (not . isEmpty $ buildDoc d1) d2
        CNest _ d  | not nestOk -> False
                   | otherwise  -> nestStart True d
        _empty_or_text   -> True

{-
   inv7_pre ==> inv7
-}
prop_inv7_a :: CDoc -> Property
prop_inv7_a cdoc = prop_inv7_pre cdoc ==> prop_inv7 (buildDoc cdoc)
    
check_invariants :: IO ()
check_invariants = do
    myTest "Invariant 1" (prop_inv1 . buildDoc)
    myTest "Invariant 2" (prop_inv2 . buildDoc)
    myTest "Invariant 3" (prop_inv3 . buildDoc)
    myTest "Invariant 4" (prop_inv4 . buildDoc)
    myTest "Invariant 5+" (prop_inv5 . buildDoc)
    myTest "Invariant 6" (prop_inv6 . buildDoc)
    mapM_ (\sp -> myTest "Invariant 6a" $ prop_inv6a sp) [ cat, sep, fcat, fsep, vcat, hcat, hsep ]
    -- XXX: Not sure if this is meant to fail (I added the expectFailure [DT])
    myTest "Invariant 7 (fails in HughesPJ:20080621)" (expectFailure . prop_inv7 . buildDoc)

-- `negative indent' 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

{-  
   In the documentation we have:
   
   (spaces n) generates a list of n spaces
   It should never be called with 'n' < 0, but that can happen for reasons I don't understand

   This is easy to explain:
   Suppose we have layout1 <> layout2
                   length of last line layout1 is k1
                   indentation of first line  of layout2 is k2
                   indentation of some other line of layout2 is k2'
   Now   layout1 <> nest k2 (line1 $$ nest k2' lineK)
    ==>  layout1 <> (line1 $$ nest k2' lineK)
   When k1 - k2' < 0, we need to layout lineK with negative indentation

   Here is a quick check property to ducment this.
-}
prop_negative_indent :: CDoc -> Property
prop_negative_indent cdoc = noNegNest cdoc ==> noNegSpaces (buildDoc cdoc)
noNegNest = genericCProp (&&) notIsNegNest where
    notIsNegNest (CNest k _) | k < 0 = stop False
    notIsNegNest  _                  = recurse True
noNegSpaces = go 0 . reduceDoc where 
    go k Empty = True
    go k (NilAbove d) = go k d
    go k (TextBeside _ d) | k < 0 = False
    go k (TextBeside s d) = go (k+annotSize s) d
    go k (Nest k' d) = go (k+k') d
    go k (Union d1 d2) = (if nonEmptySet d1 then (&&) (go k d1) else id) (go k d2)
    go k NoDoc = True

counterexample_fail9 :: Doc ()
counterexample_fail9 =  text "a" <> ( nest 2 ( text "b") $$  text "c")
-- reduces to           textb "a" ; textb "b" ; nilabove ; nest -3 ; textb "c" ; empty

{-
This cannot be fixed with violating the "intuitive property of layouts", described by John Hughes:
"Composing layouts should preserve the layouts themselves (i.e. translation)"

Consider the following example:
It is the user's fault to use <+> in t2.
-}

tstmt =  (nest 6 $ text "/* double indented comment */") $+$
         (nest 3 $ text "/* indented comment */") $+$
         text "skip;"

t1 = text "while(true)" $+$ (nest 2) tstmt
{-
while(true)
        /* double indented comment */
     /* indented comment */
  skip;
-}
t2 = text "while(true)" $+$ (nest 2 $ text "//" <+> tstmt)
{-
while(true)
  // /* double indented comment */
  /* indented comment */
skip;
-}
                        
-- (3) Touching non-prims
-- ~~~~~~~~~~~~~~~~~~~~~~

check_non_prims :: IO ()
check_non_prims = do
    myTest "Non primitive: show = renderStyle style" $ \cd -> let d = buildDoc cd in 
        show ((zeroWidthText "a") <> d) /= renderStyle style d
    myAssert "symbols" $
        (semi <> comma <> colon <> equals <> lparen <> rparen <> lbrack <> rbrack <> lbrace <> rbrace)
            `deq` 
        (text ";,:=()[]{}")
    myAssert "quoting" $
        (quotes . doubleQuotes . parens . brackets .braces $ (text "a" $$ text "b"))
            `deq`
        (text "'\"([{" <> (text "a" $$ text "b") <> text "}])\"'")
    myAssert "numbers" $
        fsep [int 42, integer 42, float 42, double 42, rational 42]
        `rdeq`
        (fsep . map text) 
            [show (42 :: Int), show (42 :: Integer), show (42 :: Float), show (42 :: Double), show (42 :: Rational)]
    myTest "Definition of <+>" $ \cd1 cd2 -> 
        let (d1,d2) = (buildDoc cd1, buildDoc cd2) in 
        layoutsCountBounded maxLayouts [d1,d2] ==>
        not (isEmpty d1) && not (isEmpty d2)   ==>
        d1 <+> d2 `rdeq` d1 <> space <> d2 
        
    myTest "hang" $ liftDoc2 (\d1 d2 -> hang d1 2 d2 `deq` sep [d1, nest 2 d2])
    
    let pLift f cp cds = f (buildDoc cp) (buildDocList cds)
    myTest "punctuate" $ pLift (\p ds -> (punctuate p ds) `deqs` (punctuateDef p ds))

check_rendering = do
    myTest' 20 10000 "one - line rendering" $ \cd -> 
        let d = buildDoc cd in        
        (renderStyle (Style OneLineMode undefined undefined) d) == oneLineRender d
    myTest' 20 10000 "left-mode rendering" $ \cd ->
        let d = buildDoc cd in
        extractText (renderStyle (Style LeftMode undefined undefined) d) == extractText (oneLineRender d)
    myTest' 20 10000 "page mode rendering" $ \cd ->
        let d = buildDoc cd in
        extractText (renderStyle (Style PageMode 6 1.7) d) == extractText (oneLineRender d)
    myTest' 20 10000 "zigzag mode rendering" $ \cd ->
        let d = buildDoc cd in
        extractTextZZ (renderStyle (Style ZigZagMode 6 1.7) d) == extractText (oneLineRender d)
        
extractText :: String -> String
extractText = filter (not . isSpace)

extractTextZZ :: String -> String
extractTextZZ = filter (\c -> not (isSpace c) && c /= '/' && c /= '\\')

punctuateDef :: Doc () -> [Doc ()] -> [Doc ()]
punctuateDef p [] = []
punctuateDef p ps = 
    let (dsInit,dLast) = (init ps, last ps) in
    map (\d -> d <> p) dsInit ++ [dLast]
       
-- (4) QuickChecking improvments and bug fixes
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{-
putStrLn $ render' $ fill True [ text "c", text "c",empty, text "c", text "b"]
c c c
b
putStrLn $ render' $ fillOld True [ text "c", text "c",empty, text "c", text "b"]
c c c
    b
-}
prop_fill_empty_reduce :: [Doc ()] -> Bool
prop_fill_empty_reduce ds = fill True ds `deq` fillOld True (filter (not.isEmpty.reduceDoc) ds)

check_improvements :: IO ()
check_improvements = do
    myTest "fill = fillOld . filter (not.isEmpty) [if no argument starts with nest]" 
           (prop_fill_empty_reduce . filter (not .isNest) . buildDocList)

-- old implementation of fill
fillOld :: Bool -> [Doc ()] -> RDoc ()
fillOld _ []     = empty
fillOld g (p:ps) = fill1 g (reduceDoc p) 0 ps where
    fill1 :: Bool -> RDoc () -> Int -> [Doc ()] -> Doc ()
    fill1 _ _                   k _  | k `seq` False = undefined
    fill1 _ NoDoc               _ _  = NoDoc
    fill1 g (p `Union` q)       k ys = fill1 g p k ys
                                       `union_`
                                       (aboveNest q False k (fillOld g ys))

    fill1 g Empty               k ys = mkNest k (fillOld g ys)
    fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)

    fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fillOld g ys))
    fill1 g (TextBeside s p)    k ys = textBeside_ s (fillNB g p (k - annotSize s) ys)
    fill1 _ (Above {})          _ _  = error "fill1 Above"
    fill1 _ (Beside {})         _ _  = error "fill1 Beside"
        -- fillNB gap textBesideArgument space_left docs
    fillNB :: Bool -> Doc () -> Int -> [Doc ()] -> Doc ()
    fillNB _ _           k _  | k `seq` False = undefined
    fillNB g (Nest _ p)  k ys  = fillNB g p k ys
    fillNB _ Empty _ []        = Empty
    fillNB g Empty k (y:ys)    = nilBeside g (fill1 g (oneLiner (reduceDoc y)) k1 ys)
                                 `mkUnion` 
                                 nilAboveNest False k (fillOld g (y:ys))
                               where
                                 k1 | g         = k - 1
                                    | otherwise = k
    fillNB g p k ys            = fill1 g p k ys


-- Specification: 
--   fill []  = empty
--   fill [p] = p
--   fill (p1:p2:ps) = oneLiner p1 <#> nest (length p1) 
--                                          (fill (oneLiner p2 : ps))
--                     `union`
--                      p1 $$ fill ps
fillOld2 :: Bool -> [Doc ()] -> RDoc ()
fillOld2 _ []     = empty
fillOld2 g (p:ps) = fill1 g (reduceDoc p) 0 ps where
    fill1 :: Bool -> RDoc () -> Int -> [Doc ()] -> Doc ()
    fill1 _ _                   k _  | k `seq` False = undefined
    fill1 _ NoDoc               _ _  = NoDoc
    fill1 g (p `Union` q)       k ys = fill1 g p k ys
                                       `union_`
                                       (aboveNest q False k (fill g ys))

    fill1 g Empty               k ys = mkNest k (fill g ys)
    fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)

    fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
    fill1 g (TextBeside s p)    k ys = textBeside_ s (fillNB g p (k - annotSize s) ys)
    fill1 _ (Above {})          _ _  = error "fill1 Above"
    fill1 _ (Beside {})         _ _  = error "fill1 Beside"

    fillNB :: Bool -> Doc () -> Int -> [Doc ()] -> Doc ()
    fillNB _ _           k _  | k `seq` False = undefined
    fillNB g (Nest _ p)  k ys  = fillNB g p k ys
    fillNB _ Empty _ []        = Empty
    fillNB g Empty k (Empty:ys)  = fillNB g Empty k ys
    fillNB g Empty k (y:ys)    = fillNBE g k y ys
    fillNB g p k ys            = fill1 g p k ys

    fillNBE g k y ys           = nilBeside g (fill1 g (oneLiner (reduceDoc y)) k1 ys)
                                 `mkUnion` 
                                 nilAboveNest True k (fill g (y:ys))
                               where
                                 k1 | g         = k - 1
                                    | otherwise = k

-- (5) Pretty printing RDocs and RDOC properties
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prettyDoc :: Doc () -> Doc ()
prettyDoc d = 
    case reduceDoc d of 
        Empty            -> text "empty"
        NilAbove d       -> (text "nilabove") <> semi <+> (prettyDoc d)
        TextBeside s d   -> (text ("text \""++tdToStr (annotToTd s) ++ "\"" ++ show (annotSize s))) <> semi <+> (prettyDoc d)
        Nest k d           -> text "nest" <+> integer (fromIntegral k) <> semi <+> prettyDoc d
        Union d1 d2        -> sep [text "union", parens (prettyDoc d1), parens (prettyDoc d2)]
        NoDoc              -> text "nodoc"

-- TODO: map strategy for Docs to avoid code duplication
-- Debug: Doc -> [Layout]
flattenDoc :: Doc () -> [RDoc ()]
flattenDoc d = flatten (reduceDoc d) where
    flatten NoDoc = []
    flatten Empty = return Empty
    flatten (NilAbove d) = map NilAbove (flatten d)
    flatten (TextBeside s d) = map (TextBeside s) (flatten d)
    flatten (Nest k d) = map (Nest k) (flatten d)
    flatten (Union d1 d2) = flattenDoc d1 ++ flattenDoc d2
    flatten (Beside d1 b d2) = error $ "flattenDoc Beside"
    flatten (Above d1 b d2) = error $ "flattenDoc Above"
  
normalize :: Doc () -> RDoc ()
normalize d = norm d where
    norm NoDoc = NoDoc
    norm Empty = Empty
    norm (NilAbove d) = NilAbove (norm d)
    norm (TextBeside s (Nest k d)) = norm (TextBeside s d)
    norm (TextBeside s d) = (TextBeside s) (norm d)
    norm (Nest k (Nest k' d)) = norm $ Nest (k+k') d
    norm (Nest 0 d) = norm d
    norm (Nest k d) = (Nest k) (norm d)  
    --   * The arguments of @Union@ are either @TextBeside@, or @NilAbove@.
    norm (Union d1 d2) = normUnion (norm d1) (norm d2)
    norm d@(Beside d1 b d2) = norm (reduceDoc d)
    norm d@(Above d1 b d2) = norm (reduceDoc d)
    normUnion d0@(Nest k d) (Union d1 d2) = norm (Union d0 (normUnion d1 d2))
    normUnion (Union d1 d2) d3@(Nest k d) = norm (Union (normUnion d1 d2) d3)
    normUnion (Nest k d1) (Nest k' d2) | k == k' = Nest k $ Union (norm d1) (norm d2)
                                       | otherwise = error "normalize: Union Nest length mismatch ?"
    normUnion (Nest _ _) d2 = error$ "normUnion Nest "++topLevelCTor d2
    normUnion d1 (Nest _ _) = error$ "normUnion Nset "++topLevelCTor d1
    normUnion p1 p2  = Union p1 p2

topLevelCTor :: Doc () -> String
topLevelCTor d = tlc d where
    tlc NoDoc = "NoDoc"
    tlc Empty = "Empty"
    tlc (NilAbove d) = "NilAbove"
    tlc (TextBeside s d) = "TextBeside"
    tlc (Nest k d) = "Nest"
    tlc (Union d1 d2) = "Union"
    tlc (Above _ _ _) = "Above"
    tlc (Beside _ _ _) = "Beside"
    
-- normalize TextBeside (and consequently apply some laws for simplification)
mergeTexts :: RDoc () -> RDoc ()
mergeTexts = merge where
    merge NoDoc = NoDoc
    merge Empty = Empty
    merge (NilAbove d) = NilAbove (merge d)
    merge (TextBeside t1 (TextBeside t2 doc)) = (merge.normalize) (TextBeside (mergeText t1 t2) doc)
    merge (TextBeside s d) = TextBeside s (merge d)
    merge (Nest k d) = Nest k (merge d)
    merge (Union d1 d2) = Union (merge d1) (merge d2)
    mergeText t1 t2 =
      NoAnnot (Str $ tdToStr (annotToTd t1) ++ tdToStr (annotToTd t2))
              (annotSize t1 + annotSize t2)
    
isOneLiner :: RDoc () -> Bool
isOneLiner = genericProp (&&) iol where
    iol (NilAbove _) = stop False
    iol (Union _ _)  = stop False
    iol  NoDoc = stop False
    iol _ = recurse True

hasOneLiner :: RDoc () -> Bool
hasOneLiner = genericProp (&&) iol where
    iol (NilAbove _) = stop False
    iol (Union d1 _) = stop $ hasOneLiner d1
    iol  NoDoc = stop False
    iol _ = recurse True

-- use elementwise concatenation as generic combinator
extractTexts :: Doc () -> [String]
extractTexts = map normWS . genericProp combine go where
    combine xs ys = [ a ++ b | a <- xs, b <- ys ]
    go (TextBeside s _ )   = recurse [tdToStr (annotToTd s)]
    go (Union d1 d2)       = stop $ extractTexts d1 ++ extractTexts d2
    go NoDoc               = stop []
    go _ = recurse [""]
    -- modulo whitespace
    normWS txt = filter (not . isWS) txt where
        isWS ws | ws == ' ' || ws == '\n' || ws == '\t'  = True
                | otherwise = False 
                
emptyReduction :: Doc () -> Doc ()
emptyReduction doc = 
    case doc of
            Empty             -> Empty
            NilAbove d        -> case emptyReduction d of Empty -> Empty ; d' -> NilAbove d'
            TextBeside s d    -> TextBeside s (emptyReduction d)
            Nest k d          -> case emptyReduction d of Empty -> Empty; d -> Nest k d
            Union d1 d2       -> case emptyReduction d2 of Empty -> Empty; _ -> Union d1 d2 -- if d2 is empty, both have to be
            NoDoc             -> NoDoc
            Beside d1 _ d2    -> emptyReduction (reduceDoc doc)
            Above d1 _ d2     -> emptyReduction (reduceDoc doc)

firstLineLength :: Doc () -> Int
firstLineLength = genericProp (+) fll . reduceDoc where
    fll (NilAbove d) = stop 0
    fll (TextBeside s d) = recurse (annotSize s)
    fll (Nest k d) = recurse k
    fll (Union d1 d2) = stop (firstLineLength d1) -- inductively assuming inv7
    fll (Above _ _ _) = error "Above"
    fll (Beside _ _ _) = error "Beside"
    fll _ = (0,True)

abstractLayout :: Doc () -> [(Int,String)]
abstractLayout d = cal 0 Nothing (reduceDoc d) where
    --   current column -> this line -> doc -> [(indent,line)]
    cal :: Int -> (Maybe (Int,String)) -> Doc () -> [(Int,String)]
    cal k cur Empty = [ addTextEOL k (Str "") cur ]    
    cal k cur (NilAbove d) = (addTextEOL k (Str "") cur) : cal k Nothing d
    cal k cur (TextBeside s d) = cal (k + annotSize s) (addText k s cur) d
    cal k cur (Nest n d) = cal (k+n) cur d
    cal _ _ (Union d1 d2) = error "abstractLayout: Union"
    cal _ _ NoDoc = error "NoDoc"
    cal _ _ (Above _ _ _) = error "Above"
    cal _ _ (Beside _ _ _) = error "Beside"
    addTextEOL k str Nothing = (k,tdToStr str)
    addTextEOL _ str (Just (k,pre)) = (k,pre++ tdToStr str)
    addText k str = Just . addTextEOL k (annotToTd str)

docifyLayout :: [(Int,String)] -> Doc ()
docifyLayout = vcat . map (\(k,t) -> nest k (text t))
    
oneLineRender :: Doc () -> String
oneLineRender = olr . abstractLayout . last . flattenDoc where
    olr = concat . intersperse " " . map snd

-- because of invariant 4, we do not have to expand to layouts here
-- but it is easier, so for now we use abstractLayout
firstLineIsLeftMost :: Doc () -> Bool
firstLineIsLeftMost = all (firstIsLeftMost . abstractLayout) . flattenDoc where
    firstIsLeftMost ((k,_):xs@(_:_)) = all ( (>= k) . fst) xs
    firstIsLeftMost _ = True

noNegativeIndent :: Doc () -> Bool
noNegativeIndent = all (noNegIndent . abstractLayout) . flattenDoc where
    noNegIndent = all ( (>= 0) . fst)
    
