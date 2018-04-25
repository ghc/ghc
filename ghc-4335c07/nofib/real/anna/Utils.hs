-- ==========================================================--
-- === Utilities                        File: utils.m (1) ===--
-- ==========================================================--

module Utils where
import MyUtils
import BaseDefs

-- ====================================--
-- === Haskell compatability        ===--
-- ====================================--


-- ==========================================================--
--
copy :: Int -> a -> [a]

copy n x = take (max 0 n) xs where xs = x:xs


-- ==========================================================--
--
sort :: (Ord a) =>  [a] -> [a]

sort [] = []
sort (a:x) = insert a (sort x)
             where
             insert :: (Ord a) => a -> [a] -> [a]
             insert a [] = [a]
             insert a (b:x) | a <=b       = a:b:x
                            | otherwise   = b:insert a x


-- ==========================================================--
--
layn :: [[Char]] -> [Char]

layn x =   f 1 x
           where
           f :: Int -> [[Char]] -> [Char]
           f n [] = []
           f n (a:x) = rjustify 4 (show n) ++") "++a++"\n"++f (n+1) x



-- ==========================================================--
--
rjustify :: Int -> [Char] -> [Char]
rjustify n s = spaces (n - length s)++s
               where
                  spaces :: Int -> [Char]
                  spaces m = copy m ' '


-- ==========================================================--
--
ljustify :: Int -> [Char] -> [Char]
ljustify n s = s ++ spaces (n - length s)
               where
                  spaces :: Int -> [Char]
                  spaces m = copy m ' '


-- ==========================================================--
--
utRandomInts :: Int -> Int -> [Int]

utRandomInts s1 s2
   = let seed1_ok = 1 <= s1 && s1 <= 2147483562
         seed2_ok = 1 <= s2 && s2 <= 2147483398

         rands :: Int -> Int -> [Int]
         rands s1 s2 
            = let k    = s1 `div` 53668
                  s1'  = 40014 * (s1 - k * 53668) - k * 12211
                  s1'' = if s1' < 0 then s1' + 2147483563 else s1'
                  k'   = s2 `div` 52774
                  s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
                  s2'' = if s2' < 0 then s2' + 2147483399 else s2'
                  z    = s1'' - s2''
              in  
                  if     z < 1 
                  then   z + 2147483562 : rands s1'' s2'' 
                  else   z : rands s1'' s2''
     in
         if     seed1_ok && seed2_ok 
         then   rands s1 s2 
         else   panic "utRandomInts: bad seeds"



-- ====================================--
-- === Projection functions for     ===--
-- === the static component         ===--
-- ====================================--

utSCdexprs :: StaticComponent -> DExprEnv
utSCdexprs (dexprs, domains, constrelems, freevars, flags, lims, sizes) 
   = dexprs

utSCdomains :: StaticComponent -> DSubst
utSCdomains (dexprs, domains, constrelems, freevars, flags, lims, sizes)
   = domains

utSCconstrelems :: StaticComponent -> AList Naam [ConstrElem]
utSCconstrelems (dexprs, domains, constrelems, freevars, flags, lims, sizes)
   = constrelems

utSCfreevars :: StaticComponent -> AList Naam [Naam]
utSCfreevars (dexprs, domains, constrelems, freevars, flags, lims, sizes)
   = freevars

utSCflags :: StaticComponent -> [Flag]
utSCflags (dexprs, domains, constrelems, freevars, flags, lims, sizes)
   = flags

utSClims :: StaticComponent -> (Int, Int, Int, Int, Int)
utSClims (dexprs, domains, constrelems, freevars, flags, lims, sizes)
   = lims

utSCsizes :: StaticComponent -> AList Domain Int
utSCsizes (dexprs, domains, constrelems, freevars, flags, lims, sizes)
   = sizes


-- ====================================--
-- === Association lists            ===--
-- ====================================--

-- ==========================================================--
--
utLookup []         k' = Nothing
utLookup ((k,v):bs) k' | k == k'   = Just v
                       | otherwise = utLookup bs k'


-- ==========================================================--
--
utSureLookup []         msg k' 
   = panic ( "utSureLookup: key not found in " ++ msg )
utSureLookup ((k,v):bs) msg k' 
   | k == k'     = v
   | otherwise   = utSureLookup bs msg k'


-- ==========================================================--
--
utLookupDef []         k' defawlt = defawlt
utLookupDef ((k,v):bs) k' defawlt | k == k'     = v
                                  | otherwise   = utLookupDef bs k' defawlt


-- ==========================================================--
--
utEmpty = []


-- ==========================================================--
--
utDomain al = map first al


-- ==========================================================--
--
utRange al = map second al


-- ==========================================================--
--
utLookupAll []         k' = []
utLookupAll ((k,v):bs) k' | k == k'     = v: utLookupAll bs k'
                          | otherwise   =    utLookupAll bs k'


-- ====================================--
-- === nameSupply                   ===--
-- ====================================--

-- ==========================================================--
--
utInitialNameSupply :: NameSupply

utInitialNameSupply = 0


-- ==========================================================--
--
utGetName :: NameSupply -> [Char] -> (NameSupply, [Char])

utGetName name_supply prefix 
   = (name_supply+1, utMakeName prefix name_supply)



-- ==========================================================--
--
utGetNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])

utGetNames name_supply prefixes 
  = (name_supply + length prefixes, 
     zipWith utMakeName prefixes (myIntsFrom name_supply))



-- ==========================================================--
--
utMakeName prefix ns = prefix ++ ")" ++ show ns



-- ====================================--
-- === iseq                         ===--
-- ====================================--

-- ==========================================================--
--
utiConcat :: [Iseq] -> Iseq

utiConcat = foldr utiAppend utiNil



-- ==========================================================--
--
utiInterleave :: Iseq -> [Iseq] -> Iseq

utiInterleave is []  = utiNil
utiInterleave is iss = foldl1 glue iss
                       where glue is1 is2 = is1 `utiAppend` (is `utiAppend` is2)
                             foldl1 f (x:xs) = foldl f x xs


-- ==========================================================--
--
utiLayn :: [Iseq] -> Iseq

utiLayn iss = utiLaynN 1 iss
              where
              utiLaynN :: Int -> [Iseq] -> Iseq
              utiLaynN n []       = utiNil
              utiLaynN n (is:isz) 
                = utiConcat [  (utiLjustify 4 (utiAppend (utiNum n) (utiStr ") "))), 
                               (utiIndent is),
                               (utiLaynN (n+1) isz)
                            ]


-- ==========================================================--
--
utiLjustify :: Int -> Iseq -> Iseq

utiLjustify n s 
   = s `utiAppend` (utiStr (utpspaces (n - length (utiMkStr s)) ""))



-- ==========================================================--
--
utiNum :: Int -> Iseq

utiNum = utiStr . show



-- ==========================================================--
--
utiFWNum :: Int -> Int -> Iseq

utiFWNum width n
 = utiStr (utpspaces spaces_reqd digits)
   where
   digits = show {-num-}  n
   spaces_reqd | length digits >= width   = 0
               | otherwise                = width - length digits


-- ====================================--
-- === oseq                         ===--
-- ====================================--

-- ==========================================================--
--
utoEmpty :: Oseq              -- An empty oseq

utoEmpty indent col = []


-- ==========================================================--
--
utoMkstr :: Oseq -> [Char]

utoMkstr oseq = oseq 0 0


-- ==========================================================--
--
utiNil = id


-- ==========================================================--
--
utiAppend = (.)


-- ==========================================================--
--
utiStr = foldr (utiAppend . utiChar) utiNil


-- ==========================================================--
--
utiMkStr iseq = utoMkstr (iseq utoEmpty)



-- ==========================================================--
--
utiChar :: Char -> Iseq

utiChar '\n' rest indent col = '\n' : rest indent 0
utiChar c    rest indent col 
   | col>=indent  = c   : rest indent (col+1)
   | otherwise    = utpspaces (indent - col) (c : rest indent (indent+1))


-- ==========================================================--
--
utiIndent iseq oseq indent col 
 = iseq oseq' (max col indent) col
   where 
   oseq' indent' col' = oseq indent col'
   -- Ignore the indent passed along to oseq; 
   -- use the original indent instead.



-- ==========================================================--
--
utpspaces :: Int -> [Char] -> [Char]
utpspaces n cs | n <= 0     = cs
               | otherwise  = ' ' : utpspaces (n-1) cs


-- ====================================--
-- === set                          ===--
-- ====================================--

-- ==========================================================--
--
--unMkSet :: (Ord a) => Set a -> [a]

unMkSet (MkSet s) = s


-- ==========================================================--
--
--utSetEmpty :: (Ord a) => Set a

utSetEmpty = MkSet []


-- ==========================================================--
--
--utSetIsEmpty :: (Ord a) => Set a -> Bool

utSetIsEmpty (MkSet s) = s == []


-- ==========================================================--
--
--utSetSingleton :: (Ord a) => a -> Set a

utSetSingleton x = MkSet [x]


-- ==========================================================--
--
--utSetFromList :: (Ord a) => [a] -> Set a

utSetFromList x = (MkSet . rmdup . sort) x
                  where rmdup []       = []
                        rmdup [x]      = [x]
                        rmdup (x:y:xs) | x==y       = rmdup (y:xs)
                                       | otherwise  = x: rmdup (y:xs)


-- ==========================================================--
--
--utSetToList :: (Ord a) => Set a -> [a]

utSetToList (MkSet xs) = xs


-- ==========================================================--
--
--utSetUnion :: (Ord a) => Set a -> Set a -> Set a

utSetUnion (MkSet [])     (MkSet [])            = (MkSet [])
utSetUnion (MkSet [])     (MkSet (b:bs))        = (MkSet (b:bs))
utSetUnion (MkSet (a:as)) (MkSet [])            = (MkSet (a:as))
utSetUnion (MkSet (a:as)) (MkSet (b:bs))
    | a < b   = MkSet (a: (unMkSet (utSetUnion (MkSet as) (MkSet (b:bs)))))
    | a == b  = MkSet (a: (unMkSet (utSetUnion (MkSet as) (MkSet bs))))
    | a > b   = MkSet (b: (unMkSet (utSetUnion (MkSet (a:as)) (MkSet bs))))


-- ==========================================================--
--
--utSetIntersection :: (Ord a) => Set a -> Set a -> Set a

utSetIntersection (MkSet [])     (MkSet [])     = (MkSet [])
utSetIntersection (MkSet [])     (MkSet (b:bs)) = (MkSet [])
utSetIntersection (MkSet (a:as)) (MkSet [])     = (MkSet [])
utSetIntersection (MkSet (a:as)) (MkSet (b:bs))
    | a < b   = utSetIntersection (MkSet as) (MkSet (b:bs))
    | a == b  = MkSet (a: (unMkSet (utSetIntersection (MkSet as) (MkSet bs))))
    | a > b   = utSetIntersection (MkSet (a:as)) (MkSet bs)


-- ==========================================================--
--
--utSetSubtraction :: (Ord a) => Set a -> Set a -> Set a

utSetSubtraction (MkSet [])     (MkSet [])      = (MkSet [])
utSetSubtraction (MkSet [])     (MkSet (b:bs))  = (MkSet [])
utSetSubtraction (MkSet (a:as)) (MkSet [])      = (MkSet (a:as))
utSetSubtraction (MkSet (a:as)) (MkSet (b:bs))  
    | a < b   = MkSet (a: (unMkSet (utSetSubtraction (MkSet as) (MkSet (b:bs)))))
    | a == b  = utSetSubtraction (MkSet as) (MkSet bs)
    | a > b   = utSetSubtraction (MkSet (a:as)) (MkSet bs)


-- ==========================================================--
--
--utSetElementOf :: (Ord a) => a -> Set a -> Bool

utSetElementOf x (MkSet [])       = False
utSetElementOf x (MkSet (y:ys))   = x==y || (x>y && utSetElementOf x (MkSet ys))


-- ==========================================================--
--
--utSetSubsetOf :: (Ord a) => Set a -> Set a -> Bool

utSetSubsetOf (MkSet [])        (MkSet bs) = True
utSetSubsetOf (MkSet (a:as))    (MkSet bs)
    = utSetElementOf a (MkSet bs) && utSetSubsetOf (MkSet as) (MkSet bs)


-- ==========================================================--
--
--utSetUnionList :: (Ord a) => [Set a] -> Set a

utSetUnionList setList = foldl utSetUnion utSetEmpty setList


-- ====================================--
-- === bag                          ===--
-- ====================================--

-- ==========================================================--
--
utBagUnion :: Bag a -> Bag a -> Bag a

utBagUnion as bs = as ++ bs


-- ==========================================================--
--
utBagInsert :: a -> Bag a -> Bag a

utBagInsert a as = a:as


-- ==========================================================--
--
utBagToList :: Bag a -> [a]

utBagToList xs   = xs


-- ==========================================================--
--
utBagFromList :: [a] -> Bag a

utBagFromList xs = xs


-- ==========================================================--
--
utBagSingleton :: a -> Bag a

utBagSingleton x = [x]


-- ==========================================================--
--
utBagEmpty :: Bag a

utBagEmpty = []


-- ====================================--
-- === Useful stuff                 ===--
-- ====================================--

-- ================================================--
--
splitList :: (a -> Bool) -> [a] -> ([a], [a])

splitList p []      = ([],[])
splitList p (x:xs)  = case splitList p xs of 
                        (ayes, noes) -> 
                          if p x then (x:ayes, noes) else (ayes, x:noes)



-- ================================================--
--
first (a,b) = a


-- ================================================--
--
second (a,b) = b


-- ================================================--
--
mapAccuml :: (a -> b -> (a, c)) -- Function of accumulator and element 
                                   --   input list, returning new
                                   --   accumulator and element of result list
             -> a                  -- Initial accumulator
             -> [b]               -- Input list
             -> (a, [c])         -- Final accumulator and result list

mapAccuml f acc []     = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x':xs')       
                         where (acc1, x')  = f acc x 
                               (acc2, xs') = mapAccuml f acc1 xs


-- ================================================--
--
unzip2 :: [(a,b)] -> ([a], [b])
unzip2 [] = ([],[])
unzip2 ((a,b):abs) = ( (a:as), (b:bs) )
                     where (as,bs) = unzip2 abs


-- ================================================--
--
map1st :: (a -> b) -> [(a,c)] -> [(b,c)]
map1st f [] = []
map1st f ((a,b):abs) = (f a,b): map1st f abs


-- ================================================--
--
map2nd :: (a -> b) -> [(c,a)] -> [(c,b)]
map2nd f [] = []
map2nd f ((a,b):abs) = (a,f b): map2nd f abs


-- ================================================--
--
interleave :: [a] -> [[a]] -> [a]

interleave e [] = []
interleave e [xs] = xs
interleave e (xs:xs2:xss) = xs ++ e ++ (interleave e (xs2:xss))


-- ====================================--
-- === State monad generics         ===--
-- ====================================--

returnS :: a -> ST a b
returnS a s0 = (a, s0)

thenS :: ST a c -> (a -> ST b c) -> ST b c
thenS m k s0 = case m s0 of (a, s1) -> k a s1

fetchS :: ST a a
fetchS s = (s, s)

assignS :: a -> ST () a
assignS snew s = ((), snew)

doStatefulOp1 :: (a -> ST b b) -> b -> a -> (b, b)
doStatefulOp1 f initState initValue1
   = f initValue1 initState

doStatefulOp2 :: (a -> b -> ST c d) -> d -> a -> b -> (c, d)
doStatefulOp2 f initState initValue1 initValue2
   = f initValue1 initValue2 initState


-- ==========================================================--
-- === End                                    utils.m (1) ===--
-- ==========================================================--
