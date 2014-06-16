{- Test code for Data.HashTable -}

module Main(main) where

import Prelude hiding (lookup)
import qualified Prelude (lookup)
import Data.Maybe(isJust,isNothing)
import Data.Int(Int32)
import Test.QuickCheck
import System.IO.Unsafe(unsafePerformIO)
import Data.HashTab
import Control.Monad(liftM2, foldM)
import System.Random
import System.Environment

infixr 0 ==.
infixr 0 ==~
infixr 0 ~~

type HT = HashTable Int Int
newtype HashFun = HF {unHF :: (Int -> Int32)}
data Empty = E {e :: (IO HT), hfe :: HashFun}
data MkH   = H {h :: (IO HT), hfh :: HashFun}
newtype List a = L [a]

data Action = Lookup Int
            | Insert Int Int
            | Delete Int
            | Update Int Int
            deriving (Show)

instance Arbitrary Action where
  arbitrary = frequency [(10,fmap Lookup arbitrary),
                         (5, liftM2 Insert arbitrary arbitrary),
                         (3, liftM2 Update arbitrary arbitrary),
                         (1, fmap Delete arbitrary)]
  coarbitrary = error "coarbitrary Action"

simA :: [Action] -> [Either Bool [Int]]
simA = fst . foldl sim ([],[])
  where sim :: ([Either Bool [Int]], [Action]) -> Action ->
               ([Either Bool [Int]], [Action])
        sim (res, past) (Lookup k)   = (Right (lkup k past) : res, past)
        sim (res, past) (Insert k v) = (res, Insert k v : past)
        sim (res, past) (Delete k)   = (res, Delete k   : past)
        sim (res, past) (Update k v) =
          (Left (not (null l)) : res, Update k v : past)
          where l = lkup k past
        lkup _ [] = []
        lkup k (Delete k' : _)
          | k==k' = []
        lkup k (Update k' v : _)
          | k==k'     = [v]
        lkup k (Insert k' v : past)
          | k==k'     = v:lkup k past
        lkup k (_ : past) = lkup k past

runA :: HashFun -> [Action] -> IO [Either Bool (Maybe Int)]
runA hf acts = do
  ht <- new (==) (unHF hf)
  let run res (Lookup a) = fmap (lkup res) $ lookup ht a
      run res (Insert k v) = insert ht k v >> return res
      run res (Delete k)   = delete ht k   >> return res
      run res (Update k v) = fmap (upd res) $ update ht k v
      lkup res m = Right m : res
      upd res b = Left b : res
  foldM run [] acts

(~~) :: IO [Either Bool (Maybe Int)] -> [Either Bool [Int]] -> Bool
acts ~~ sims = and $ zipWith same (unsafePerformIO acts) sims
  where same (Left b)         (Left b')  = b==b'
        same (Right Nothing)  (Right []) = True
        same (Right (Just a)) (Right xs) = a `elem` xs
        same _                _          = False

lookups :: HT -> [Int] -> IO [Maybe Int]
lookups ht ks = mapM (lookup ht) ks

instance Show HashFun where
  showsPrec _ (HF hf) r
      | hf 1 == 0 = "degenerate"++r
      | otherwise = "usual"++r

instance Show Empty where
  showsPrec _ ee r = shows (hfe ee) r

instance Show MkH where
  showsPrec _ hh r = shows (hfh hh) $
                     ("; "++shows (unsafePerformIO (h hh >>= toList)) r)

instance Show a => Show (List a) where
  showsPrec _ (L l) r = shows l r

instance Arbitrary HashFun where
  arbitrary = frequency [(20,return (HF hashInt)),
                         (1,return (HF (const 0)))]
  coarbitrary = error "coarbitrary HashFun"

instance Arbitrary Empty where
  arbitrary = fmap mkE arbitrary
    where mkE (HF hf) = E {e = new (==) hf, hfe=HF hf}
  coarbitrary = error "coarbitrary Empty"

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    sz <- frequency [(50, sized return),
                     (1,return (4096*2)),
                     (0, return (1024*1024))]
    resize sz $ fmap L $ sized vector
  coarbitrary = error "coarbitrary (List a)"

instance Arbitrary MkH where
  arbitrary = do
    hf <- arbitrary
    L list <- arbitrary
    let mkH act = H { h = act, hfh = hf }
    return (mkH . fromList (unHF hf) $ list)
  coarbitrary = error "coarbitrary MkH"

(==~) :: (Eq a) => IO a -> IO a -> Bool
act1 ==~ act2 = unsafePerformIO act1 == unsafePerformIO act2

(==.) :: (Eq a) => IO a -> a -> Bool
act ==. v = unsafePerformIO act == v

notin :: (Testable a) => Int -> MkH -> a -> Property
k `notin` hh = \prop ->
  let f = (not . isJust . unsafePerformIO) (h hh >>= flip lookup k) in
  f `trivial` prop

prop_emptyLookup :: Empty -> Int -> Bool
prop_emptyLookup ee k =
  isNothing . unsafePerformIO $
  (do mt <- e ee
      lookup mt k)

prop_emptyToList :: Empty -> Bool
prop_emptyToList ee =
  (do mt <- e ee
      toList mt) ==. []

prop_emptyFromList :: HashFun -> Int -> Bool
prop_emptyFromList hf k =
  (do mt <- new (==) (unHF hf) :: IO HT
      lookup mt k) ==~
  (do mt <- fromList (unHF hf) []
      lookup mt k)

prop_insert :: MkH -> Int -> Int -> Bool
prop_insert hh k v =
  (do ht <- h hh
      insert ht k v
      lookup ht k) ==. Just v

prop_insertu :: MkH -> Int -> Int -> List Int -> Bool
prop_insertu hh k v (L ks) =
  let ks' = filter (k /=) ks in
  (do ht <- h hh
      insert ht k v
      lookups ht ks') ==~
  (do ht <- h hh
      lookups ht ks')

prop_delete :: MkH -> Int -> Property
prop_delete hh k =
  k `notin` hh $
  isNothing . unsafePerformIO $
  (do ht <- h hh
      delete ht k
      lookup ht k)

prop_deleteu :: MkH -> Int -> List Int -> Bool
prop_deleteu hh k (L ks) =
  let ks' = filter (k /=) ks in
  (do ht <- h hh
      delete ht k
      lookups ht ks') ==~
  (do ht <- h hh
      lookups ht ks')

naiveUpdate :: HT -> Int -> Int -> IO ()
naiveUpdate ht k v = do
  delete ht k
  insert ht k v

prop_update :: MkH -> Int -> Int -> List Int -> Bool
prop_update hh k v (L ks) =
  (do ht <- h hh
      _ <- update ht k v
      lookups ht ks) ==~
  (do ht <- h hh
      naiveUpdate ht k v
      lookups ht ks)

prop_updatec :: MkH -> Int -> Int -> Bool
prop_updatec hh k v =
  (do ht <- h hh
      _ <- update ht k v
      lookup ht k) ==. Just v

prop_updateLookup :: MkH -> Int -> Int -> Property
prop_updateLookup hh k v =
  k `notin` hh $
  (do ht <- h hh
      update ht k v) ==~
  (do ht <- h hh
      fmap isJust (lookup ht k))

prop_simulation :: HashFun -> List Action -> Property
prop_simulation hf (L acts) =
  (null acts `trivial`) $
  runA hf acts ~~ simA acts

{-

For "fromList" and "toList" properties we're a bit sloppy: we perform
multiple insertions for a key (potentially) but give nor promises
about which one we will retrieve with lookup, or what order they'll be
returned by toList (or if they'll all be returned at all).  Thus we
insert all occurrences of a key with the same value, and do all
checking via lookups.

-}

prop_fromList :: HashFun -> List Int -> List Int -> Property
prop_fromList hf (L l) (L ks) =
  null l `trivial`
  let assocs = map (\t -> (t,t)) l in
  ( do ht <- fromList (unHF hf) assocs
       lookups ht ks) ==. (map (`Prelude.lookup` assocs) ks)

prop_fromListInsert :: HashFun -> List (Int,Int) -> Int -> Int -> List Int -> Property
prop_fromListInsert hf (L l) k v (L ks) =
  null l `trivial`
  (( do ht <- fromList (unHF hf) l
        insert ht k v
        lookups ht ks) ==~
   ( do ht <- fromList (unHF hf) (l++[(k,v)])
        lookups ht ks))

prop_toList :: HashFun -> List Int -> List Int -> Property
prop_toList hf (L l) (L ks) =
  null l `trivial`
  let assocs = map (\t -> (t,t)) l in
  ( do ht <- fromList (unHF hf) assocs
       lookups ht ks) ==~
  ( do ht <- fromList (unHF hf) assocs
       fmap (\as -> map (`Prelude.lookup` as) ks) $ toList ht )

te :: (Testable a) => String -> a -> IO ()
-- te name prop = putStrLn name >> verboseCheck prop
te name prop = do
  putStr name
  check (defaultConfig{configMaxTest = 500,
                               configMaxFail = 10000,
                               configEvery = \_ _ -> "" }) prop

main :: IO ()
main = do
  [r] <- getArgs
  setStdGen (mkStdGen (read r :: Int))
  sequence_ $
    [ te "emptyLookup:" prop_emptyLookup,
      te "emptyToList:" prop_emptyToList,
      te "emptyFromList:" prop_emptyFromList,
      te "insert:" prop_insert,
      te "insertu:" prop_insertu,
      te "delete:" prop_delete,
      te "deleteu:" prop_deleteu,
      te "update:" prop_update,
      te "updatec:" prop_updatec,
      te "updateLookup:" prop_updateLookup,
      te "fromList:" prop_fromList,
      te "fromListInsert:" prop_fromListInsert,
      te "toList:" prop_toList,
      te "simulation:" prop_simulation
    ]
  putStrLn "OK"
