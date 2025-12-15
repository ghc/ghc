{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main where

import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Foreign.Marshal.Alloc as Foreign (alloca)
import Foreign.Marshal.Utils as Foreign
import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr

import System.Exit
import System.Mem


-- This test is in the style of QuickCheck state machines (but without that dep)
-- We compare a simple pure model/reference implementation of the ClosureTable
-- against the real thing.
--
-- We generate sequences of ClosureTable operations and compare the results of
-- each operation and also compare the state after each operation.
--
-- We use a SmallCheck style generator.

main :: IO ()
main =
  sequence_
    [ do tbl <- newClosureTable (fromIntegral (fromEnum compactness))
         res <- checkRealVsModel compactness tbl ops
         case res of
           TestSuccess         -> return ()
           TestFailure failure -> do
             reportTableFailure failure
             exitFailure

         freeClosureTable tbl

         -- This can trigger faults too, if we made mistakes in the
         -- ClosureTable C code that manages the heap objects.
         performGC

     -- This should exhaustively test all valid sequences up to depth 7,
     -- which is about ~1200 sequences. And then double that for the
     -- non-compact and compact scenarios.
   | compactness <- [NonCompact, Compact]
   , ops <- genTableOps 7 compactness initTableModel ]


-- This is deliberately a data type, and not a newtype Int, so we will get
-- heap closures in the young generation and not just statically allocated
-- small Int closures.
data Val = Val Int deriving (Eq, Show)

newtype Ix = Ix Int32  deriving (Eq, Ord, Enum, Show)

data TableOp a = Enlarge Int
               | Insert a
               | Remove Ix
  deriving Show

data TableOpResult = OpResultIx Ix
                   | OpResultRemapIx Ix Ix -- ^ tx_from tx_to
                   | OpVoid
                   | OpFailure
  deriving (Eq, Show)

data Compactness = NonCompact | Compact
  deriving (Eq, Show, Enum)

-------------------------------------------------------------------------------
-- Pure reference model
--

data TableModel a = TableModel {
                      capacity :: Int,
                      elems    :: !(Map Ix a),
                      free     :: [Ix]
                    }
  deriving (Eq, Show)

initTableModel :: TableModel a
initTableModel =
    TableModel {
      capacity = 0,
      elems    = Map.empty,
      free     = []
    }


size :: TableModel a -> Int
size TableModel { elems } = Map.size elems

isFull :: TableModel a -> Bool
isFull TableModel { free = [] } = True
isFull _                        = False

isEmpty :: TableModel a -> Bool
isEmpty TableModel { elems } = Map.null elems

invariantTableModel :: TableModel a -> Bool
invariantTableModel TableModel{ capacity, elems, free } =
    capacity == Map.size elems + length free

    -- elements and free list are disjoint
 && Set.null (Map.keysSet elems `Set.intersection` Set.fromList free)

    -- but their union is the whole range
 && Map.keysSet elems <> Set.fromList free == Set.fromList range
  where
    range = [ Ix 0 .. Ix (fromIntegral capacity - 1) ]

isCompact :: TableModel a -> Bool
isCompact TableModel{ elems, free } =
    Map.keys elems == map Ix [0 .. fromIntegral (Map.size elems - 1)]
 && ascending free
  where
    ascending xs = and (zipWith (<) xs (drop 1 xs))


applyTableOp :: Compactness -> TableOp a -> TableModel a
             -> (TableOpResult, TableModel a)
applyTableOp _ (Enlarge capacity') tm@TableModel{capacity}
  | capacity' >= capacity =
    (,) OpVoid
    tm {
      capacity = capacity',
      free     = [ Ix (fromIntegral capacity) .. Ix (fromIntegral capacity'-1) ]
              ++ free tm
    }

applyTableOp _ (Insert v) tm@TableModel{ elems, free = ix:free' }
  | not (ix `Map.member` elems) =
    (,) (OpResultIx ix)
    tm {
      elems = Map.insert ix v elems,
      free  = free'
    }

applyTableOp NonCompact (Remove ix) tm@TableModel{ elems, free }
  | ix `Map.member` elems =
    (,) OpVoid
    tm {
      elems = Map.delete ix elems,
      free  = ix : free
    }

applyTableOp Compact (Remove ix) tm@TableModel{ elems, free }
  | ix `Map.member` elems
  , let ix_from = Ix (fromIntegral (Map.size elems - 1))
        ix_to   = ix
  , Just e_from <- Map.lookup ix_from elems =
    (,) (OpResultRemapIx ix_from ix_to)
    tm {
      elems = Map.delete ix_from
            . Map.insert ix_to e_from
            $ elems,
      free  = ix_from : free
    }

applyTableOp _ _op tm = (OpFailure, tm)


-------------------------------------------------------------------------------
-- Perform the operations
--

performTableOp :: Compactness -> ClosureTable a -> TableOp a -> IO TableOpResult
performTableOp _ tbl (Enlarge capacity) =
    check . toBool <$>
      enlargeClosureTable tbl (fromIntegral capacity)
  where
    check True  = OpVoid
    check False = OpFailure

performTableOp _ tbl (Insert v) =
    OpResultIx . Ix . fromIntegral <$> insertClosureTable tbl v

performTableOp NonCompact tbl (Remove (Ix ix)) =
    OpVoid <$ removeClosureTable tbl (fromIntegral ix)

performTableOp Compact tbl (Remove (Ix ix)) =
    Foreign.alloca $ \ix_from_ptr ->
    Foreign.alloca $ \ix_to_ptr   -> do
      removeCompactClosureTable tbl (fromIntegral ix) ix_from_ptr ix_to_ptr
      ix_from <- peek ix_from_ptr
      ix_to   <- peek ix_to_ptr
      return $! OpResultRemapIx (Ix (fromIntegral ix_from))
                                (Ix (fromIntegral ix_to))


-------------------------------------------------------------------------------
-- Snapshot real state
--

data TableSnapshot a = TableSnapshot {
       tsIsFull       :: Bool,
       tsIsEmpty      :: Bool,
       tsCapacity     :: Int,
       tsSize         :: Int,
       tsElements     :: Map Ix a,
       tsElemsNull    :: Set Ix,
       tsFreeList     :: Map Ix Ix,
       tsFreeListHead :: Ix
     }
  deriving Show

takeTableSnapshot :: ClosureTable a -> [Ix] -> IO (TableSnapshot a)
takeTableSnapshot tbl ixs = do
    isfull   <- isFullClosureTable tbl
    isempty  <- isEmptyClosureTable tbl
    capacity <- capacityClosureTable tbl
    sz       <- sizeClosureTable tbl
    elems    <- sequence [ indexClosureTable tbl (fromIntegral ix)
                         | Ix ix <- ixs ]
    isnull   <- sequence [ toBool <$> indexIsNullClosureTable tbl ix
                         | ix <- [0..capacity - 1] ]
    free     <- sequence [ indexFreeListClosureTable tbl ix
                         | ix <- [0..capacity - 1] ]
    free_hd  <- getFreeListHeadClosureTable tbl

    let ixrange = [Ix 0 .. Ix (fromIntegral capacity - 1)]
    return TableSnapshot {
       tsIsFull       = toBool isfull,
       tsIsEmpty      = toBool isempty,
       tsCapacity     = fromIntegral capacity,
       tsSize         = fromIntegral sz,
       tsElements     = Map.fromList (zip ixs elems),
       tsElemsNull    = Set.fromList [ ix | (ix, True) <- zip ixrange isnull ],
       tsFreeList     = Map.fromList (zip ixrange (map (Ix . fromIntegral) free)),
       tsFreeListHead = Ix (fromIntegral free_hd)
     }

snapshotMatchesModel :: Eq a => TableSnapshot a -> TableModel a -> Maybe [String]
snapshotMatchesModel s m
  | all fst checks = Nothing
  | otherwise      = Just (map snd checks)
  where
    checks =
      [ (isFull   m == tsIsFull   s,
        "isFull   m /= tsIsFull   s")

      , (isEmpty  m == tsIsEmpty  s,
        "isEmpty  m /= tsIsEmpty  s")

      , (capacity m == tsCapacity s,
        "capacity m /= tsCapacity s")

      , (size     m == tsSize     s,
        "size     m /= tsSize     s")

      , (elems    m == tsElements s,
        "elems    m /= tsElements s")

      , (free     m == walkFree (tsFreeListHead s),
        "free     m /= walkFree (tsFreeListHead s)")

      , (Set.fromList (free m) == tsElemsNull s,
        "Set.fromList (free m) /= tsElemsNull s")
      ]
    walkFree ix
      | ix == Ix (-1)  = []
      | otherwise = ix : walkFree (tsFreeList s Map.! ix)


-------------------------------------------------------------------------------
-- Compare vs model
--

data TestResult failure = TestSuccess | TestFailure failure

data TableFailure a =
       ResultMismatch TableOpResult TableOpResult
                      [TableOp a] (TableModel a) (TableSnapshot a)
     | StateMismatch [TableOp a] (TableModel a) (TableSnapshot a) [String]
     | ModelInvariantViolation [TableOp a] (TableModel a) (TableModel a)

reportTableFailure :: Show a => TableFailure a -> IO ()
reportTableFailure (StateMismatch ops model snapshot mismatches) = do
    putStrLn "Real state vs model mismatch after operations:"
    mapM_ print ops
    putStrLn "Model state:"
    print model
    putStrLn "ClosureTable state snapshot:"
    print snapshot
    putStrLn ("Mismatches:" ++ unlines mismatches)

reportTableFailure (ResultMismatch modelResult realResult ops model snapshot) = do
    putStrLn "Real result vs model result mismatch after operations:"
    mapM_ print ops
    putStrLn "Model result:"
    print modelResult
    putStrLn "ClosureTable result:"
    print realResult
    putStrLn "Model state:"
    print model
    putStrLn "ClosureTable state snapshot:"
    print snapshot

reportTableFailure (ModelInvariantViolation ops model model') = do
    putStrLn "Model invariant violation mismatch after operations:"
    mapM_ print ops
    putStrLn "Model state before final op:"
    print model
    putStrLn "Model state after final op:"
    print model'

checkRealVsModel :: forall a. (Eq a, Show a)
                 => Compactness
                 -> ClosureTable a -> [TableOp a]
                 -> IO (TestResult (TableFailure a))
checkRealVsModel compactness tbl ops0 = do
    snapshot <- takeTableSnapshot tbl []
    checkOpResult initTableModel snapshot [] ops0
  where
    checkOpResult :: TableModel a
                  -> TableSnapshot a
                  -> [TableOp a]
                  -> [TableOp a]
                  -> IO (TestResult (TableFailure a))
    checkOpResult _ _ _ [] = return TestSuccess
    checkOpResult model snapshot ops' (op:ops) = do
      let (modelResult, model') = applyTableOp compactness op model
      if not (invariantTableModel model') ||
         (compactness == Compact && not (isCompact model'))
        then return $ TestFailure $
               ModelInvariantViolation (reverse (op:ops')) model model'
        else do
          realResult <- performTableOp compactness tbl op
          if modelResult /= realResult
            then return $ TestFailure $
                   ResultMismatch modelResult realResult
                                  (reverse (op:ops')) model snapshot

            else checkState model' (op:ops') ops

    checkState :: TableModel a
               -> [TableOp a]
               -> [TableOp a]
               -> IO (TestResult (TableFailure a))
    checkState model ops' ops = do
      snapshot <- takeTableSnapshot tbl (Map.keys (elems model))
      case snapshotMatchesModel snapshot model of
        Just failures ->
          return $ TestFailure $
            StateMismatch (reverse ops') model snapshot failures

        Nothing -> checkOpResult model snapshot ops' ops


-------------------------------------------------------------------------------
-- ClosureTable FFI imports
--

-- Mostly wrappers in the ./ClosureTable_cbits.c, some from rts/ClosureTable.c

newtype ClosureTable a = ClosureTable (Ptr (ClosureTable a))
  deriving Show

foreign import ccall unsafe "newClosureTable"
  newClosureTable :: CInt -> IO (ClosureTable a)

foreign import ccall unsafe "freeClosureTable"
  freeClosureTable :: ClosureTable a -> IO ()

foreign import ccall unsafe "isEmptyClosureTable_wrapper"
  isEmptyClosureTable :: ClosureTable a -> IO CBool

foreign import ccall unsafe "isFullClosureTable_wrapper"
  isFullClosureTable :: ClosureTable a -> IO CBool

foreign import ccall unsafe "capacityClosureTable_wrapper"
  capacityClosureTable :: ClosureTable a -> IO CInt

foreign import ccall unsafe "sizeClosureTable_wrapper"
  sizeClosureTable :: ClosureTable a -> IO CInt

foreign import ccall unsafe "insertClosureTable_wrapper"
  insertClosureTable_wrapper :: ClosureTable a -> StablePtr a -> IO CInt

foreign import ccall unsafe "removeClosureTable_wrapper"
  removeClosureTable :: ClosureTable a -> CInt -> IO ()

foreign import ccall unsafe "removeCompactClosureTable_wrapper"
  removeCompactClosureTable :: ClosureTable a -> CInt
                            -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "indexClosureTable_wrapper"
  indexClosureTable_wrapper :: ClosureTable a -> CInt -> IO (StablePtr a)

foreign import ccall unsafe "indexIsNullClosureTable"
  indexIsNullClosureTable :: ClosureTable a -> CInt -> IO CBool

foreign import ccall unsafe "indexFreeListClosureTable"
  indexFreeListClosureTable :: ClosureTable a -> CInt -> IO CInt

foreign import ccall unsafe "getFreeListHeadClosureTable"
  getFreeListHeadClosureTable :: ClosureTable a -> IO CInt

foreign import ccall unsafe "enlargeClosureTable_wrapper"
  enlargeClosureTable :: ClosureTable a -> CInt -> IO CBool

foreign import ccall unsafe "printClosureTable"
  printClosureTable :: ClosureTable a -> IO ()

insertClosureTable :: ClosureTable a -> a -> IO CInt
insertClosureTable t v = do
    p <- newStablePtr v
    ix <- insertClosureTable_wrapper t p
    freeStablePtr p
    return ix

indexClosureTable :: ClosureTable a -> CInt -> IO a
indexClosureTable t ix = do
    p <- indexClosureTable_wrapper t ix
    v <- deRefStablePtr p
    freeStablePtr p
    return v


-------------------------------------------------------------------------------
-- Generator
--

genTableOps :: Int -> Compactness -> TableModel Val -> [[TableOp Val]]
genTableOps 0 _ _ = pure []
genTableOps d c m =
    genTableOp d c m >>- \op ->
    let (r, m') = applyTableOp c op m in
    case r of
      OpFailure -> error ("genTableOps: " ++ show op ++ " in state " ++ show m)
      _         -> (:) op <$> genTableOps (d-1) c m'

genTableOp :: Int -> Compactness -> TableModel Val -> [TableOp Val]
genTableOp d c m =
    genEnlarge d c m
 \/ genInsert  d m
 \/ genDelete  m

genEnlarge :: Int -> Compactness -> TableModel Val -> [TableOp Val]
genEnlarge d Compact m | not (isFull m) = []
  -- not allowed to enlarge a compact table unless it is full

genEnlarge d c m =
    [ (capacity m+1) .. d ] >>- \n ->
    pure (Enlarge n)

genInsert :: Int -> TableModel Val -> [TableOp Val]
genInsert d m | isFull m  = []
              | otherwise = pure (Insert (Val d))

genDelete :: TableModel Val -> [TableOp Val]
genDelete m =
    Map.keys (elems m) >>- \ix ->
    pure (Remove ix)


-------------------------------------------------------------------------------
-- "MuCheck" SmallCheck, but much smaller
--

infixr 3 \/

(\/) :: [a] -> [a] -> [a]
[]     \/ ys = ys
(x:xs) \/ ys = x : (ys \/ xs)

infixl 1 >>-

(>>-) :: [a] -> (a -> [b]) -> [b]
[]     >>- _ = []
(x:xs) >>- f = f x \/ (xs >>- f)

