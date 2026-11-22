{-# LANGUAGE DataKinds, EmptyCase, GADTs, NoExperimentalDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider

paired :: Nested a -> [(a, a)]
paired (Nested (Pair _ (xs, Pair _ (ys, _)))) = cells (zipVec xs ys)

outerCells :: Nested a -> [a]
outerCells (Nested (Pair _ (xs, _))) = cells xs

isEmptyPack :: Packed Bool -> Bool
isEmptyPack (Pair _ xs) = isJust (emptyWitness xs)

rebuildDirect :: DirectVec a n -> DirectVec a n
rebuildDirect (DirectVec n xs) = DirectVec n xs

openDirect :: Packed a -> [a]
openDirect (Pair n xs) =
    let wrapped = DirectVec n xs
    in case rebuildDirect wrapped of DirectVec _ ys -> cells ys

-- | Read the first cell only when the stored length is a successor.
firstDirect :: DirectVec a n -> Maybe a
firstDirect (DirectVec Z _) = Nothing
firstDirect (DirectVec (S _) (VCons x _)) = Just x

-- | Use the public index to check a later argument at the same length.
firstAtIndex :: DirectVec a n -> Vec b n -> Maybe b
firstAtIndex (DirectVec Z _) _ = Nothing
firstAtIndex (DirectVec (S _) _) (VCons x _) = Just x

-- | Relate the public index to the zero branch of the stored length.
zeroIndex :: DirectVec a n -> Maybe (n :~: 'Z)
zeroIndex (DirectVec Z _) = Just Refl
zeroIndex (DirectVec (S _) _) = Nothing

-- | Recover the predecessor payload at the index of the caller.
succPayload :: SuccVec a ('S n) -> Vec a n
succPayload (SuccVec _ xs) = xs

-- | Match the outer box and leave both fields unevaluated.
hasSuccVec :: SuccVec a size -> Bool
hasSuccVec (SuccVec _ _) = True

-- | Inspect the predecessor at an unknown public index.
zeroPredecessor :: SuccVec a size -> Bool
zeroPredecessor (SuccVec Z _) = True
zeroPredecessor (SuccVec (S _) _) = False

-- | Read both dimensions independently.
matrixShape :: Matrix a rows columns -> (Int, Int)
matrixShape (Matrix rows columns _) = (lengthValue rows, lengthValue columns)
matrixShape (MatrixByColumns columns rows _) = (lengthValue rows, lengthValue columns)

-- | Check the payload against both public dimensions.
matrixCells :: Matrix a rows columns -> Vec (Vec a columns) rows
matrixCells (Matrix _ _ xs) = xs
matrixCells (MatrixByColumns _ _ xs) = xs

-- | Recover the payload at both components of the public shape.
tileCells :: Tile a ('Rectangle rows columns) -> Vec (Vec a columns) rows
tileCells (Tile _ _ xs) = xs

-- | Read the dimensions independently.
tileDimensions :: Tile a shape -> (Int, Int)
tileDimensions (Tile rows columns _) = (lengthValue rows, lengthValue columns)

-- | Match the constructor and leave its dimensions and cells unevaluated.
hasTile :: Tile a shape -> Bool
hasTile (Tile _ _ _) = True

-- | Prove the shape when the stored flag is true.
flaggedTrue :: Flagged shape -> Maybe (shape :~: 'Flag 'True)
flaggedTrue (Flagged True) = Just Refl
flaggedTrue (Flagged False) = Nothing

-- | Recover each payload index from its position in the public shape.
blockPayloads :: Blocked a ('Block width mark depth) -> (Vec a width, Vec a depth)
blockPayloads (Blocked _ _ _ xs ys) = (xs, ys)

-- | Infer the predecessor when passing the first payload to a helper.
blockFirst :: Blocked a ('Block ('S width) mark depth) -> a
blockFirst (Blocked _ _ _ xs _) = first xs
  where
    first :: Vec b ('S n) -> b
    first (VCons x _) = x

-- | Read only the mark.
blockMark :: Blocked a shape -> Bool
blockMark (Blocked mark _ _ _ _) = mark

-- | Use both result equalities to prove the dimensions agree.
squareDimensions :: SquareMatrix a rows columns -> rows :~: columns
squareDimensions (Square _ _) = Refl

-- | Eliminate a square whose public dimensions disagree.
{-# NOINLINE emptySquare #-}
emptySquare :: SquareMatrix a 'Z ('S 'Z) -> ()
emptySquare x = case x of {}

-- | Inspect the stored field of a possible square.
{-# NOINLINE squareIsEmpty #-}
squareIsEmpty :: SquareMatrix a n n -> Bool
squareIsEmpty (Square Z _) = True
squareIsEmpty (Square (S _) _) = False

-- | Return the vector from a marked row.
markedCells :: MarkedRows a rows 'True -> Vec a rows
markedCells (MarkedRows True _ xs) = xs

phantomPayload :: Phantom -> Bool
phantomPayload (Pair _ xs) = sameLength xs empty
  where
    empty :: Vec Bool 'Z
    empty = VNil

require :: Bool -> String -> IO ()
require ok label = unless ok (fail label)

throws :: String -> a -> IO ()
throws label value = do
    result <- try @ErrorCall (evaluate value)
    case result of
        Left err -> require (label `isPrefixOf` displayException err) (displayException err)
        Right _ -> fail ("Expected " ++ label)

main :: IO ()
main = do
    let one = VCons True VNil
        other = VCons False VNil
        value = makeNested (S Z) (S (S Z)) one other (VCons False one)
        hidden = makeHidden (S (S Z)) one other (VCons False one)
        aligned = [(True, False)]
        packedChar :: Packed Char
        packedChar = Pair (S Z) (VCons 'q' VNil)
    require (isEmptyPack (pack Z VNil)) "inferred empty payload index"
    require (not (isEmptyPack (pack (S Z) (VCons True (error "unused tail")))))
        "inferred nonempty payload index without demanding its tail"
    require (phantomPayload phantom) "the reduced payload determines the index"
    require (take 1 (openDirect (pack (S Z) (VCons True (error "direct tail")))) == [True])
        "a direct result keeps the index opened by the dependent sum"
    require (openDirect packedChar == "q") "a direct result preserves the element type"
    n <- newIORef (S Z) >>= readIORef
    unused <- newIORef (error "direct length" :: Nat) >>= readIORef
    let vector = DirectVec n (error "direct vector")
        atLength = DirectAtLength unused (AtLength 37)
    require (firstDirect (DirectVec (S Z) (VCons True (error "refined tail"))) == Just True)
        "a direct successor pattern checks the vector without demanding its tail"
    require (firstAtIndex (DirectVec (S Z) (error "unused first vector"))
        (VCons 'r' (error "later tail")) == Just 'r')
        "the public successor index scopes over a later argument"
    let emptyDirect = DirectVec Z (error "zero payload")
    require (firstDirect emptyDirect == (Nothing :: Maybe Bool))
        "the zero branch leaves the payload unevaluated"
    require (firstAtIndex emptyDirect (error "later zero vector") == (Nothing :: Maybe Char))
        "the zero branch leaves the later vector unevaluated"
    case zeroIndex emptyDirect of
        Just Refl -> pure ()
        Nothing -> fail "the zero branch did not prove the public index"
    require (not (isJust (zeroIndex vector))) "the successor branch leaves the vector unevaluated"
    throws "direct vector" (firstDirect vector)
    throws "direct length" (firstDirect (DirectVec unused (error "unexpected direct payload")))
    require ((case rebuildDirect vector of DirectVec len _ -> lengthValue len) == 1)
        "a direct result leaves the vector unevaluated"
    require ((case atLength of DirectAtLength _ (AtLength value) -> value) == 37)
        "a direct result leaves the length unevaluated"
    throws "direct vector" (case rebuildDirect vector of DirectVec _ xs -> cells xs)
    let lazySucc = SuccVec unused (error "successor payload")
    require (cells (succPayload (SuccVec (S Z) one)) == [True])
        "the successor result determines the payload index"
    require (hasSuccVec lazySucc) "wildcards leave both successor fields unevaluated"
    throws "direct length" (zeroPredecessor lazySucc)
    throws "successor payload" (cells (succPayload lazySucc))
    throws "direct length" (case atLength of DirectAtLength len _ -> lengthValue len)
    let rectangle = VCons (VCons True (VCons False VNil)) VNil
        matrix = Matrix (S Z) (S (S Z)) rectangle
        byColumns = MatrixByColumns (S (S Z)) (S Z) rectangle
        badRow = Matrix unused (S Z) (error "matrix payload")
        badColumn = MatrixByColumns unused (S Z) (error "matrix payload")
    require (matrixShape matrix == (1, 2) && matrixShape byColumns == (1, 2))
        "both constructor orders preserve the public dimensions"
    require (map cells (cells (matrixCells matrix)) == [[True, False]] &&
             map cells (cells (matrixCells byColumns)) == [[True, False]])
        "both result equalities apply to the matrix payload"
    require (snd (matrixShape badRow) == 1 && fst (matrixShape badColumn) == 1)
        "reading either dimension leaves the other dimension and the payload unevaluated"
    throws "direct length" (fst (matrixShape badRow))
    throws "direct length" (snd (matrixShape badColumn))
    throws "matrix payload" (cells (matrixCells badRow))
    let tile = Tile (S Z) (S (S Z)) (VCons (VCons True (VCons False VNil)) VNil)
        tileRowBottom = Tile unused (S (S Z)) (error "tile cells")
        tileColumnBottom = Tile (S Z) unused (error "tile cells")
    require (tileDimensions tile == (1, 2) && map cells (cells (tileCells tile)) == [[True, False]])
        "the public shape determines both matrix dimensions"
    require (fst (tileDimensions tileColumnBottom) == 1 && snd (tileDimensions tileRowBottom) == 2)
        "reading one dimension leaves the other and the cells unevaluated"
    require (hasTile (Tile unused unused (error "tile cells")))
        "a wildcard tile match leaves all fields unevaluated"
    throws "direct length" (fst (tileDimensions tileRowBottom))
    throws "direct length" (snd (tileDimensions tileColumnBottom))
    throws "tile cells" (cells (tileCells tileRowBottom))
    case flaggedTrue (Flagged True) of
        Just Refl -> pure ()
        Nothing -> fail "the stored flag determines the unary shape"
    let block = Blocked True Z (S Z) one VNil
        (wide, deep) = blockPayloads block
    require (cells wide == [True] && null (cells deep))
        "the promoted shape determines the payloads in index order"
    require (blockFirst block) "a helper infers its predecessor from the public shape"
    require (blockMark (Blocked True unused unused (error "width cells") (error "depth cells")))
        "reading the block mark leaves dimensions and payloads unevaluated"
    require (cells (markedCells (MarkedRows True (S Z) one)) == [True])
        "different retained kinds follow the result parameter order"
    case squareDimensions (Square unused (error "square payload")) of
        Refl -> pure ()
    throws "square scrutinee" (emptySquare (error "square scrutinee"))
    throws "direct length" (squareIsEmpty (Square unused (error "square payload")))
    require (storedLengths value == (1, 2)) "stored lengths"
    require (innerCells value == [False, True]) "inner cells"
    require (paired value == aligned) "shared outer index"
    require (withHidden hidden (\_ xs ys _ -> cells (zipVec xs ys)) == aligned)
        "constructor-local erased index"
    require (innerCells outerWitnessBottom == [True]) "lazy outer witness and vectors"
    require (paired innerWitnessBottom == aligned) "lazy inner witness and vector"
    require (outerCells innerPairBottom == [True]) "lazy inner pair"
    throws "outer witness" (fst (storedLengths outerWitnessBottom))
    throws "inner witness" (snd (storedLengths innerWitnessBottom))
    throws "second vector" (innerCells innerWitnessBottom)
    throws "inner pair" (innerCells innerPairBottom)
    require (withLater (makeLater (S (S Z)) (S Z) (VCons False one) (VCons 'x' VNil))
        (\n m xs ys -> (lengthValue n, lengthValue m, cells xs, cells ys)) == (2, 1, [False, True], "x"))
        "earlier retained constructor argument"
    require (withLater laterWitnessBottom (\_ _ _ ys -> cells ys) == "x") "lazy earlier constructor argument"
    throws "earlier witness" (withLater laterWitnessBottom (\n _ _ _ -> lengthValue n))
    case fieldScope of
        FieldScope n produce (Pair _ (outer, _)) ->
            case produce (S Z) True one of
                Pair m (_, _, inner, _) ->
                    require ((lengthValue n, lengthValue m, cells outer, cells inner)
                        == (2, 0, [False, True], [True])) "field-local retained length"
