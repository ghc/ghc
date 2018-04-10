module T15005 (
    OrderCell,
    ElementCell,
    rawAlgorithm,
    rawAlgorithmWithSize
    ) where

-- Control

import Control.Applicative
import Control.Monad
import Control.Monad.ST

-- Data

import Data.Word
import Data.Bits
import Data.STRef

type RawOrder s o = STRef s (o s)

type RawElement s e = STRef s (e s)

data RawAlgorithm s o e = RawAlgorithm {
    newOrder        :: ST s (RawOrder s o),
    compareElements :: RawElement s e -> RawElement s e -> RawOrder s o -> ST s Ordering,
    newMinimum      :: RawOrder s o -> ST s (RawElement s e),
    newMaximum      :: RawOrder s o -> ST s (RawElement s e),
    newAfter        :: RawElement s e -> RawOrder s o -> ST s (RawElement s e),
    newBefore       :: RawElement s e -> RawOrder s o -> ST s (RawElement s e),
    delete          :: RawElement s e -> RawOrder s o -> ST s ()
}
{-FIXME:
    If we ever allow users to plug in their own algorithms, we have to flag the
    respective function as unsafe and point out that referential transparency is
    in danger if the algorithm does not fulfill the specification. This is
    because element comparison is presented to the user as a pure function. The
    important condition is that for any two elements, compareElements must
    always return the same result as long as delete is not called on either
    element.
-}

type OrderCell = Cell

type ElementCell = Cell

data Cell s = Cell {
                  label :: Label,
                  next  :: CellRef s,
                  prev  :: CellRef s
              }

type CellRef s = STRef s (Cell s)

newtype Label = Label LabelWord deriving (Eq, Ord)

type LabelWord = Word64

labelWordSize :: Int
labelWordSize = 64

initialBaseLabel :: Label
initialBaseLabel = Label 0

rawAlgorithm :: RawAlgorithm s OrderCell ElementCell
rawAlgorithm = rawAlgorithmWithSize defaultSize

defaultSize :: Int
defaultSize = 63

rawAlgorithmWithSize :: Int -> RawAlgorithm s OrderCell ElementCell
rawAlgorithmWithSize size
    | size < 0 || size >= labelWordSize
        = error "Data.Order.Algorithm.dietzSleatorAmortizedLogWithSize: \
                \Size out of bounds"
    | otherwise
        = RawAlgorithm {
              newOrder        = fixST $
                                \ ref -> newSTRef $ Cell {
                                   label = initialBaseLabel,
                                   next  = ref,
                                   prev  = ref
                                },
              compareElements = \ ref1 ref2 baseRef -> do
                                    baseCell <- readSTRef baseRef
                                    cell1 <- readSTRef ref1
                                    cell2 <- readSTRef ref2
                                    let offset1 = labelDiff (label cell1)
                                                            (label baseCell)
                                    let offset2 = labelDiff (label cell2)
                                                            (label baseCell)
                                    return $ compare offset1 offset2,
              newMinimum      = newAfterCell,
              newMaximum      = newBeforeCell,
              newAfter        = const . newAfterCell,
              newBefore       = const . newBeforeCell,
              delete          = \ ref _ -> do
                                    cell <- readSTRef ref
                                    modifySTRef
                                        (prev cell)
                                        (\ prevCell -> prevCell {
                                                           next = next cell
                                                       })
                                    modifySTRef
                                        (next cell)
                                        (\ nextCell -> nextCell {
                                                           prev = prev cell
                                                       })
          } where

    noOfLabels :: LabelWord
    noOfLabels = shiftL 1 size

    labelMask :: LabelWord
    labelMask = pred noOfLabels

    toLabel :: LabelWord -> Label
    toLabel = Label . (.&. labelMask)

    labelSum :: Label -> Label -> Label
    labelSum (Label word1) (Label word2) = toLabel (word1 + word2)

    labelDiff :: Label -> Label -> Label
    labelDiff (Label word1) (Label word2) = toLabel (word1 - word2)

    labelDistance :: Label -> Label -> LabelWord
    labelDistance lbl1 lbl2 = case labelDiff lbl1 lbl2 of
                                  Label word | word == 0 -> noOfLabels
                                             | otherwise -> word

    newAfterCell :: CellRef s -> ST s (CellRef s)
    newAfterCell ref = do
        relabel ref
        lbl <- label <$> readSTRef ref
        nextRef <- next <$> readSTRef ref
        nextLbl <- label <$> readSTRef nextRef
        newRef <- newSTRef $ Cell {
            label = labelSum lbl (Label (labelDistance nextLbl lbl `div` 2)),
            next  = nextRef,
            prev  = ref
        }
        modifySTRef ref     (\ cell     -> cell     { next = newRef })
        modifySTRef nextRef (\ nextCell -> nextCell { prev = newRef })
        return newRef

    relabel :: CellRef s -> ST s ()
    relabel startRef = do
        startCell <- readSTRef startRef
        let delimSearch ref gapCount = do
                cell <- readSTRef ref
                let gapSum = labelDistance (label cell) (label startCell)
                if gapSum <= gapCount ^ 2
                    then if ref == startRef
                             then error "Data.Order.Algorithm.\
                                        \dietzSleatorAmortizedLogWithSize: \
                                        \Order full"
                             else delimSearch (next cell) (succ gapCount)
                    else return (ref, gapSum, gapCount)
        (delimRef, gapSum, gapCount) <- delimSearch (next startCell) 1
        let smallGap = gapSum `div` gapCount
        let largeGapCount = gapSum `mod` gapCount
        let changeLabels ref ix = when (ref /= delimRef) $ do
                cell <- readSTRef ref
                let lbl = labelSum
                              (label startCell)
                              (Label (ix * smallGap + min largeGapCount ix))
                writeSTRef ref (cell { label = lbl })
                changeLabels (next cell) (succ ix)
        changeLabels (next startCell) 1
    {-FIXME:
        We allow the number of cells to be larger than the square root of the
        number of possible labels as long as we find a sparse part in our circle
        of cells (since our order full condition is only true if the complete
        circle is congested). This should not influence correctness and probably
        also not time complexity, but we should check this more thoroughly.
    -}
    {-FIXME:
        We arrange the large and small gaps differently from Dietz and Sleator
        by putting all the large gaps at the beginning instead of distributing
        them over the relabeled area. However, this should not influence time
        complexity, as the complexity proof seems to only rely on the fact that
        gap sizes differ by at most 1. We should check this more thoroughly
        though.
    -}

    newBeforeCell :: CellRef s -> ST s (CellRef s)
    newBeforeCell ref = do
        cell <- readSTRef ref
        newAfterCell (prev cell)
