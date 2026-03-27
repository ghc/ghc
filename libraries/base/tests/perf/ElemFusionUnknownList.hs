-- We expect `elem` to fuse with good producers such as `map`, `concatMap`,
-- and `filter`.
module ElemFusionUnknownList where

fusionElemMap :: Int -> [Int] -> Bool
fusionElemMap x = elem x . map (+1)

fusionNotElemMap :: Int -> [Int] -> Bool
fusionNotElemMap x = notElem x . map (+1)

fusionElemConcatMap :: Int -> [Int] -> Bool
fusionElemConcatMap x = elem x . concatMap (\y -> [y + 1, y + 2])

fusionNotElemConcatMap :: Int -> [Int] -> Bool
fusionNotElemConcatMap x = notElem x . concatMap (\y -> [y + 1, y + 2])

fusionElemFilter :: Int -> [Int] -> Bool
fusionElemFilter x = elem x . filter odd

fusionNotElemFilter :: Int -> [Int] -> Bool
fusionNotElemFilter x = notElem x . filter odd
