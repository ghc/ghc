
-- | Extra array operators implemented using lists.
--   These are DPH specific functions that don't come in the standard list library.
-- 
--   NOTE: These are mostly copied from dph-prim-interface:Data.Array.Parallel.Unlifted.
--         We copy them instead of using them directly because we don't want the Elt 
--         contstraint imposed by DPH_Interface.h
--
--   TODO: Perhaps we should make the D.A.P.Unlifted module point to this one 
--         (or a common one) instead. Maybe put this stuff in dph-base.
--
module DPH.Operators.List
        ( packByTag
        , combine
        , combine2)
where

-- | Filter an array based on some tags.
packByTag :: Eq t => [a] -> [t] -> t -> [a] 
packByTag xs tags tag
        = [x | (x, t) <- zip xs tags, t == tag]


-- | Combine two arrays based on a `Bool` selector list.
combine :: [Bool] -> [a] -> [a] -> [a]
combine [] [] [] = []
combine (True  : bs) (x : xs) ys       = x : combine bs xs ys
combine (False : bs) xs       (y : ys) = y : combine bs xs ys


-- | Combine two arrays based on an `Int` selector list.
--   If the selector value is 0 then choose the first list, else choose the second.
combine2 :: [Int] -> [a] -> [a] -> [a]
combine2 tags xs ys = go tags xs ys
  where
    go [] [] [] = []
    go (0 : bs) (x : xs) ys = x : go bs xs ys
    go (1 : bs) xs (y : ys) = y : go bs xs ys
