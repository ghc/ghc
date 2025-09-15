module T23426 where

class (Char ~ a) => ListLike a where
    mnull :: a -> b

indent :: forall a. (ListLike a) => a -> Bool
indent x = let doText y = const (mnull y) doText
           in const (doText x) doText
