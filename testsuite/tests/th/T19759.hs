{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module T19759 where

import Language.Haskell.TH

data T a where
  MkT :: a -> b -> T a

{-
pattern MkT' :: () => forall b. a -> b -> T a
pattern MkT' x y = MkT x y
-}

$(do let mkT' = mkName "MkT'"
     a <- newName "a"
     b <- newName "b"
     x <- newName "x"
     y <- newName "y"
     pure [ PatSynSigD mkT' $ ForallT [] [] $ ForallT [PlainTV b SpecifiedSpec] []
                            $ ArrowT `AppT` VarT a `AppT` (ArrowT `AppT` VarT b `AppT` (ConT ''T `AppT` VarT a))
          , PatSynD mkT' (PrefixPatSyn [x, y]) ImplBidir $
              ConP 'MkT [VarP x, VarP y]
          ])

