{-# LANGUAGE TemplateHaskell, GADTs, ExplicitForAll, KindSignatures,
             TypeFamilies, DataKinds #-}

module T10828 where

import Language.Haskell.TH
import System.IO

$( do { decl <- [d| data family D a :: * -> *
                    data instance D Int Bool :: * where
                         DInt :: D Int Bool

                    data E where
                      MkE :: a -> E

                    data Foo a b where
                      MkFoo, MkFoo' :: a -> Foo a b

                    newtype Bar :: * -> Bool -> * where
                      MkBar :: a -> Bar a b
                 |]

      ; runIO $ putStrLn (pprint decl) >> hFlush stdout
      ; return decl }
 )

-- data T a :: * where
--    MkT :: a -> a -> T a
--    MkC :: forall a b. (a ~ Int) => { foo :: a, bar :: b } -> T Int

$( return
   [ DataD [] (mkName "T")
           [ PlainTV (mkName "a") ]
           (Just StarT)
           [ GadtC [(mkName "MkT")]
                   [ (NotStrict, VarT (mkName "a"))
                   , (NotStrict, VarT (mkName "a"))]
                   ( mkName "T" )
                   [ VarT (mkName "a") ]
           , ForallC [PlainTV (mkName "a"), PlainTV (mkName "b")]
                     [AppT (AppT EqualityT (VarT $ mkName "a"  ) )
                                           (ConT $ mkName "Int") ] $
             RecGadtC [(mkName "MkC")]
                  [ (mkName "foo", NotStrict, VarT (mkName "a"))
                  , (mkName "bar", NotStrict, VarT (mkName "b"))]
                  ( mkName "T" )
                  [ ConT (mkName "Int") ] ]
           [] ])

$( do { -- test reification
        TyConI dec <- runQ $ reify (mkName "T")
      ; runIO $ putStrLn (pprint dec) >> hFlush stdout

        -- test quoting
      ; d <- runQ $ [d|
             data T' a :: * where
                MkT' :: a -> a -> T' a
                MkC' :: forall a b. (a ~ Int) => { foo :: a, bar :: b }
                                              -> T' Int |]
      ; runIO $ putStrLn (pprint d) >> hFlush stdout
      ; return [] } )
