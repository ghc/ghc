{-# LANGUAGE TemplateHaskell, GADTs, ExplicitForAll, KindSignatures,
             TypeFamilies, DataKinds #-}

module T10828 where

import Language.Haskell.TH hiding (Type)
import System.IO
import Data.Kind (Type)

$( do { decl <- [d| data family D a :: Type -> Type
                    data instance D Int Bool :: Type where
                         DInt :: D Int Bool

                    data E where
                      MkE :: a -> E

                    data Foo a b where
                      MkFoo, MkFoo' :: a -> Foo a b

                    newtype Bar :: Type -> Bool -> Type where
                      MkBar :: a -> Bar a b
                 |]

      ; runIO $ putStrLn (pprint decl) >> hFlush stdout
      ; return decl }
 )

-- data T a :: Type where
--    MkT :: a -> a -> T a
--    MkC :: forall a b. (a ~ Int) => { foo :: a, bar :: b } -> T Int

$( return
   [ DataD [] (mkName "T")
           [ PlainTV (mkName "a") () ]
           (Just StarT)
           [ GadtC [(mkName "MkT")]
                   [ ( Bang NoSourceUnpackedness NoSourceStrictness
                     , VarT (mkName "a")
                     )
                   , ( Bang NoSourceUnpackedness NoSourceStrictness
                     , VarT (mkName "a")
                     )
                   ]
                   (AppT (ConT (mkName "T"))
                         (VarT (mkName "a")))
           , ForallC [PlainTV (mkName "a") SpecifiedSpec, PlainTV (mkName "b") SpecifiedSpec]
                     [AppT (AppT EqualityT (VarT $ mkName "a"  ) )
                                           (ConT $ mkName "Int") ] $
             RecGadtC [(mkName "MkC")]
                  [ ( mkName "foo"
                    , Bang NoSourceUnpackedness NoSourceStrictness
                    , VarT (mkName "a")
                    )
                  , ( mkName "bar"
                    , Bang NoSourceUnpackedness NoSourceStrictness
                    , VarT (mkName "b")
                    )
                  ]
                  (AppT (ConT (mkName "T"))
                        (ConT (mkName "Int"))) ]
           [] ])

$( do { -- test reification
        TyConI dec <- runQ $ reify (mkName "T")
      ; runIO $ putStrLn (pprint dec) >> hFlush stdout

        -- test quoting
      ; d <- runQ $ [d|
             data T' a :: Type where
                MkT' :: a -> a -> T' a
                MkC' :: forall a b. (a ~ Int) => { foo :: a, bar :: b }
                                              -> T' Int |]
      ; runIO $ putStrLn (pprint d) >> hFlush stdout
      ; return [] } )
