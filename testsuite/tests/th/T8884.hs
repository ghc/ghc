{-# LANGUAGE TemplateHaskell, TypeFamilies, PolyKinds #-}

module T8884 where

import Language.Haskell.TH
import System.IO

type family Foo a = r | r -> a where
  Foo x = x

type family Baz (a :: k) = (r :: k) | r -> a
type instance Baz x = x

$( do FamilyI foo@(ClosedTypeFamilyD _ tvbs1 res1 m_kind1 eqns1)
              [] <- reify ''Foo
      FamilyI baz@(OpenTypeFamilyD _ tvbs2 res2 m_kind2)
              [inst@(TySynInstD _ eqn2)] <- reify ''Baz
      runIO $ putStrLn $ pprint foo
      runIO $ putStrLn $ pprint baz
      runIO $ putStrLn $ pprint inst
      runIO $ hFlush stdout
      return [ ClosedTypeFamilyD (mkName "Foo'") tvbs1 res1 m_kind1 eqns1
             , OpenTypeFamilyD   (mkName "Baz'") tvbs2 res2 m_kind2
             , TySynInstD (mkName "Baz'") eqn2 ] )
