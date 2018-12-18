{-# LANGUAGE TemplateHaskell, TypeApplications, PolyKinds
             , TypeFamilies, DataKinds #-}

module T12045TH2 where

import Data.Kind
import Language.Haskell.TH hiding (Type)
import System.IO

type family Foo (a :: k) :: Type where
  Foo @Type a = Bool

type family Baz (a :: k)
type instance Baz @(Type->Type->Type) a = Char

$( do FamilyI foo@(ClosedTypeFamilyD (TypeFamilyHead _ tvbs1 res1 m_kind1)
                   [TySynEqn (Just bndrs1) (AppT _ lhs1) rhs1])
              [] <- reify ''Foo
      FamilyI baz@(OpenTypeFamilyD (TypeFamilyHead _ tvbs2 res2 m_kind2))
              [inst@(TySynInstD (TySynEqn (Just bndrs2) (AppT _ lhs2) rhs2))] <- reify ''Baz
      runIO $ putStrLn $ pprint foo
      runIO $ putStrLn $ pprint baz
      runIO $ putStrLn $ pprint inst
      runIO $ hFlush stdout
      return [ ClosedTypeFamilyD
                 (TypeFamilyHead (mkName "Foo'") tvbs1 res1 m_kind1)
                 [TySynEqn (Just bndrs1) (AppT (ConT (mkName "Foo'")) lhs1) rhs1]
             , OpenTypeFamilyD
                 (TypeFamilyHead (mkName "Baz'") tvbs2 res2 m_kind2)
             , TySynInstD (TySynEqn (Just bndrs2) (AppT (ConT (mkName "Baz'")) lhs2) rhs2)] )
