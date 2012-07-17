{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TH_RichKinds where

import GHC.Prim
import Language.Haskell.TH

$(do  tys <- sequence [ [t| forall a. (a :: Bool) |]
                      , [t| forall a. (a :: Constraint) |]
                      , [t| forall a. (a :: [*]) |]
                      , [t| forall a. (a :: (*, Bool)) |]
                      , [t| forall a. (a :: ()) |]
                      , [t| forall a. (a :: (* -> Bool) -> ((*, * -> *) -> Bool)) |]
                      ]

      reportWarning (pprint tys)
      return [])
