{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TH_RichKinds where

import Data.Kind (Type, Constraint)
import Language.Haskell.TH hiding (Type)

$(do  tys <- sequence [ [t| forall a. (a :: Bool) |]
                      , [t| forall a. (a :: Constraint) |]
                      , [t| forall a. (a :: [Type]) |]
                      , [t| forall a. (a :: (Type, Bool)) |]
                      , [t| forall a. (a :: ()) |]
                      , [t| forall a. (a :: (Type -> Bool)
                                         -> ((Type, Type -> Type) -> Bool)) |]
                      ]

      reportWarning (pprint tys)
      return [])
