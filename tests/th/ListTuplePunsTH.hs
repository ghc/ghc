{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ListTuplePunsTH where

import Data.Kind (Type, Constraint)
import Language.Haskell.TH hiding (Type)
import GHC.Exts (List)
import Data.Tuple.Experimental (Tuple2, Unit)

$(do  tys <- sequence [ [t| forall a. (a :: Constraint) |]
                      , [t| forall a. (a :: List Type) |]
                      , [t| forall a. (a :: Tuple2 Type Bool) |]
                      , [t| forall a. (a :: Unit) |]
                      , [t| forall a. (a :: (Type -> Bool)
                                         -> (Tuple2 Type (Type -> Type) -> Bool)) |]
                      ]

      reportWarning (pprint tys)
      return [])
