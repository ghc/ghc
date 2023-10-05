{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeFamilies        #-}

module StaticPtrTypeFamily where

import Data.Typeable
import GHC.StaticPtr

type family F a

caller :: forall a. (Typeable a, Typeable (F a)) => a -> F a -> ()
caller a fa = deRefStaticPtr (static func) a fa

func :: a -> F a -> ()
func _ _ = ()
