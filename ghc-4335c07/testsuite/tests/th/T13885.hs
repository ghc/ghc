{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Function (on)
import Language.Haskell.TH.Syntax

data a :~: b where
  Refl :: a ~ b => a :~: b

$(return [])

main :: IO ()
main = print
  $(do TyConI (DataD _ _ tycon_tyvars _
                     [ForallC con_tyvars _ _] _) <- reify ''(:~:)

       let tvbName :: TyVarBndr -> Name
           tvbName (PlainTV  n)   = n
           tvbName (KindedTV n _) = n

       lift $ and $ zipWith ((/=) `on` tvbName) tycon_tyvars con_tyvars)
