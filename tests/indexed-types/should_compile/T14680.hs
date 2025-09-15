{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O1 #-}
module T14680 where

import GHC.Base (getTag)
import GHC.Exts (Int(..), tagToEnum#)

data family TyFamilyEnum
data instance TyFamilyEnum = TyFamilyEnum1 | TyFamilyEnum2 | TyFamilyEnum3

suc :: TyFamilyEnum -> TyFamilyEnum
suc a_aaf8
        = case getTag a_aaf8 of
             a_aaf9
               -> if 2 ==  I# a_aaf9
                  then error "succ{TyFamilyEnum}: tried to take `succ' of last tag in enumeration"
                  else case I# a_aaf9 + 1 of
                         I# i_aafa -> tagToEnum# i_aafa :: TyFamilyEnum
