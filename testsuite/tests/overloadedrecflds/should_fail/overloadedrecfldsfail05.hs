{-# LANGUAGE OverloadedRecordFields, TypeFamilies, FlexibleInstances,
             DataKinds, MultiParamTypeClasses #-}

import GHC.Records

data Person = MkPerson { firstName :: String, lastName :: String }

type instance FldTy Person "fullName" = String
instance Has Person "fullName" String where
  getField _ p = firstName p ++ " " ++ lastName p
