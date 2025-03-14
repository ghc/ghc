{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module T25238 where

import Data.Kind

data MsgDef = MsgDoThis

type family Msg c
type instance Msg X = MsgDef

type family FromPat c :: Msg c

data X

type Id a = a

type instance FromPat X = MsgDoThis
type instance FromPat X = (MsgDoThis :: Msg X)
type instance FromPat X = (MsgDoThis :: Id (Msg X))
