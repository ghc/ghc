{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module T17394 where

import GHC.Generics
import Language.Haskell.TH

type T1 = $(infixT (promotedT 'Nothing) '(:*:) (promotedT 'Nothing))
type T2 = $(infixT (conT     ''Maybe)  ''(:*:) (conT     ''Maybe))
