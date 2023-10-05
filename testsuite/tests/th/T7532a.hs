{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T7532a where

import Language.Haskell.TH

class C a where
     data D a

bang' :: DecsQ
bang' = return [
     InstanceD Nothing [] (AppT (ConT ''C) (ConT ''Int)) [
         DataInstD [] Nothing (AppT (ConT ''D) (ConT ''Int)) Nothing [
             NormalC (mkName "T") []] []]]
