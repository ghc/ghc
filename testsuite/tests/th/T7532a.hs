{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T7532a where

import Language.Haskell.TH

class C a where
     data D a

bang :: DecsQ
bang = return [
     InstanceD [] (AppT (ConT ''C) (ConT ''Int)) [
         DataInstD [] ''D [ConT ''Int] [
             NormalC (mkName "T") []] []]] 
