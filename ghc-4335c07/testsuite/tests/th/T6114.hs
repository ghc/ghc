{-# LANGUAGE TemplateHaskell #-}
module T6114 where
import Language.Haskell.TH

$(do
   xName <- newName "x"
   instanceType <- [t| $(varT xName) |]
   _ <- reifyInstances ''Eq [instanceType]
   return []
  )
