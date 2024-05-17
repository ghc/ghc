{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Bug679 where

import Language.Haskell.TH

data Bar a = Bar

$(do
   a <- newName "a"
   
   let classN = mkName "Foo"
   let methodN = mkName "foo"

   methodTy <- [t| $(varT a) -> $(varT a) |]
   let cla = ClassD [] classN [PlainTV a BndrReq] [] [SigD methodN methodTy]
 
   -- Note that we are /reusing/ the same type variable 'a' as in the class
   instanceHead <- [t| $(conT classN) (Bar $(varT a)) |]
   idCall <- [e| id |]
   let ins = InstanceD Nothing [] instanceHead [FunD methodN [Clause [] (NormalB idCall) []]]
    
   pure [cla,ins])

