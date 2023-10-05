{-# LANGUAGE TemplateHaskellQuotes #-}
module T21077_Lib where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO

data Foo = MkFoo () (forall a. a -> a)

qq :: QuasiQuoter
qq = QuasiQuoter
       { quoteExp  = const [| MkFoo () |]
       , quotePat  = undefined
       , quoteType = undefined
       , quoteDec  = undefined
       }

qqMod1 :: QuasiQuoter
qqMod1 = QuasiQuoter
           { quoteExp  = const $ do
                           addModFinalizer (runIO (hPutStrLn stderr "G"))
                           [| MkFoo () |]
           , quotePat  = undefined
           , quoteType = undefined
           , quoteDec  = undefined
           }

qqMod2 :: QuasiQuoter
qqMod2 = QuasiQuoter
           { quoteExp  = const $ do
                           addModFinalizer (runIO (hPutStrLn stderr "H2"))
                           [| $(do addModFinalizer (runIO (hPutStrLn stderr "H1"))
                                   [| MkFoo () |])
                            |]
           , quotePat  = undefined
           , quoteType = undefined
           , quoteDec  = undefined
           }
