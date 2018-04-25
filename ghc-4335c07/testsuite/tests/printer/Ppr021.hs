{-# LANGUAGE PatternSynonyms #-}
{-# Language DeriveFoldable #-}
{-# LANGUAGE Safe #-}
{-# options_ghc -w #-}

-- | A simple let expression, to ensure the layout is detected
-- With some haddock in the top
{- And a normal
   multiline comment too -}
  module {- brah -}  LetExpr        ( foo -- foo does ..
                                    , bar -- bar does ..
                                    , Baz () -- baz does ..
                                 , Ba   ( ..),Ca(Cc,Cd)   ,
                                     bbb ,  aaa
                                  , module  Data.List
                                    , pattern  Bar
                                    )
    where

import Data.List
-- A comment in the middle
import {-# SOURCE #-} BootImport ( Foo(..) )
import {-# SoURCE  #-} safe   qualified  BootImport   as    BI
import qualified Data.Map as {- blah -}  Foo.Map

import Control.Monad  (   )
import Data.Word (Word8)
import Data.Tree hiding  (  drawTree   )

import qualified Data.Maybe as M hiding    ( maybe  , isJust  )


-- comment
foo = let x = 1
          y = 2
      in x + y

bar = 3
bbb x
 | x == 1 = ()
 | otherwise = ()


aaa [ ] _   = 0
aaa x  _unk = 1

aba () = 0

x `ccc` 1 = x + 1
x `ccc` y = x + y

x !@# y = x + y

data Baz = Baz1 | Baz2

data Ba = Ba | Bb

data Ca = Cc | Cd

pattern Foo a <- RealFoo a
pattern Bar a <- RealBar a

data Thing = RealFoo Thing | RealBar Int
