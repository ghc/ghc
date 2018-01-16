{-# LANGUAGE TypeFamilies #-}

module T10891 where

import Language.Haskell.TH
import System.IO

class C a where
  f :: a -> Int

class C' a where
  type F a :: *
  type F a = a
  f' :: a -> Int

class C'' a where
  data Fd a :: *

instance C' Int where
  type F Int = Bool
  f' = id

instance C'' Int where
  data Fd Int = B Bool | C Char

$(return [])

test :: ()
test =
  $(let
      display :: Name -> Q ()
      display q = do
        i <- reify q
        runIO (hPutStrLn stderr (pprint i) >> hFlush stderr)
    in do
      display ''C
      display ''C'
      display ''C''
      [| () |])
