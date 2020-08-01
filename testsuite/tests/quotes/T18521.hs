{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module T18521 where

import GHC.Exts (Int#)
import Language.Haskell.TH

a :: Code Q Int#
a = [|| 42# ||]

b :: CodeQ Int#
b = a

c :: TExpQ Int#
c = examineCode a
