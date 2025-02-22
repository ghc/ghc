{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
module Language.Haskell.TH.Syntax
  ( module GHC.Boot.TH.Syntax
  , makeRelativeToProject
  , module GHC.Boot.TH.Lift
  , addrToByteArrayName
  , addrToByteArray
  )
where

import GHC.Boot.TH.Syntax
import GHC.Boot.TH.Lift
import System.FilePath
import Data.Array.Byte
import GHC.Exts
import GHC.ST

-- This module completely re-exports 'GHC.Boot.TH.Syntax',
-- and exports additionally functions that depend on filepath.

-- | The input is a filepath, which if relative is offset by the package root.
makeRelativeToProject :: FilePath -> Q FilePath
makeRelativeToProject fp | isRelative fp = do
  root <- getPackageRoot
  return (root </> fp)
makeRelativeToProject fp = return fp

-- The following two defintions are copied from 'Data.Byte.Array'
-- in order to preserve the old export list of 'TH.Syntax'.
-- They will soon be removed as part of #24782.

addrToByteArrayName :: Name
addrToByteArrayName = 'addrToByteArray

addrToByteArray :: Int -> Addr# -> ByteArray
addrToByteArray (I# len) addr = runST $ ST $
  \s -> case newByteArray# len s of
    (# s', mb #) -> case copyAddrToByteArray# addr mb 0# len s' of
      s'' -> case unsafeFreezeByteArray# mb s'' of
        (# s''', ret #) -> (# s''', ByteArray ret #)

