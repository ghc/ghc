{-# LANGUAGE ForeignFunctionInterface, CApiFFI, GHCForeignImportPrim,
             QuasiQuotes, TemplateHaskell, JavaScriptFFI, MagicHash,
             UnliftedFFITypes #-}

module TH_foreignCallingConventions where

import GHC.Prim
import Control.Applicative
import Language.Haskell.TH
import System.IO
import Foreign.Ptr

$( do let fi cconv safety lbl name ty =
            ForeignD (ImportF cconv safety lbl name ty)
      dec1 <- fi CCall      Interruptible "&"   (mkName "foo") <$> [t| Ptr () |]
      dec2 <- fi Prim       Safe          "bar" (mkName "bar") <$> [t| Int# -> Int# |]
      -- the declarations below would result in warnings or errors when returned
      dec3 <- fi CApi       Unsafe        "baz" (mkName "baz") <$> [t| Double -> IO () |]
      dec4 <- fi StdCall    Safe          "bay" (mkName "bay") <$> [t| (Int -> Bool) -> IO Int |]
      dec5 <- fi JavaScript Unsafe        "bax" (mkName "bax") <$> [t| Ptr Int -> IO String |]
      runIO $
        mapM_ (putStrLn . pprint) [dec1, dec2, dec3, dec4, dec5] >> hFlush stdout
      return [dec1, dec2]
 )
