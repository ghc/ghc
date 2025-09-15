{-# LANGUAGE TemplateHaskell #-}

module T24299 where
import Language.Haskell.TH.Syntax (addModFinalizer, runIO)
import GHC.Types (Type)
import System.IO

type Proxy :: forall a. a -> Type
data Proxy a = MkProxy

check :: ($(addModFinalizer (runIO (do putStrLn "check"; hFlush stdout)) >>
  [t| Proxy |]) :: Type -> Type) Int -- There is kind signature, we are in check mode
check = MkProxy

infer :: ($(addModFinalizer (runIO (do putStrLn "infer"; hFlush stdout)) >>
  [t| Proxy |]) ) Int -- no kind signature, inference mode is enabled
infer = MkProxy
