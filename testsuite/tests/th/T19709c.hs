{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror #-}

module T19709c where

import Language.Haskell.TH

$( do runIO $ putStrLn "compiling the splice"
      case tail "hello" of "hello" -> return [] )
