{-# LANGUAGE BangPatterns, CPP, DataKinds, MonoLocalBinds, NoExperimentalDependentTypes #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
#ifdef DEEP
{-# LANGUAGE DeepSubsumption #-}
#else
{-# LANGUAGE NoDeepSubsumption #-}
#endif
module Main where

import Data.IORef
import GHC.Types (Multiplicity(..))
import LexicalLetProvider
import LexicalOrdinaryShallow
import LexicalRetainedReader

readCombined :: forall a. Display a => Bool -> OpenPayload a -> String
readCombined tag package =
    let !(value, reader) = relayLinear @Many (forwardLinear @Many) package
    in let apply :: RetainedReader -> String
           apply f = getText (f tag @a (reader value))
       in apply worker

ignoreCombined :: forall a. Display a => Bool -> OpenPayload a -> String
ignoreCombined tag package =
    let ~(value, reader) = relayLinear @Many (forwardLinear @Many) package
    in let apply :: RetainedReader -> String
           apply f = getText (f tag @Bool True)
       in apply worker

main :: IO ()
main = do
  flag <- newIORef True
  first <- readIORef flag
  writeIORef flag False
  second <- readIORef flag
  let bottomTag = error "retained tag"
  print (readCombined bottomTag (makeOpenPayload first True False),
         readCombined bottomTag (makeOpenPayload second 'a' 'q'),
         ignoreCombined @Bool bottomTag (error "package"), shallow)
