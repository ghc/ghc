{-# OPTIONS_GHC -fwrite-if-simplified-core -fbyte-code-and-object-code -fprefer-byte-code #-}
{-# LANGUAGE TemplateHaskell #-}

module C where

import T22840A
import T22840B
import Control.Monad.IO.Class

$(liftIO $ do
    putStrLn "start"
    putStrLn (disp theT)
    putStrLn "end"
    return [])
