{-# LANGUAGE Safe #-}

module GHC.Conc.Signal
    (Signal,
     HandlerFun,
     setHandler,
     runHandlers,
     runHandlersPtr
     ) where

import GHC.Internal.Conc.Signal