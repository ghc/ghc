{-# LANGUAGE FlexibleContexts, DataKinds #-}

module T6054 where

import T6054a

foo = print (Proxy :: Bar '() a => Proxy a)
