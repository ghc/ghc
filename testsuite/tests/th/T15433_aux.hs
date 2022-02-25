{-# LANGUAGE TemplateHaskell #-}

module T15433_aux ( wild ) where

import Language.Haskell.TH.Syntax
  ( Q, Type )

wild :: Q Type
wild = [t| _ |]
