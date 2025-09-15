{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module T15433a where

import T15433_aux ( wild )

type T = $wild
