{-# LANGUAGE TypeApplications #-}

module T14158 where

import qualified Control.Category as Cat

foo = (Cat.id @(->) >>=)
