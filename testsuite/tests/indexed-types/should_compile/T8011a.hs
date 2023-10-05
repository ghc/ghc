{-# LANGUAGE TypeFamilies #-}
module T8011a ( ToURL(toURL, nullURL, errorURL, URLT) ) where

class ToURL a where
     type URLT a
     toURL :: a -> URLT a
     nullURL :: a
     errorURL :: a -> URLT a
