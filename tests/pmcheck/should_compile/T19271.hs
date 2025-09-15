{-# OPTIONS_GHC -Wincomplete-patterns #-}
module T19271 where

data Roles = RolesUnsubscribed Bool
           | RolesPartner Int
           | RolesAnonymousClient Char
           | RolesAnonymousVendor ()

doingThing :: Roles -> ()
doingThing roles =
    case roles of
