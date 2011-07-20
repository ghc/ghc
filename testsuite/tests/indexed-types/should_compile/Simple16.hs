{-# LANGUAGE TypeFamilies #-}
-- submitted by g9ks157k@acme.softbase.org as #1713
module TypeFamilyBug where

type family TestFamily a :: *

type instance TestFamily () = [()]

testFunction :: value -> TestFamily value -> ()
testFunction = const (const ())

testApplication :: ()
testApplication = testFunction () (return ())