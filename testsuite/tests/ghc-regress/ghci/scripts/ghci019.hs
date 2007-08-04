-- Trac #1581
-- Even though Eq is not in scope unqualified, we want to 
-- see the Eq instance of Foo when we say :i Foo

module Foo where
import qualified Prelude

data Foo = Foo
instance Prelude.Eq Foo
