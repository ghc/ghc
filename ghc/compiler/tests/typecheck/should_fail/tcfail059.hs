--!! The tycon export shouldn't be allowed to succeed
--
module Foo ( Bar(..) ) where { data Bar = Bar X; data X = Y }
