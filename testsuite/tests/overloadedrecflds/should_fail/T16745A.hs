module T16745A where

import T16745B        (field)  -- imports both 'field's
import T16745D hiding (foo)    -- allowed, hides both 'foo' fields

foo = foo

wrong = field -- ambiguous which 'field' is meant
