--!!! instances with no binds;
--!!! be sure we get a legit .hi file
--
module Bar where

import ClassFoo

instance Foo Int

instance Foo a => Foo [a]
