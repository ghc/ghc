module A where

-- when used with '-g' debug generation option
-- '*/*' leaked into a /* comment */ and broke
-- GNU as.
x */* y = 42
