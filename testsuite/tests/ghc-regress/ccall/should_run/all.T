
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "fed001"   { vtr("-fglasgow-exts -package lang", 
                     "", "") }
test "ffi001"   { vtr("-fglasgow-exts -package lang", 
                     "", "") }
