
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "callback" { vt("-fglasgow-exts -package lang -fvia-C callback_stub.o", 
                     "", "") }
test "fed001"   { vt("-fglasgow-exts -package lang", 
                     "", "") }
test "ffi001"   { vt("-fglasgow-exts -package lang", 
                     "", "") }
