
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "fed001"   { vtr("-fglasgow-exts",        "", "") }
test "ffi001"   { vtr("-fglasgow-exts",        "", "") }

-- skip this test for now: we don't have the machinery to compile the .c file
-- separately.
test "ffi002"   { skip when True
		  vtr("ffi002.c -fglasgow-exts -no-hs-main", "", "") }

test "ffi003"   { vtr("-fglasgow-exts", "", "") }
test "ffi004"   { vtr("-fglasgow-exts", "", "") }

