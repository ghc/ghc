
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "fed001"   { vtr("-fglasgow-exts",        "", "") }
test "ffi001"   { vtr("-fglasgow-exts",        "", "") }

-- skip this test for now: we don't have the machinery to compile the .c file
-- separately.
test "ffi002"   { vtr("ffi002_c.c -fglasgow-exts -no-hs-main", "", "") }

test "ffi003"   { skip when $platform == "alpha-dec-osf3" -- no NCG on Alpha yet
		  vtr("-fglasgow-exts", "", "") }

test "ffi004"   { vtr("-fglasgow-exts", "", "") }

-- skip this test for now: it is non-portable due to the use of literal values
-- instead of CPP symbols for the flag arguments to open().
test "ffi005"   { skip when True
	 	  vtr("-fglasgow-exts", "", "3") }

test "ffi006"   { vtr("-fglasgow-exts", "", "") }
test "ffi007"   { vtr("-fglasgow-exts", "", "") }
test "ffi008"   { vtr("-fglasgow-exts", "", "1") }
