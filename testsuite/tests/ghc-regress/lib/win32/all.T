-- This isn't a very good test to run automatically at the moment, since
-- it doesn't terminate

include ($confdir ++ "/../vanilla-test.T")

-- Use this itsy helper fn to pass in an extra flag
def myvtr($extra_comp_args, $a, $b)
{
   -- Only run the tests on Windows
   skip when $platform /= "i386-unknown-mingw32" && 
	     $platform /= "i386-unknown-cygwin32"
   vtr("-package lang -package win32 " ++ $extra_comp_args, $a, $b)
}

test "win32001"     { myvtr("","","") }
