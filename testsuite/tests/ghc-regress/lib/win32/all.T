-- This isn't a very good test to run automatically at the moment, since
-- it doesn't terminate

include ($confdir ++ "/../vanilla-test.T")

-- Use this itsy helper fn to pass in an extra flag
def myvtr($extra_comp_args, $a, $b)
{
   vtr("-package lang -package win32 " ++ $extra_comp_args, $a, $b)
}

-- Only run the tests on Windows
--if $platform == "i386-unknown-mingw32" then 

test "win32001"     { myvtr("","","") }

--fi