include ($confdir ++ "/../vanilla-test.T")

-- Use this itsy helper fn to pass in an extra flag
def myvtr($extra_comp_args, $a, $b)
{
   vtr("-package lang " ++ $extra_comp_args, $a, $b)
}

test "echo001"		{ $stdin = "echo001.hs" myvtr("","","") }
test "hTell001"		{ myvtr("","","") }
test "hTell002"		{ myvtr("","","") }
test "performGC001"	{ myvtr("","","") }
test "trace001"		{ myvtr("","","") }
test "hGetBuf001"       { myvtr("","","") }
test "hPutBuf001"       { myvtr("","","") }
test "hPutBuf002"       { myvtr("","","") }
