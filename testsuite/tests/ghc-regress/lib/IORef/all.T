include ($confdir ++ "/../vanilla-test.T")

-- Use this itsy helper fn to pass in an extra flag
def myvtr($extra_comp_args, $a, $b)
{
   vtr("-package lang " ++ $extra_comp_args, $a, $b)
}

test "ioref001"		{ myvtr("","+RTS -K16m -RTS","") }

