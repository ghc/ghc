include ($confdir ++ "/../vanilla-test.T")

def lang_vtr($extra_comp_args, $a, $b)
{
   vtr("-package lang " ++ $extra_comp_args, $a, $b)
}

test "addr001" 		{ lang_vtr("","","") }
test "char001"  	{ vtr("","","") }
test "dynamic001"  	{ lang_vtr("","","") }
test "dynamic002"  	{ lang_vtr("","","") }
test "enum01"  		{ lang_vtr("-cpp","","") }
test "enum02"  		{ lang_vtr("-cpp","","") }
test "enum03"  		{ lang_vtr("-cpp","","") }
test "enum04"  		{ $expect = "fail" lang_vtr("","","") }
test "exceptions001"  	{ lang_vtr("","","") }
test "list001"		{ lang_vtr("","","") }
test "memo001"		{ lang_vtr("-package util","+RTS -A10k -G1 -RTS","") }
test "memo002"		{ lang_vtr("-package util","20","") }
test "packedstring001"  { lang_vtr("","","") }
test "rand001"		{ vtr("","","") }
test "show001"		{ vtr("","","") }
test "stableptr001"	{ lang_vtr("","+RTS -K4m -RTS","") }
test "stableptr003"	{ lang_vtr("","","") }
test "stableptr004"	{ lang_vtr("","+RTS -K4m -RTS","") }
test "text001"		{ vtr("","","") }
test "weak001"		{ lang_vtr("-fglasgow-exts","","") }
