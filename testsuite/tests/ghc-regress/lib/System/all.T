include ($confdir ++ "/../vanilla-test.T")

test "exitWith001"	{ vtr("","","42") }
test "getArgs001"	{ vtr("","","") }
test "getEnv001"	{ vtr("","","") }
test "system001"	{ vtr("","","") }
