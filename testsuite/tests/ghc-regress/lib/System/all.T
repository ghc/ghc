include ($confdir ++ "/../vanilla-test.T")

test "exitWith001"	{ vtr("","","42") }
test "getArgs001"	{ $normalise_output = True vtr("","","") }
test "getEnv001"	{ vtr("","","") }
test "system001"	{ skip when $platform == "i386-unknown-mingw32" vtr("","","") }
