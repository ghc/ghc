include ($confdir ++ "/../vanilla-test.T")

test "net001"	{ if $platform == "i386-unknown-mingw32" then $expect = "fail" fi
                  vtr("-package concurrent -package net","","") }
test "uri001"	{ vtr("-package network","","") }
test "net002"   { vtr("-package net", "3", "") }
