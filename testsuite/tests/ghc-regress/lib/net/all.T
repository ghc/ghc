include ($confdir ++ "/../vanilla-test.T")

test "net001"	{ vtr("-package concurrent -package net","","") }
test "uri001"	{ vtr("-package network","","") }
test "net002"   { vtr("-package net", "3", "") }
