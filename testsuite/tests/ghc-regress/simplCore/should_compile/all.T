
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

test "simpl001" { vtc("") }
test "simpl002" { vtc("") }
test "simpl003" { vtc("") }
test "simpl004" { vtc("-package lang") }
test "simpl005" { vtc("") }

-- these two are really a multi-module test
test "Simpl006Help" { vtc("-O") }
test "simpl006" { vtc("-package concurrent") }

test "simpl007" { vtc("") }
test "simpl008" { vtc("-fglasgow-exts") }

-- these two are really a multi-module test
test "Simpl009Help" { vtc("-O") }
test "simpl009" { vtc("-O") }
