
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

-- Use this itsy helper fn to pass in an extra flag
--def myvtc($extra_comp_args)
--{
--   vtc(" -fno-warn-incomplete-patterns " ++ $extra_comp_args)
--}

--Simpl006Help.hs
--Simpl006Help_HC_OPTS = -O

test "simpl001" { vtc("") }
test "simpl002" { vtc("") }
test "simpl003" { vtc("") }
test "simpl004" { vtc("-package lang") }
test "simpl005" { vtc("") }
test "simpl006" { vtc("-package concurrent") }
test "simpl007" { vtc("") }
