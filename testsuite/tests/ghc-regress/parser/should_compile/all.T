
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

test "read001" { vtc("") }
test "read002" { vtc("") }
test "read003" { vtc("") }
test "read004" { vtc("") }
test "read005" { vtc("") }
test "read006" { vtc("") }
test "read007" { vtc("") }
test "read008" { vtc("") }
test "read009" { vtc("") }
test "read010" { vtc("") }
test "read011" { vtc("") }
test "read012" { vtc("-O -funfold-casms-in-hi-file -fglasgow-exts -ohi T1.hi") }
test "read013" { vtc("-O -funfold-casms-in-hi-file -fglasgow-exts") }
test "read014" { vtc("-Wall") }
test "read015" { vtc("") }
test "read016" { vtc("") }
test "read017" { vtc("") }
test "read018" { vtc("") }
test "read019" { vtc("") }
test "read020" { vtc("") }
test "read021" { vtc("") }
test "read022" { vtc("-fglasgow-exts") }
test "read023" { vtc("") }
test "read024" { vtc("-fglasgow-exts") }
test "read025" { vtc("") }
test "read026" { vtc("") }
test "read027" { vtc("") }
test "read028" { vtc("") }
test "read029" { vtc("") }
test "read030" { vtc("") }
