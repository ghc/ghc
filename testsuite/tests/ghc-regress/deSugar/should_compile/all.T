
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

-- Use this itsy helper fn to pass in an extra flag
--def myvtc($extra_comp_args)
--{
--   vtc(" -fno-warn-incomplete-patterns " ++ $extra_comp_args)
--}

test "ds-wildcard" { vtc("") }
test "ds001" { vtc("") }
test "ds002" { vtc("") }
test "ds003" { vtc("") }
test "ds004" { vtc("") }
test "ds005" { vtc("") }
test "ds006" { vtc("") }
test "ds007" { vtc("") }
test "ds008" { vtc("") }
test "ds009" { vtc("") }
test "ds010" { vtc("") }
test "ds011" { vtc("") }
test "ds012" { vtc("") }
test "ds013" { vtc("") }
test "ds014" { vtc("") }
test "ds015" { vtc("") }
test "ds016" { vtc("") }
test "ds017" { vtc("") }
test "ds018" { vtc("") }
test "ds019" { vtc("") }
test "ds020" { vtc("") }
test "ds021" { vtc("") }
test "ds022" { vtc("") }
test "ds023" { vtc("") }
test "ds024" { vtc("") }
test "ds025" { vtc("") }
test "ds026" { vtc("") }
test "ds027" { vtc("") }
test "ds028" { vtc("") }
test "ds029" { vtc("") }
test "ds030" { vtc("") }
test "ds031" { vtc("") }
test "ds032" { vtc("") }
test "ds033" { vtc("") }
test "ds034" { vtc("") }
test "ds035" { vtc("-fglasgow-exts -package lang") }
test "ds036" { vtc("") }
test "ds037" { vtc("") }
test "ds038" { vtc("") }
test "ds039" { vtc("") }
test "ds040" { vtc("") }
test "ds041" { vtc("") }
test "ds042" { vtc("") }
test "ds043" { vtc("") }
test "ds044" { vtc("") }
test "ds045" { vtc("") }
test "ds046" { vtc("-O -funbox-strict-fields") }
test "ds047" { vtc("") }
test "ds048" { vtc("") }
test "ds049" { vtc("-fvia-C -package lang") }
test "ds050" { vtc("-fglasgow-exts") }
test "ds051" { vtc("") }
test "ds052" { $expect = "fail" vtc("") }
test "ds053" { $expect = "fail" vtc("") }
