
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtcf are: extra compile flags

test "tcfail001" { vtcf("") }
test "tcfail002" { vtcf("") }
test "tcfail003" { vtcf("") }
test "tcfail004" { vtcf("") }
test "tcfail005" { vtcf("") }
test "tcfail006" { vtcf("") }
test "tcfail007" { vtcf("") }
test "tcfail008" { vtcf("") }
test "tcfail009" { vtcf("") }
test "tcfail010" { vtcf("") }
test "tcfail011" { vtcf("") }
test "tcfail012" { vtcf("") }
test "tcfail013" { vtcf("") }
test "tcfail014" { vtcf("") }
test "tcfail015" { vtcf("") }
test "tcfail016" { vtcf("") }
test "tcfail017" { vtcf("") }
test "tcfail018" { vtcf("") }
test "tcfail019" { vtcf("") }
test "tcfail020" { vtcf("") }
test "tcfail021" { vtcf("") }
test "tcfail023" { vtcf("") }
test "tcfail027" { vtcf("") }
test "tcfail028" { vtcf("") }
test "tcfail029" { vtcf("") }
test "tcfail030" { vtcf("") }
test "tcfail031" { vtcf("") }
test "tcfail032" { vtcf("") }
test "tcfail033" { vtcf("") }
test "tcfail034" { vtcf("") }
test "tcfail035" { vtcf("") }
test "tcfail036" { vtcf("") }
test "tcfail037" { vtcf("") }
test "tcfail038" { vtcf("") }
test "tcfail040" { vtcf("") }
test "tcfail042" { vtcf("") }
test "tcfail043" { vtcf("") }
test "tcfail044" { vtcf("") }
test "tcfail045" { vtcf("-fglasgow-exts -package lang") }
test "tcfail046" { vtcf("") }
test "tcfail047" { vtcf("") }
test "tcfail048" { vtcf("") }
test "tcfail049" { vtcf("") }
test "tcfail050" { vtcf("") }
test "tcfail051" { vtcf("") }
test "tcfail052" { vtcf("") }
test "tcfail053" { vtcf("") }
test "tcfail054" { vtcf("") }
test "tcfail055" { vtcf("") }
test "tcfail056" { vtcf("") }
test "tcfail057" { vtcf("") }
test "tcfail058" { vtcf("") }
test "tcfail061" { vtcf("") }
test "tcfail062" { vtcf("") }
test "tcfail063" { vtcf("") }
test "tcfail065" { vtcf("") }
test "tcfail067" { vtcf("") }
test "tcfail068" { vtcf("-fglasgow-exts -package lang") }
test "tcfail069" { vtcf("") }
test "tcfail070" { vtcf("") }
test "tcfail071" { vtcf("") }
test "tcfail072" { vtcf("") }
test "tcfail073" { vtcf("") }
test "tcfail075" { vtcf("") }
test "tcfail076" { vtcf("") }
test "tcfail077" { vtcf("") }
test "tcfail078" { vtcf("") }
test "tcfail079" { vtcf("") }
test "tcfail080" { $expect = "fail" 
                   vtcf("-fglasgow-exts -package lang") }
test "tcfail082" { vtcf("") }
test "tcfail083" { vtcf("") }
test "tcfail084" { vtcf("") }
test "tcfail085" { vtcf("") }
test "tcfail086" { vtcf("") }
test "tcfail087" { vtcf("") }
test "tcfail088" { vtcf("") }
test "tcfail089" { vtcf("-O") }
test "tcfail090" { vtcf("") }
test "tcfail091" { vtcf("") }
test "tcfail092" { vtcf("") }
test "tcfail093" { vtcf("") }
test "tcfail094" { vtcf("") }
test "tcfail095" { vtcf("") }
test "tcfail096" { vtcf("") }
test "tcfail097" { vtcf("") }

-- The compiler should reject tc098, but it doesn't.  Hence
-- this is an expected failure.
test "tcfail098" { $expect = "fail" 
                   vtcf("") }

test "tcfail099" { vtcf("") }

test "tcfail100" { vtcf("") }
test "tcfail101" { vtcf("") }
test "tcfail102" { vtcf("") }
test "tcfail103" { vtcf("") }
test "tcfail104" { vtcf("") }
test "tcfail105" { vtcf("") }
test "tcfail106" { vtcf("") }
test "tcfail107" { vtcf("") }
