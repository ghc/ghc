
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

def myvt ( $args_c, $args_r, $ret_res )
{
   vt ( " -package concurrent -fglasgow-exts " ++ $args_c,
        $args_r, $ret_res )
}

--conc009_RUNTEST_OPTS = -x 1
--conc021_RUNTEST_OPTS = -x 250 -o2 conc021.stderr-mingw

test "conc001" { myvt("", "", "") }
test "conc002" { myvt("", "", "") }
test "conc003" { myvt("", "", "") }
test "conc004" { myvt("", "", "") }
test "conc005" { myvt("", "", "") }
test "conc006" { myvt("", "", "") }
test "conc007" { myvt("", "+RTS -H128M -RTS", "") }
test "conc008" { myvt("", "", "") }
test "conc009" { myvt("", "", "") }
test "conc010" { myvt("", "", "") }
test "conc012" { myvt("", "", "") }
test "conc013" { myvt("", "", "") }
test "conc014" { myvt("", "", "") }
test "conc015" { myvt("", "", "") }
test "conc016" { myvt("", "", "") }
test "conc017" { myvt("", "", "") }
test "conc018" { myvt("", "", "") }
test "conc019" { myvt("", "", "") }

--# conc020 *should* work on mingw32
test "conc020" { skip when $platform == "i386-unknown-mingw32"
                 myvt("", "", "") }

test "conc021" { skip when $platform == "i386-unknown-mingw32"
                 myvt("", "", "") }

test "conc022" { myvt("", "", "") }
test "conc023" { myvt("", "", "") }
test "conc024" { myvt("", "", "") }
test "conc025" { myvt("", "", "") }
test "conc026" { myvt("", "", "") }
test "conc027" { myvt("", "", "") }
test "conc028" { myvt("", "", "") }
test "conc029" { myvt("", "", "") }
test "conc030" { myvt("", "", "") }
test "conc031" { myvt("", "", "") }
