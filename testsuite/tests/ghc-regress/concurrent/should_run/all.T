
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero
--                  whether to normalise (depathify) stderr output

def myvtr ( $args_c, $args_r, $ret_res )
{
   vtr ( " -fglasgow-exts " ++ $args_c,
         $args_r, $ret_res )
}

test "conc001" { myvtr("", "", "") }
test "conc002" { myvtr("", "", "") }
test "conc003" { myvtr("", "", "") }
test "conc004" { myvtr("", "", "") }
test "conc005" { myvtr("", "", "") }
test "conc006" { myvtr("", "", "") }
test "conc007" { myvtr("", "+RTS -H128M -RTS", "") }
test "conc008" { myvtr("", "", "") }
test "conc009" { myvtr("", "", "1") }
test "conc010" { myvtr("", "", "") }
test "conc012" { myvtr("", "", "") }
test "conc013" { myvtr("", "", "") }
test "conc014" { myvtr("", "", "") }
test "conc015" { myvtr("", "", "") }
test "conc016" { myvtr("", "", "") }
test "conc017" { myvtr("", "", "") }
test "conc018" { myvtr("", "", "") }
test "conc019" { myvtr("", "", "") }
test "conc020" { myvtr("", "", "") }
test "conc021" { $normalise_errmsg = True myvtr("", "", "254") }
test "conc022" { myvtr("", "", "") }
test "conc023" { myvtr("", "", "") }
test "conc024" { myvtr("", "", "") }
test "conc025" { myvtr("", "", "") }
test "conc026" { myvtr("", "", "") }
test "conc027" { myvtr("", "", "") }
test "conc028" { myvtr("", "", "") }
test "conc029" { myvtr("", "", "") }
test "conc030" { myvtr("", "+RTS -K2M -RTS", "") }
test "conc031" { myvtr("", "", "") }
test "conc032" { myvtr("", "", "") }
test "conc033" { myvtr("", "", "") }
test "conc034" { myvtr("", "+RTS -C0 -RTS", "") }
test "conc035" { myvtr("", "", "") }
test "conc036" { myvtr("", "", "") }
test "conc037" { myvtr("", "", "") }
