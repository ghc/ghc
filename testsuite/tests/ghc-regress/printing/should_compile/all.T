
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

def myvtc ( $stuff )
{
   vtc( " -dcore-lint -hi-with-declarations -hi-with-exports " ++
        " -hi-with-instances -hi-with-fixities -ddump-tc " ++ 
        $stuff )
}

test "Print001" { myvtc("") }
test "Print002" { myvtc("-fno-implicit-prelude") }
test "Print003" { myvtc("") }
test "Print004" { myvtc("") }


