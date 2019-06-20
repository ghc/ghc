test('T16521', extra_files( \
    [ 'A.hs' \
    , 'a.h' \
    , 'b.h' \
    , 'b2.h' \
    , 'check.sh'
    ]), makefile_test, [])
