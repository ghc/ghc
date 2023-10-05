test('codes', [ req_ls_files
              , req_hadrian_deps(["lint:codes"]) ]
            , makefile_test, ['codes'])
