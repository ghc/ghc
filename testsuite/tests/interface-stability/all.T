def check_package(pkg_name):
    test(f'{pkg_name}-exports',
         [req_hadrian_deps(['test:dump-decls']),
          normalise_version(pkg_name),
          normalise_version('ghc-internal'),
         ],
         makefile_test,
         [f'exports_{pkg_name}'])

check_package('base')
check_package('ghc-experimental')
check_package('template-haskell')
check_package('ghc-prim')
