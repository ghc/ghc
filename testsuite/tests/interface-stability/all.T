def check_package(pkg_name):
    test(f'{pkg_name}-exports',
         req_hadrian_deps(['test:dump-decls']),
         makefile_test,
         [f'exports_{pkg_name}'])

check_package('base')
