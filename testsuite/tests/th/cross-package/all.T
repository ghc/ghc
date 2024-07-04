def cross_test(suf):
    name = f'CrossPackage{suf}'
    test(
        name,
        [
            extra_files([
                'Cross.hs',
                'CrossLocal.hs',
                'CrossDep.hs',
                'CrossDepApi.hs',
                'CrossNum.hs',
                'dep.conf',
                'prep.bash',
                'run.bash',
            ]),
        ],
        makefile_test,
        [name],
    )

cross_test('Archive')
cross_test('EmptyArchive')
cross_test('NoArchive')
