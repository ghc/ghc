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
            use_specs({'stdout': 'CrossPackage.stdout'}),
        ],
        makefile_test,
        [name],
    )

cross_test('Archive')
cross_test('EmptyArchive')
cross_test('NoArchive')
cross_test('ArchiveObjCode')
