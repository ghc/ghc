def cross_test(suf, files = []):
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
                'CrossNum.hs-boot',
                'CrossObj.hs',
                'dep.conf',
                'obj.conf',
                'prep.bash',
                'run.bash',
            ] + files),
            use_specs({'stdout': 'CrossPackage.stdout'}),
        ],
        makefile_test,
        [name],
    )

cross_test('Archive')
cross_test('EmptyArchive')
cross_test('NoArchive')
cross_test('ArchiveObjCode')
cross_test('MultiUnit', ['unit1', 'unit2'])
