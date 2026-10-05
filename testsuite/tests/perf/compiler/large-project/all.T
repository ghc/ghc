# These tests are supposed to prevent severe performance regressions when
# operating on projects with unusually large numbers of modules.
# Inefficient algorithms whose complexity depends on the number of modules won't
# be noticed when running the test suite or compiling medium size projects.

def large_project_makedepend(herald, num, dep_mode):
    return test(
        f'large-project-makedepend-{herald}-{num}',
        [
            collect_compiler_stats('bytes allocated', 10),
            pre_cmd(f'./large-project.sh {num}'),
            extra_files(['large-project.sh']),
            ignore_stderr,
            windows_skip,
            dep_mode,
            ignore_dep_outputs,
        ],
        make_depend,
        [[f'Mod{i:04d}' for i in range(0, num - 1)], ''],
        )

large_project_makedepend('makefile', 4000 , dep_makefile)
large_project_makedepend('makefile', 10000, dep_makefile)
large_project_makedepend('json',     4000 , dep_json)
large_project_makedepend('json',     10000, dep_json)
