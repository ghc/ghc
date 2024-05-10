# These tests are supposed to prevent severe performance regressions when
# operating on projects with unusually large numbers of modules.
# Inefficient algorithms whose complexity depends on the number of modules won't
# be noticed when running the test suite or compiling medium size projects.

def large_project_makedepend(num):
    return test(
        f'large-project-makedepend-{num}',
        [
            collect_compiler_stats('bytes allocated', 1),
            pre_cmd(f'./large-project.sh {num}'),
            extra_files(['large-project.sh']),
            ignore_stderr,
            windows_skip,
        ],
        make_depend,
        [[f'Mod{i:04d}' for i in range(0, num - 1)], ''],
        )

large_project_makedepend(4000)
large_project_makedepend(10000)
