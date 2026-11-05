# Vendored Python packages for the testsuite driver

Pure-Python dependencies that the driver cannot expect from the system:
the testsuite must run offline from a source tree, with nothing but a
Python interpreter installed. Each package lives in its own subdirectory,
copied verbatim from a pinned upstream release together with its licence
file; `testlib.py` puts this directory on `sys.path`.

Rules:

- Never edit a vendored package in place. Fix things upstream; until the
  fix is released, keep the local change as `patches/<package>-*.patch`
  (`git format-patch` style, paths relative to the repository root), which
  the update script applies after copying.
- To update a package: bump the tag and commit in `update-<package>.sh`,
  run it, review `git diff`, commit. Re-running the script on a clean tree
  must produce no diff.

## filecheck

A Python clone of LLVM's FileCheck (Apache-2.0,
<https://github.com/AntonLydike/filecheck>), used by `filecheck_stderr`
and `filecheck_stdout` in `testlib.py`. Deviations from LLVM's tool are
listed in the upstream README; the main one is Python regex syntax in
`{{...}}` and `[[...]]`. Requires Python 3.10 or later.
