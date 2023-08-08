
# Copied from linters/all.T:
def has_ls_files() -> bool:
  try:
    files = subprocess.check_output(['git', 'ls-files']).splitlines()
    return b"hie.yaml" in files
  except subprocess.CalledProcessError:
    return False

test('codes', [ normal if has_ls_files() else skip
              , req_hadrian_deps(["lint:codes"]) ]
            , makefile_test, ['codes'])
