test('uniques', [no_deps, extra_files(["checkUniques"])], makefile_test, ['uniques'])

def normalise_nos(s):
  return re.sub(r':\d+:\d+','<line>:<no>', s)

# Check that ls-files works and returns files from the source tree.
# We just check that "hie.yaml" is there because it's top-level (dont have have to deal with
# path differences) and quite unique to GHC.
def has_ls_files() -> bool:
  try:
    files = subprocess.check_output(['git', 'ls-files']).splitlines()
    return b"hie.yaml" in files
  except subprocess.CalledProcessError:
    return False

test('makefiles', [ no_deps if has_ls_files() else skip
                  , extra_files(["regex-linters"]) ]
                , makefile_test, ['makefiles'])

test('changelogs', [ no_deps if has_ls_files() else skip
                   , extra_files(["regex-linters"]) ]
                 , makefile_test, ['changelogs'])

test('cpp', [ no_deps if has_ls_files() else skip
            , extra_files(["regex-linters"]) ]
            , makefile_test, ['cpp'])

test('version-number', [ no_deps if has_ls_files() else skip
                       , extra_files(["regex-linters"]) ]
                     , makefile_test, ['version-number'])

test('notes', [ no_deps if has_ls_files() else skip
              , req_hadrian_deps(["lint:notes"])
              , normalise_fun(normalise_nos) ]
            , makefile_test, ['notes'])

test('whitespace', [ no_deps if has_ls_files() else skip
                   , req_hadrian_deps(["lint:whitespace"]) ]
                 , makefile_test, ['whitespace'])
