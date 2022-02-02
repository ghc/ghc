test('uniques', [no_deps, extra_files(["checkUniques"])], makefile_test, ['uniques'])

def normalise_nos(s):
  return re.sub(r':\d+:\d+','<line>:<no>', s)

# Check that ls-files works and returns files from the source tree.
# We just check that "hie.yaml" is there because it's top-level (dont have have to deal with
# path differences) and quite unique to GHC.
def has_ls_files() -> bool:
  try:
    files = subprocess.check_output(['git', 'ls-files']).splitlines()
    print (b"hie.yaml" in files)
    return b"hie.yaml" in files
  except subprocess.CalledProcessError:
    return False


test('notes', [no_deps if has_ls_files() else skip
              , req_hadrian_deps(["lint:notes-util"])
              , normalise_fun(normalise_nos) ]
              , makefile_test, ['notes'])
