def normalise_port(str):
  str = re.sub(r'on port [0-9]+', r'on port ****', str)
  return str

test('remote-iserv'
    , [ reqlib('network')
      , normalise_fun(normalise_port)
      , normalise_errmsg_fun(normalise_port)
      , extra_files(['Main.hs', 'Lib.hs', 'iserv-wrapper', 'Setup.hs'])]
    , makefile_test
    , [])
