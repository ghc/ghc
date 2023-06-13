def normalise_paths(s):
  res = []
  for line in s.splitlines():
    lib_name = re.search(r'(HS[a-z-]+)-', line)
    if lib_name:
      res.append(lib_name.group(0))
    else:
      res.append(line)
  return '\n'.join(res)


test('T20604', [ req_th
               , js_broken(23013)
               , extra_files(['A.hs', 'A1.hs'])
               , normalise_fun(normalise_paths)], makefile_test, [])
