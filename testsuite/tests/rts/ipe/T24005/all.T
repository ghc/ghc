
# Remove the ipId field as it's volatile (depends on e.g. architecture and may change with every new GHC version)
def normalise_InfoProv_ipId(str):
     return re.sub('ipId = \\d*', '', str)

test('t24005', [ js_skip, normalise_fun(normalise_InfoProv_ipId) ], compile_and_run, ['-finfo-table-map -fdistinct-constructor-tables'])
