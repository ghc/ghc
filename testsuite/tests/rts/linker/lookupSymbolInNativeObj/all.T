test('lookupSymbolInNativeObj1',
     [unless(have_dynamic(), skip),
      extra_files(['obj.c'])],
     makefile_test, [])

