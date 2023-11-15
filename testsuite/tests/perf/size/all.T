test('size_hello_obj', [collect_size(3, 'size_hello_obj.o')], compile, [''])

test('libdir',[stat_from_file('size', 3, 'SIZE')], makefile_test, ['libdir_size'] )
