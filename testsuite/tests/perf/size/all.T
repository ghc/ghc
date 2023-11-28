test('size_hello_obj', [collect_size(5, 'size_hello_obj.o')], compile, [''])

test('libdir',[stat_from_file('size', 10, 'SIZE')], makefile_test, ['libdir_size'] )
