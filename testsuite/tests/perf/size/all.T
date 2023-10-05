test('size_hello_obj', [collect_size(5, 'size_hello_obj.o')], compile, [''])

test('libdir',[collect_size_dir(10, config.libdir)], static_stats, [] )
