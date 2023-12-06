test('size_hello_obj', [collect_size(5, 'size_hello_obj.o')], compile, [''])

test('size_hello_artifact', [collect_size(5, 'size_hello_artifact' + exe_extension())],
                             compile_artifact, [''])

test('libdir',[collect_size_dir(10, config.libdir)], static_stats, [] )
