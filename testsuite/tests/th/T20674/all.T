setTestOpts(req_interp)
# TH should work with -fexternal-interpreter too
if config.have_ext_interp :
     setTestOpts(extra_ways(['ext-interp']))
     setTestOpts(only_ways(['normal','ghci','ext-interp']))

     if llvm_build():
         setTestOpts(fragile_for(16087, ['ext-interp']))

test('T20674', [expect_broken(20674), extra_files(['Main.hs', 'P.hs', 'foo.c', 'header.h'])], multimod_compile, ['Main', 'P foo.c'])
