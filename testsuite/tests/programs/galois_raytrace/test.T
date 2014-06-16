setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

# Floating point differences on x86 using the NCG
if config.platform.startswith('i386-') and \
       config.platform != 'i386-unknown-openbsd':
    setTestOpts(expect_fail_for(['hpc','optasm','profasm','threaded2','profthreaded']))

test('galois_raytrace',
     [when(fast(), skip),
      extra_clean(['CSG.hi',           'CSG.o',
                   'Construct.hi',     'Construct.o',
                   'Data.hi',          'Data.o',
                   'Eval.hi',          'Eval.o',
                   'Geometry.hi',      'Geometry.o',
                   'Illumination.hi',  'Illumination.o',
                   'Intersections.hi', 'Intersections.o',
                   'Interval.hi',      'Interval.o',
                   'Main.hi',          'Main.o',
                   'Misc.hi',          'Misc.o',
                   'Parse.hi',         'Parse.o',
                   'Primitives.hi',    'Primitives.o',
                   'Surface.hi',       'Surface.o']),
      reqlib('parsec')],
     multimod_compile_and_run,
     ['Main','-package parsec'])

