test('galois_raytrace', [extra_files(['CSG.hs', 'Construct.hs', 'Data.hs', 'Eval.hs', 'Geometry.hs', 'Illumination.hs', 'Intersections.hs', 'Interval.hs', 'Main.hs', 'Misc.hs', 'Parse.hs', 'Primitives.hs', 'Surface.hs', 'galois.gml']),
                         when(fast(), skip)],
     multimod_compile_and_run, ['Main', '-package parsec'])
