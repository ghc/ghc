test    ('dph-ExportList-vseg-fast' 
        , [ extra_clean(['ExportList.o', 'ExportList.hi'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile
        , [ 'ExportList'
          , '-O0 -package dph-lifted-vseg'])
