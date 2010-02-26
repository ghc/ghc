
test	('dph-smvm' 
 	, [ reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
	, multimod_compile_and_run 
	, [ 'Main'
	  , '-Odph -package dph-par -package dph-prim-par'])


