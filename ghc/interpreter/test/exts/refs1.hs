--!!! Testing Refs
import IOExts

a1 = 
	newIORef 'a'	>>= \ v ->
	readIORef v	>>= \ x ->
	print x

a2 = 
	newIORef 'a'		>>= \ v ->
	writeIORef v 'b'	>>
	readIORef v		>>= \ x ->
	print x

a3 = 
	newIORef 'a'		>>= \ v1 ->
	newIORef 'a'		>>= \ v2 ->
	print (v1 == v1, v1 == v2, v2 == v2)


