libdph-trace.dylib: dph-trace.h dph-trace.c
	gcc -dynamiclib -o $@ -current_version 1.0 -compatibility_version 1.0 dph-trace.c
	install_name_tool -id `pwd`/libdph-trace.dylib $@

%.h: %.d
	dtrace -h -s $^

clean:
	rm -f libdph-trace.dylib dph-trace.h dph-trace.o

