// Turns off globbing for MingW, this is the same as that
// CRT_noglob.o, but avoids having to locate CRT_nogob.o in the
// filesystem.
unsigned long _CRT_glob = 0;
