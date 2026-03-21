// Minimal module with an initializer and finalizer.
// The compiler places the function pointers in .init_array/.fini_array
// (ELF) or __mod_init_func/__mod_term_func (Mach-O).
//
// The counter lives in the main binary so it survives after this
// object is unloaded.

extern int init_counter;

__attribute__((constructor))
static void lib_init(void) {
    init_counter++;
}

__attribute__((destructor))
static void lib_fini(void) {
    init_counter--;
}
