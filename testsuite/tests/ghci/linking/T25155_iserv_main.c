#include <ghcversion.h>
#include <rts/PosixSource.h>
#include <Rts.h>
#include <RtsSymbols.h>
#include <HsFFI.h>

int main (int argc, char *argv[])
{
    RtsConfig conf = defaultRtsConfig;

    // We never know what symbols GHC will look up in the future, so
    // we must retain CAFs for running interpreted code.
    conf.keep_cafs = 1;

    conf.rts_opts_enabled = RtsOptsAll;
    extern StgClosure ZCMain_main_closure;
    hs_main(argc, argv, &ZCMain_main_closure, conf);
}

// Inject "foobar" in the linker symbol table.
//
// The target object isn't compiled against any object defining foobar, yet we
// can use the FFI call to foobar in a TH splice.

#define SymI_HasProto(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    (void*)(&(vvv)), STRENGTH_NORMAL, SYM_TYPE_CODE },


void foobar(int x) {
    printf("Called foobar with %d\n", x);
}

RtsSymbolVal extra_syms[] = {
    SymI_HasProto(foobar)
    { NULL, NULL, STRENGTH_NORMAL, SYM_TYPE_CODE } /* sentinel */
  };

/* Extend the list of built-in symbols */
RtsSymbolVal* rtsExtraSyms() {
    return extra_syms;
}
