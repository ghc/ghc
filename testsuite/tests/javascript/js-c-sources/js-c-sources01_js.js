//#OPTIONS:CPP
//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_hello_c
//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_write_c,_alloc_c,_modify_c
//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_callback_c
//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_free,_strlen,_malloc

function hello_c_wrapper(a) {
    return _hello_c(a);
}

function write_c_wrapper(a,o) {
    h$withCStringOnHeap(a,o, (ptr) => {
        _write_c(ptr)
    });
}

function alloc_c_wrapper() {
    const ptr = _alloc_c();
    const a = h$copyCStringFromHeap(ptr);
    _free(ptr);
    RETURN_ADDR(a,0);
}

function modify_c_wrapper(a,o) {
    const len = h$strlen(a,o);
    h$withOutBufferOnHeap(a, o, len, (ptr) => {
        _modify_c(ptr);
    });
}

function callback_c_wrapper(a,o,f_ptr,f_o) {
    const cb_c = h$registerFunPtrOnHeap(f_ptr, f_o, false, 'ii', (cb) => {
        // we return the function that will actually be called by the C code.
        // This is a wrapper to call the Haskell function (cb).
        //
        // Here it's simple because we only have 'char' arguments and results
        // but with other arguments it could have to copy data from/to the heap
        // (e.g. if CStrings were involved).
        //
        // 'ii' is the type of the function, according to Emscripten (return one
        // int, take one int as argument).
        //
        // Finally we pass `false` because we don't need to unregister the
        // callback asynchronously as would be the case for a `destructor`-like
        // callback. We unregister it below explicitly after its use.
        return function(arg) {
            return cb(arg);
        };
    });
    const len = h$strlen(a,o);
    h$withOutBufferOnHeap(a, o, len, (ptr) => {
        _callback_c(ptr, cb_c);
    });

    h$unregisterFunPtrFromHeap(cb_c);
}
