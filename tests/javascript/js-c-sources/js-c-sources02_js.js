//#OPTIONS:CPP
//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_foo_c
//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_malloc,_free,_strlen

function foo(str_d,str_o) {
    h$withCStringOnHeap(str_d,str_o, (ptr) => _foo_c(ptr));
}
