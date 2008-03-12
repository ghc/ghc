#include "ffi014_cbits.h"

void callC( FUNC* f) {
   int i;
   for(i=0;i<1000;i++) f();
}
