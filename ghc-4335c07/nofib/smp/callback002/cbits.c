#include "cbits.h"

int count;

void callC( FUNC* f) {
   int i;
   for(i=0;i<count;i++) f();
}
