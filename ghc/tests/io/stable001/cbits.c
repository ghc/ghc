#define NULL_REG_MAP
#include "../../../includes/Stg.h"

int
test1( stableIOPtr )
  StgStablePtr stableIOPtr;
{
  int i;
  int result;

  printf("Using stable pointer %x\n", stableIOPtr);

  for( i = 0; i != 10; i = i + 1 ) {
    printf( "Calling stable pointer for %dth time\n", i );
    performIO( stableIOPtr );
    printf( "Returned after stable pointer\n" );
  }

  return 1;
}
