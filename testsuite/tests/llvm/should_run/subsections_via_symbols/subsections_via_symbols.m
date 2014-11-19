#import <Foundation/Foundation.h>
#import "HsFFI.h"
#import "SymbolsViaSections_stub.h"

int
main(int argc, char * argv[]) {
  hs_init(&argc, &argv);
  atexit(&hs_exit);
  test(10);
  return EXIT_SUCCESS;
}
