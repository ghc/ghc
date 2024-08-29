
// Global variable that records which vector registers are supported
// (conservative estimate).
//
// 0: no support for vector registers
// 1: support for 128-bit vector registers
// 2: support for 256-bit vector registers
// 3: support for 512-bit vector registers
extern int vectorSupportGlobalVar;

int checkVectorSupport(void);
void setVectorSupport(void);
