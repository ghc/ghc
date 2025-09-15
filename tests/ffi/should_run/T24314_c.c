#include <stddef.h>
#include <stdint.h>




typedef void (*PtrAll64)(uint64_t p1, uint64_t p2, uint64_t p3, uint64_t p4, uint64_t p5, uint64_t p6, uint64_t p7, uint64_t p8);
typedef void (*PtrOne32)(uint64_t p1, uint64_t p2, uint32_t p3, uint64_t p4, uint64_t p5, uint64_t p6, uint64_t p7, uint64_t p8);
typedef void (*PtrOneF)(uint64_t p1, uint64_t p2, float p3, uint64_t p4, uint64_t p5, uint64_t p6, uint64_t p7, uint64_t p8);
typedef void (*PtrTwo32)(uint64_t p1, uint32_t p2, uint32_t p3, uint64_t p4, uint64_t p5, uint64_t p6, uint64_t p7, uint64_t p8);

typedef void (*PtrAllKinds)(uint8_t p1, uint16_t p2, uint32_t p3, uint64_t p4, float p5, double p6,
                            uint8_t p11, uint16_t p12, uint32_t p13, uint64_t p14, float p15, double p16,
                            uint8_t p21, uint16_t p22, uint32_t p23, uint64_t p24, float p25, double p26,
                            uint8_t p31, uint16_t p32, uint32_t p33, uint64_t p34, float p35, double p36);


void callMe(PtrAll64 ptrAll64, PtrOne32 ptrOne32, PtrOneF ptrOneF, PtrTwo32 ptrTwo32, PtrAllKinds ptrAllKinds)
{
  (*ptrAll64)(1,2,3,4,5,6,7,8);
  (*ptrOne32)(1,2,3,4,5,6,7,8);
  (*ptrTwo32)(1,2,3,4,5,6,7,8);
  (*ptrOneF)(1,2,3,4,5,6,7,8);
  (*ptrAllKinds) (1,2,3,4,5,6,
                  11,12,13,14,15,16,
                  21,22,23,24,25,26,
                  31,32,33,34,35,36
                  );

}
