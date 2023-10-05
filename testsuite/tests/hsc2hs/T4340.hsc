module Main where

#include <stdint.h>

#def typedef struct { uint8_t a; uint64_t b; uint16_t c; } eight;

main :: IO ()
main = print #{alignment eight}
