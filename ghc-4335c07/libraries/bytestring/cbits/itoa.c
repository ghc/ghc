///////////////////////////////////////////////////////////////
// Encoding numbers using ASCII characters                   //
//                                                           //
// inspired by: http://www.jb.man.ac.uk/~slowe/cpp/itoa.html //
///////////////////////////////////////////////////////////////

#include <stdio.h>

// Decimal Encoding
///////////////////

static const char* digits = "0123456789abcdef";

// signed integers
char* _hs_bytestring_int_dec (int x, char* buf)
{
    char c, *ptr = buf, *next_free;
    int x_tmp;

    // we cannot negate directly as  0 - (minBound :: Int) = minBound
    if (x < 0) {
        *ptr++ = '-';
        buf++;
        x_tmp = x;
        x /= 10;
        *ptr++ = digits[x * 10 - x_tmp];
        if (x == 0)
          return ptr;
        else
          x = -x;
    }

    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = digits[x_tmp - x * 10];
    } while ( x );

    // reverse written digits
    next_free = ptr--;
    while (buf < ptr) {
        c       = *ptr;
        *ptr--  = *buf;
        *buf++  = c;
    }
    return next_free;
}

// signed long long ints (64 bit integers)
char* _hs_bytestring_long_long_int_dec (long long int x, char* buf)
{
    char c, *ptr = buf, *next_free;
    long long int x_tmp;

    // we cannot negate directly as  0 - (minBound :: Int) = minBound
    if (x < 0) {
        *ptr++ = '-';
        buf++;
        x_tmp = x;
        x /= 10;
        *ptr++ = digits[x * 10 - x_tmp];
        if (x == 0)
          return ptr;
        else
          x = -x;
    }

    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = digits[x_tmp - x * 10];
    } while ( x );

    // reverse written digits
    next_free = ptr--;
    while (buf < ptr) {
        c       = *ptr;
        *ptr--  = *buf;
        *buf++  = c;
    }
    return next_free;
}

// unsigned integers
char* _hs_bytestring_uint_dec (unsigned int x, char* buf)
{
    char c, *ptr = buf, *next_free;
    unsigned int x_tmp;

    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = digits[x_tmp - x * 10];
    } while ( x );

    // reverse written digits
    next_free = ptr--;
    while (buf < ptr) {
        c       = *ptr;
        *ptr--  = *buf;
        *buf++  = c;
    }
    return next_free;
}

// unsigned long ints
char* _hs_bytestring_long_long_uint_dec (long long unsigned int x, char* buf)
{
    char c, *ptr = buf, *next_free;
    long long unsigned int x_tmp;

    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x /= 10;
        *ptr++ = digits[x_tmp - x * 10];
    } while ( x );

    // reverse written digits
    next_free = ptr--;
    while (buf < ptr) {
        c       = *ptr;
        *ptr--  = *buf;
        *buf++  = c;
    }
    return next_free;
}


// Padded, decimal, positive integers for the decimal output of bignums
///////////////////////////////////////////////////////////////////////

// Padded (9 digits), decimal, positive int:
// We will use it with numbers that fit in 31 bits; i.e., numbers smaller than
// 10^9, as "31 * log 2 / log 10 = 9.33"
void _hs_bytestring_int_dec_padded9 (int x, char* buf)
{
    const int max_width_int32_dec = 9;
    char* ptr = buf + max_width_int32_dec;
    int x_tmp;

    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x /= 10;
        *(--ptr) = digits[x_tmp - x * 10];
    } while ( x );

    // pad beginning
    while (buf < ptr) { *(--ptr) = '0'; }
}

// Padded (19 digits), decimal, positive long long int:
// We will use it with numbers that fit in 63 bits; i.e., numbers smaller than
// 10^18, as "63 * log 2 / log 10 = 18.96"
void _hs_bytestring_long_long_int_dec_padded18 (long long int x, char* buf)
{
    const int max_width_int64_dec = 18;
    char* ptr = buf + max_width_int64_dec;
    long long int x_tmp;

    // encode positive number as little-endian decimal
    do {
        x_tmp = x;
        x /= 10;
        *(--ptr) = digits[x_tmp - x * 10];
    } while ( x );

    // pad beginning
    while (buf < ptr) { *(--ptr) = '0'; }
}


///////////////////////
// Hexadecimal encoding
///////////////////////

// unsigned ints (32 bit words)
char* _hs_bytestring_uint_hex (unsigned int x, char* buf) {
    // write hex representation in reverse order
    char c, *ptr = buf, *next_free;
    do {
        *ptr++ = digits[x & 0xf];
        x >>= 4;
    } while ( x );
    // invert written digits
    next_free = ptr--;
    while(buf < ptr) {
        c      = *ptr;
        *ptr-- = *buf;
        *buf++ = c;
    }
    return next_free;
};

// unsigned long ints (64 bit words)
char* _hs_bytestring_long_long_uint_hex (long long unsigned int x, char* buf) {
    // write hex representation in reverse order
    char c, *ptr = buf, *next_free;
    do {
        *ptr++ = digits[x & 0xf];
        x >>= 4;
    } while ( x );
    // invert written digits
    next_free = ptr--;
    while(buf < ptr) {
        c      = *ptr;
        *ptr-- = *buf;
        *buf++ = c;
    }
    return next_free;
};
