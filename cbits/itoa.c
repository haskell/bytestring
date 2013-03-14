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
