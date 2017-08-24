///////////////////////////////////////////////////////////////
// Encoding numbers using ASCII characters                   //
//                                                           //
// inspired by: http://www.jb.man.ac.uk/~slowe/cpp/itoa.html //
///////////////////////////////////////////////////////////////

#include <stdint.h>

// Decimal Encoding
///////////////////

static const char* digits = "0123456789abcdef";

static const char* digits_upper = "0123456789ABCDEF";

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

// signed 64 bit integers
char* _hs_bytestring_int64_dec (int64_t x, char* buf)
{
    char c, *ptr = buf, *next_free;
    int64_t x_tmp;

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

// unsigned 64 bit integers
char* _hs_bytestring_uint64_dec (uint64_t x, char* buf)
{
    char c, *ptr = buf, *next_free;
    uint64_t x_tmp;

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

// Padded (19 digits), decimal, positive int64_t:
// We will use it with numbers that fit in 63 bits; i.e., numbers smaller than
// 10^18, as "63 * log 2 / log 10 = 18.96"
void _hs_bytestring_int64_dec_padded18 (int64_t x, char* buf)
{
    const int max_width_int64_dec = 18;
    char* ptr = buf + max_width_int64_dec;
    int64_t x_tmp;

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
char* _hs_bytestring_uint32_hex (uint32_t x, char* buf) {
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

// unsigned 64 bit integers
char* _hs_bytestring_uint64_hex (uint64_t x, char* buf) {
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

// unsigned ints (32 bit words)
void _hs_bytestring_builder_uint32_fixed_width_hex (int width,
                                                    uint32_t x,
                                                    char* buf) {
    while (--width >= 0) {
      buf[width] = digits[x & 0xf];
      x >>= 4;
    }
};

// unsigned ints (64 bit words)
void _hs_bytestring_builder_uint64_fixed_width_hex (int width,
                                                    uint64_t x,
                                                    char* buf) {
    while (--width >= 0) {
      buf[width] = digits[x & 0xf];
      x >>= 4;
    }
};

// unsigned ints (32 bit words)
char* _hs_bytestring_uint32_hex_upper (uint32_t x, char* buf) {
    // write hex representation in reverse order
    char c, *ptr = buf, *next_free;
    do {
        *ptr++ = digits_upper[x & 0xf];
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
char* _hs_bytestring_uint64_hex_upper (uint64_t x, char* buf) {
    // write hex representation in reverse order
    char c, *ptr = buf, *next_free;
    do {
        *ptr++ = digits_upper[x & 0xf];
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

// unsigned ints (32 bit words)
void _hs_bytestring_builder_uint32_fixed_width_hex_upper (int width,
                                                          uint32_t x,
                                                          char* buf) {
    while (--width >= 0) {
      buf[width] = digits_upper[x & 0xf];
      x >>= 4;
    }
};

// unsigned ints (64 bit words)
void _hs_bytestring_builder_uint64_fixed_width_hex_upper (int width,
                                                          uint64_t x,
                                                          char* buf) {
    while (--width >= 0) {
      buf[width] = digits_upper[x & 0xf];
      x >>= 4;
    }
};
