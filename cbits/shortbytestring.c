#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>


int
sbs_memcmp_off(const void *s1,
            size_t off1,
            const void *s2,
            size_t off2,
            size_t n)
{
    const void *s1o = s1 + off1;
    const void *s2o = s2 + off2;

    int r = memcmp(s1o, s2o, n);

    return r;
}

ptrdiff_t
sbs_elem_index(const void *s,
            uint8_t c,
            size_t n)
{
    const void *so = memchr(s, c, n);

    if (so) {
        ptrdiff_t diff = so - s;
        assert(diff >= 0);
        return diff;
    } else {
        return -1;
    }
}
