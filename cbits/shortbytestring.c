#include <assert.h>
#include <stddef.h>
#include <string.h>


int
_memcmp_off(const void *s1,
            size_t len1,
            size_t off1,
            const void *s2,
            size_t len2,
            size_t off2,
            size_t n)
{
    assert(off1 + n <= len1);
    assert(off2 + n <= len2);

    const void *s1o = s1 + off1;
    const void *s2o = s2 + off2;

    int r = memcmp(s1o, s2o, n);

    return r;
}

ptrdiff_t
_elem_index(const void *s,
            int c,
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
