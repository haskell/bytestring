#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>


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
