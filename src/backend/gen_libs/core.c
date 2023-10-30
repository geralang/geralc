
#include <stdlib.h>

typedef void (*GeraFreeHandler)(char* data);

typedef struct GeraAllocation {
    size_t rc;
    GeraFreeHandler fh;
    char data[];
} GeraAllocation;

GeraAllocation* gera___rc_alloc(size_t size, GeraFreeHandler fh) {
    GeraAllocation* a = (GeraAllocation*) malloc(sizeof(GeraAllocation) + size);
    a->rc = 1;
    a->fh = fh;
    return a;
}

void gera___rc_incr(GeraAllocation* a) {
    a->rc += 1;
}

void gera___rc_free(GeraAllocation* a) {
    (a->fh)(a->data);
    free(a);
}

void gera___rc_decr(GeraAllocation* a) {
    a->rc -= 1;
}

double gera___float_mod(double x, double div) {
    if (div != div || x != x) { return x; }
    if (div == 0) { return (0.0f / 0.0f); }
    return x - (int) (x / div) * div;
}

char gera___string_eq(char* a, char* b) {
    char eq = 0;
    for(unsigned long long int i = 0;; i += 1) {
        if(a[i] != b[i]) { return 0; }
        if(a[i] != '\0') { continue; }
        return 1;
    }
}
