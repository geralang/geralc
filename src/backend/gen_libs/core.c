
#ifndef GERA_H
#include "gera.h" 
#endif

#include <stdio.h>

GeraAllocation* gera___rc_alloc(size_t size, GeraFreeHandler fh) {
    GeraAllocation* a = (GeraAllocation*) malloc(sizeof(GeraAllocation) + size);
    a->rc = 1;
    a->size = size;
    a->fh = fh;
    //printf("GC: %p | alloc!\n", a);
    return a;
}

void gera___rc_incr(GeraAllocation* a) {
    a->rc += 1;
    //printf("GC: %p | rc++ => %llu\n", a, a->rc);
}

void gera___rc_free(GeraAllocation* a) {
    (a->fh)(a->data, a->size);
    free(a);
    //printf("GC: %p | free!\n", a);
}

void gera___rc_decr(GeraAllocation* a) {
    a->rc -= 1;
    //printf("GC: %p | rc-- => %llu\n", a, a->rc);
    if(a->rc == 0) { gera___rc_free(a); }
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

void gera___free_nothing(char* data, size_t size) {}