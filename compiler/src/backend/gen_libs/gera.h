
#ifndef GERA_H
#define GERA_H

#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

typedef void (*GeraFreeHandler)(char* data, size_t size);

typedef struct GeraAllocation {
    size_t rc;
    size_t size;
    GeraFreeHandler fh;
    char data[];
} GeraAllocation;

typedef struct GeraArray {
    GeraAllocation* allocation;
    size_t length;
    char* data;
} GeraArray;

typedef struct GeraString {
    GeraAllocation* allocation;
    size_t length;
    size_t length_bytes;
    const char* data;
} GeraString;

typedef double gfloat;
typedef int64_t gint;
typedef char gbool;

#define GERA_STRING_NULL_TERM(s_name, d_name) \
    char d_name[s_name.length_bytes + 1]; \
    for(size_t c = 0; c < s_name.length_bytes; c += 1) { \
        d_name[c] = s_name.data[c]; \
    } \
    d_name[s_name.length_bytes] = '\0';

GeraAllocation* gera___rc_alloc(size_t size, GeraFreeHandler fh);
void gera___rc_incr(GeraAllocation* a);
void gera___rc_decr(GeraAllocation* a);
void gera___free_nothing(char* data, size_t size);
GeraString gera___alloc_string(const char* data);
GeraString gera___wrap_static_string(const char* data);
size_t gera___codepoint_size(char fb);
void gera___panic(const char* reason);

extern GeraArray GERA_ARGS;

#endif