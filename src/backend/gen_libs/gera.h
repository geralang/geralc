
#ifndef GERA_H
#define GERA_H

#include <stdlib.h>

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

#endif