
#include <stdio.h>
#include "gera.h"

void gera_std_io_print(GeraString line) {
    GERA_STRING_NULL_TERM(line, line_nt);
    printf("%s", line_nt);
}