
#include <stdio.h>
#include "gera.h"

void gera_std_io_println(GeraString line) {
    GERA_STRING_NULL_TERM(line, line_nt);
    puts(line_nt);
}