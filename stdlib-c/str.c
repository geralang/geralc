
#include "gera.h"
#include <locale.h>
#include <errno.h>

gbool parse_success = 0;

gbool gera_std_str_parse_success() {
    return parse_success;
}

gfloat gera_std_str_parse_flt(GeraString parsed) {
    setlocale(LC_NUMERIC, "C");
    GERA_STRING_NULL_TERM(parsed, parsed_nt);
    errno = 0;
    char* end_ptr;
    gfloat result = strtod(parsed_nt, &end_ptr);
    parse_success = errno == 0 && end_ptr == parsed_nt + parsed.allocation->size;
    return result;
}

gint gera_std_str_parse_int(GeraString parsed) {
    setlocale(LC_NUMERIC, "C");
    GERA_STRING_NULL_TERM(parsed, parsed_nt);
    errno = 0;
    char* end_ptr;
    gint result = strtoll(parsed_nt, &end_ptr, 10);
    parse_success = errno == 0 && end_ptr == parsed_nt + parsed.allocation->size;
    return result;
}