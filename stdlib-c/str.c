
#include "gera.h"
#include <locale.h>
#include <errno.h>

size_t codepoint_size(char fb) {
    if((fb & 0b10000000) == 0b00000000) { return 1; }
    if((fb & 0b11100000) == 0b11000000) { return 2; }
    if((fb & 0b11110000) == 0b11100000) { return 3; }
    if((fb & 0b11111000) == 0b11110000) { return 4; }
    return 0;
}

GeraString gera_std_str_substring(GeraString src, gint start, gint end) {
    size_t start_idx = (size_t) start;
    size_t end_idx = (size_t) end;
    size_t start_offset = 0;
    for(size_t i = 0; i < start_idx; i += 1) {
        start_offset += codepoint_size(src.data[start_offset]);
    }
    size_t length = 0;
    for(size_t c = start_idx; c < end_idx; c += 1) {
        length += codepoint_size(src.data[start_offset + length]);
    }
    GeraString result;
    GeraAllocation* allocation = gera___rc_alloc(length, &gera___free_nothing);
    result.allocation = allocation;
    result.length = end_idx - start_idx;
    result.data = allocation->data;
    for(size_t i = 0; i < length; i += 1) {
        result.data[i] = src.data[start_offset + i];
    }
    return result;
}

GeraString gera_std_str_concat(GeraString a, GeraString b) {
    GeraString result;
    GeraAllocation* allocation = gera___rc_alloc(
        a.allocation->size + b.allocation->size, &gera___free_nothing
    );
    result.allocation = allocation;
    result.length = a.length + b.length;
    result.data = allocation->data;
    for(size_t i = 0; i < a.allocation->size; i += 1) {
        result.data[i] = a.data[i];
    }
    for(size_t i = 0; i < b.allocation->size; i += 1) {
        result.data[a.allocation->size + i] = b.data[i];
    }
    return result;
}

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