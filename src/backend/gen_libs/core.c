
#ifndef GERA_H
#include "gera.h" 
#endif

#include <stdio.h>
#include <errno.h>
#include <locale.h>

void gera___panic(const char* message);

GeraAllocation* gera___rc_alloc(size_t size, GeraFreeHandler fh) {
    GeraAllocation* a = (GeraAllocation*) malloc(sizeof(GeraAllocation) + size);
    if(a == NULL) { gera___panic("unable to allocate memory for object, array or closure"); }
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

typedef struct GeraStackItem {
    const char* name;
    const char* file;
    size_t line;
} GeraStackItem;

GeraStackItem* stack_trace = NULL;
size_t stack_trace_msize = 32;
size_t stack_trace_size = 0;

void gera___st_init() {
    stack_trace = (GeraStackItem*) malloc(sizeof(GeraStackItem) * stack_trace_msize);
}

void gera___st_push(const char* name, const char* file, size_t line) {
    if(stack_trace_size >= stack_trace_msize) {
        stack_trace_msize *= 2;
        stack_trace = (GeraStackItem*) realloc(
            stack_trace, sizeof(GeraStackItem) * stack_trace_msize
        );
    }
    stack_trace[stack_trace_size] = (GeraStackItem) { .name = name, .file = file, .line = line };
    stack_trace_size += 1;
}

void gera___st_pop() {
    stack_trace_size -= 1;
}

double gera___float_mod(double x, double div) {
    if (div != div || x != x) { return x; }
    if (div == 0) { return (0.0f / 0.0f); }
    return x - (int) (x / div) * div;
}

char gera___string_eq(GeraString a, GeraString b) {
    if(a.length_bytes != b.length_bytes) { return 0; }
    for(size_t i = 0; i < a.length_bytes; i += 1) {
        if(a.data[i] != b.data[i]) { return 0; }
    }
    return 1;
}

void gera___free_nothing(char* data, size_t size) {}

#ifndef _WIN32
    #define ERROR_NOTE_COLOR "\033[0;90m"
    #define ERROR_MESSAGE_COLOR "\033[0;91m"
    #define ERROR_INDEX_COLOR "\033[0;90m"
    #define ERROR_PROCEDURE_COLOR "\033[0;32;1m"
    #define ERROR_FILE_NAME_COLOR "\033[0;37m"
    #define ERROR_RESET_COLOR "\033[0m"
#else
    #define ERROR_NOTE_COLOR ""
    #define ERROR_MESSAGE_COLOR ""
    #define ERROR_INDEX_COLOR ""
    #define ERROR_PROCEDURE_COLOR ""
    #define ERROR_FILE_NAME_COLOR ""
    #define ERROR_RESET_COLOR ""
#endif

void gera___panic(const char* message) {
    printf("The program panicked: " ERROR_MESSAGE_COLOR "%s\n", message);
    printf(ERROR_NOTE_COLOR "Stack trace (latest call first):\n");
    for(size_t i = stack_trace_size - 1;;) {
        GeraStackItem* si = &stack_trace[i];
        printf(
            ERROR_INDEX_COLOR " %lld " ERROR_PROCEDURE_COLOR "%s",
            i, si->name
        );
        printf(
            ERROR_NOTE_COLOR " at " ERROR_FILE_NAME_COLOR "%s:%lld" ERROR_NOTE_COLOR "\n",
            si->file,
            si->line
        );
        if(i == 0) { break; }
        i -= 1;
    }
    printf(ERROR_RESET_COLOR);
    exit(1);
}

#define INVALID_ARRAY_IDX_ERROR_MSG "the index %lld is out of bounds for an array of length %lld"

void gera___verify_index(gint index, size_t size) {
    if(index >= 0 && ((size_t) index) < size) { return; }
    size_t error_message_length = snprintf(NULL, 0, INVALID_ARRAY_IDX_ERROR_MSG, index, size);
    char error_message[error_message_length + 1];
    sprintf(error_message, INVALID_ARRAY_IDX_ERROR_MSG, index, size);
    gera___panic(error_message);
}

#define INVALID_INTEGER_DIVISOR "integer division by zero"

void gera___verify_integer_divisor(gint d) {
    if(d != 0) { return; }
    gera___panic(INVALID_INTEGER_DIVISOR);
}

GeraString gera___alloc_string(const char* data) {
    size_t size = 0;
    for(;;size += 1) {
        if(data[size] == '\0') { break; }
    }
    GeraString result;
    GeraAllocation* allocation = gera___rc_alloc(size, &gera___free_nothing);
    result.allocation = allocation;
    result.length = 0;
    result.data = allocation->data;
    result.length_bytes = size;
    for(size_t c = 0; c < size; c += 1) {
        char cu = data[c];
        if((cu & 0b10000000) == 0b00000000
        || (cu & 0b11000000) == 0b11000000) {
            result.length += 1;
        }
        result.data[c] = cu;
    }
    return result;
}

size_t gera___codepoint_size(char fb) {
    if((fb & 0b10000000) == 0b00000000) { return 1; }
    if((fb & 0b11100000) == 0b11000000) { return 2; }
    if((fb & 0b11110000) == 0b11100000) { return 3; }
    if((fb & 0b11111000) == 0b11110000) { return 4; }
    return 0;
}

GeraString gera___substring(GeraString src, gint start, gint end) {
    size_t start_idx = (size_t) start;
    size_t end_idx = (size_t) end;
    size_t start_offset = 0;
    for(size_t i = 0; i < start_idx; i += 1) {
        start_offset += gera___codepoint_size(src.data[start_offset]);
    }
    size_t length_bytes = 0;
    for(size_t c = start_idx; c < end_idx; c += 1) {
        length_bytes += gera___codepoint_size(src.data[start_offset + length_bytes]);
    }
    gera___rc_incr(src.allocation);
    GeraString result;
    result.allocation = src.allocation;
    result.length = end_idx - start_idx;
    result.length_bytes = length_bytes;
    result.data = src.allocation->data + start_offset;
    return result;
}

GeraString gera___concat(GeraString a, GeraString b) {
    GeraString result;
    GeraAllocation* allocation = gera___rc_alloc(
        a.length_bytes + b.length_bytes, &gera___free_nothing
    );
    result.allocation = allocation;
    result.length = a.length + b.length;
    result.length_bytes = a.length_bytes + b.length_bytes;
    result.data = allocation->data;
    for(size_t i = 0; i < a.length_bytes; i += 1) {
        result.data[i] = a.data[i];
    }
    for(size_t i = 0; i < b.length_bytes; i += 1) {
        result.data[a.length_bytes + i] = b.data[i];
    }
    return result;
}

gbool gera___parse_success = 0;

gfloat gera___parse_flt(GeraString parsed) {
    setlocale(LC_NUMERIC, "C");
    GERA_STRING_NULL_TERM(parsed, parsed_nt);
    errno = 0;
    char* end_ptr;
    gfloat result = strtod(parsed_nt, &end_ptr);
    gera___parse_success = errno == 0 && end_ptr == parsed_nt + parsed.length_bytes;
    return result;
}

gint gera___parse_int(GeraString parsed) {
    setlocale(LC_NUMERIC, "C");
    GERA_STRING_NULL_TERM(parsed, parsed_nt);
    errno = 0;
    char* end_ptr;
    gint result = strtoll(parsed_nt, &end_ptr, 10);
    gera___parse_success = errno == 0 && end_ptr == parsed_nt + parsed.length_bytes;
    return result;
}