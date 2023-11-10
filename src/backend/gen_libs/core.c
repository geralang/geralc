
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

char gera___string_eq(char* a, char* b) {
    char eq = 0;
    for(unsigned long long int i = 0;; i += 1) {
        if(a[i] != b[i]) { return 0; }
        if(a[i] != '\0') { continue; }
        return 1;
    }
}

void gera___free_nothing(char* data, size_t size) {}

#define ERROR_NOTE_COLOR "\033[0;90m"
#define ERROR_MESSAGE_COLOR "\033[0;91m"
#define ERROR_INDEX_COLOR "\033[0;90m"
#define ERROR_PROCEDURE_COLOR "\033[0;32;1m"
#define ERROR_FILE_NAME_COLOR "\033[0;37m"
#define ERROR_RESET_COLOR "\033[0m"

void gera___panic(char* message) {
    printf(ERROR_NOTE_COLOR "The program panicked: " ERROR_MESSAGE_COLOR "%s\n", message);
    printf(ERROR_NOTE_COLOR "Stack trace (latest call last):\n");
    for(size_t i = 0; i < stack_trace_size; i += 1) {
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