
#include <geracoredeps.h>
#include <gera.h>

void gera___panic(const char* message);

GeraAllocation* gera___rc_alloc(size_t size, GeraFreeHandler fh) {
    if(size == 0) { return NULL; }
    GeraAllocation* a = (GeraAllocation*) geracoredeps_malloc(
        sizeof(GeraAllocation) + size
    );
    if(a == NULL) { gera___panic("unable to allocate heap memory"); }
    a->rc_mutex = geracoredeps_create_mutex();
    a->data_mutex = geracoredeps_create_mutex();
    a->rc = 1;
    a->size = size;
    a->fh = fh;
    return a;
}

void gera___rc_incr(GeraAllocation* a) {
    if(a == NULL) { return; }
    geracoredeps_lock_mutex(&a->rc_mutex);
    a->rc += 1;
    geracoredeps_unlock_mutex(&a->rc_mutex);
}

void gera___rc_free(GeraAllocation* a) {
    if(a == NULL) { return; }
    (a->fh)(a->data, a->size);
    geracoredeps_free_mutex(&a->rc_mutex);
    geracoredeps_free_mutex(&a->data_mutex);
    geracoredeps_free(a);
}

void gera___rc_decr(GeraAllocation* a) {
    if(a == NULL) { return; }
    geracoredeps_lock_mutex(&a->rc_mutex);
    a->rc -= 1;
    geracoredeps_unlock_mutex(&a->rc_mutex);
    if(a->rc == 0) { gera___rc_free(a); }
}

void gera___rc_lock_read(GeraAllocation* a) {
    if(a == NULL) { return; }
    geracoredeps_lock_mutex(&a->data_mutex);
}

void gera___rc_unlock_read(GeraAllocation* a) {
    if(a == NULL) { return; }
    geracoredeps_unlock_mutex(&a->data_mutex);
}

void gera___rc_lock_write(GeraAllocation* a) {
    if(a == NULL) { return; }
    geracoredeps_lock_mutex(&a->data_mutex);
}

void gera___rc_unlock_write(GeraAllocation* a) {
    if(a == NULL) { return; }
    geracoredeps_unlock_mutex(&a->data_mutex);
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

extern size_t GERA_MAX_CALL_DEPTH;

typedef struct GeraCallEntry {
    const char* name;
    const char* file;
    size_t line;
} GeraCallEntry;

typedef struct GeraTraceEntry {
    GERACORE_THREAD_ID thread;
    GeraCallEntry* trace;
    size_t trace_size;
    size_t call_depth;  
} GeraTraceEntry;

typedef struct GeraTraceEntries {
    GERACORE_MUTEX mutex;
    GeraTraceEntry* entries;
    size_t entries_size;
    size_t entry_count;
} GeraTraceEntries;

static GeraTraceEntries GERA_TRACE_ENTRIES = {
    .entries = NULL,
    .entries_size = 0,
    .entry_count = 0
};

void gera___stack_push(const char* name, const char* file, size_t line) {
    if(GERA_TRACE_ENTRIES.entries == NULL) {
        GERA_TRACE_ENTRIES.mutex = geracoredeps_create_mutex();
        GERA_TRACE_ENTRIES.entries_size = 1;
        GERA_TRACE_ENTRIES.entries = malloc(
            sizeof(GeraTraceEntry) * GERA_TRACE_ENTRIES.entries_size
        );
    }
    geracoredeps_lock_mutex(&GERA_TRACE_ENTRIES.mutex);
    GeraTraceEntry* trace_entry = NULL;
    GERACORE_THREAD_ID calling_thread = geracoredeps_thread_id();
    size_t entry_count = GERA_TRACE_ENTRIES.entry_count;
    for(size_t entry_idx = 0; entry_idx < entry_count; entry_idx += 1) {
        GeraTraceEntry* entry = &GERA_TRACE_ENTRIES.entries[entry_idx];
        if(entry->thread == calling_thread) {
            trace_entry = entry;
            break;
        }
    }
    if(trace_entry == NULL) {
        if(GERA_TRACE_ENTRIES.entry_count >= GERA_TRACE_ENTRIES.entries_size) {
            GERA_TRACE_ENTRIES.entries_size *= 2;
            GERA_TRACE_ENTRIES.entries = realloc(
                GERA_TRACE_ENTRIES.entries, sizeof(GeraTraceEntry) * GERA_TRACE_ENTRIES.entries_size
            );
        }
        size_t trace_size = 32;
        GERA_TRACE_ENTRIES.entries[GERA_TRACE_ENTRIES.entry_count] = (GeraTraceEntry) {
            .thread = calling_thread,
            .call_depth = 0,
            .trace = malloc(sizeof(GeraCallEntry) * trace_size),
            .trace_size = trace_size
        };
        trace_entry = &GERA_TRACE_ENTRIES.entries[GERA_TRACE_ENTRIES.entry_count];
        GERA_TRACE_ENTRIES.entry_count += 1;
    }
    if(trace_entry->call_depth >= trace_entry->trace_size) {
        trace_entry->trace_size *= 2;
        trace_entry->trace = realloc(
            trace_entry->trace, sizeof(GeraCallEntry) * trace_entry->trace_size
        );
    }
    trace_entry->trace[trace_entry->call_depth] = (GeraCallEntry) {
        .name = name,
        .file = file,
        .line = line
    };
    trace_entry->call_depth += 1;
    if(trace_entry->call_depth > GERA_MAX_CALL_DEPTH) {
        gera___panic("Maximum call depth exceeded!");
    }
    geracoredeps_unlock_mutex(&GERA_TRACE_ENTRIES.mutex); 
}

void gera___stack_pop() {
    if(GERA_TRACE_ENTRIES.entries == NULL) { return; }
    geracoredeps_lock_mutex(&GERA_TRACE_ENTRIES.mutex);
    GeraTraceEntry* trace_entry = NULL;
    GERACORE_THREAD_ID calling_thread = geracoredeps_thread_id();
    size_t entry_count = GERA_TRACE_ENTRIES.entry_count;
    for(size_t entry_idx = 0; entry_idx < entry_count; entry_idx += 1) {
        GeraTraceEntry* entry = &GERA_TRACE_ENTRIES.entries[entry_idx];
        if(entry->thread == calling_thread) {
            trace_entry = entry;
            break;
        }
    }
    if(trace_entry == NULL) { goto unlock; }
    trace_entry->call_depth -= 1;
    unlock:
    geracoredeps_unlock_mutex(&GERA_TRACE_ENTRIES.mutex); 
}

#if defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__))
    #define ERROR_NOTE_COLOR "\033[0;90m"
    #define ERROR_MESSAGE_COLOR "\033[0;91;1m"
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

void gera___panic_pre_at(const char* file, size_t line) {
    size_t line_str_len = geracoredeps_display_uint_length(line);
    char line_str[line_str_len + 1];
    geracoredeps_display_uint(line, line_str);
    line_str[line_str_len] = '\0';
    geracoredeps_eprint("The program panicked (at \"");
    geracoredeps_eprint(file);
    geracoredeps_eprint("\":");
    geracoredeps_eprint(line_str);
    geracoredeps_eprint("): " ERROR_MESSAGE_COLOR);
}

void gera___panic_pre() {
    geracoredeps_eprint("The program panicked: " ERROR_MESSAGE_COLOR);
}

void gera___panic_post() {
    geracoredeps_eprint("\n");
    if(GERA_TRACE_ENTRIES.entries == NULL) { goto notrace; }
    GeraTraceEntry* trace_entry = NULL;
    GERACORE_THREAD_ID calling_thread = geracoredeps_thread_id();
    size_t entry_count = GERA_TRACE_ENTRIES.entry_count;
    for(size_t entry_idx = 0; entry_idx < entry_count; entry_idx += 1) {
        GeraTraceEntry* entry = &GERA_TRACE_ENTRIES.entries[entry_idx];
        if(entry->thread != calling_thread) { continue; }
        trace_entry = entry;
        break;
    }
    if(trace_entry == NULL) { goto notrace; }
    if(trace_entry->call_depth == 0) { goto notrace; }
    geracoredeps_eprint(ERROR_NOTE_COLOR "Stack trace (latest call first):\n");
    for(size_t call_idx = trace_entry->call_depth - 1;;) {
        GeraCallEntry* call_entry = &trace_entry->trace[call_idx];
        size_t call_idx_str_len = geracoredeps_display_uint_length(call_idx);
        char call_idx_str[call_idx_str_len + 1];
        geracoredeps_display_uint(call_idx, call_idx_str);
        call_idx_str[call_idx_str_len] = '\0';
        geracoredeps_eprint(ERROR_INDEX_COLOR " ");
        geracoredeps_eprint(call_idx_str);
        geracoredeps_eprint(" " ERROR_PROCEDURE_COLOR);
        geracoredeps_eprint(call_entry->name);
        geracoredeps_eprint(ERROR_NOTE_COLOR " at " ERROR_FILE_NAME_COLOR);
        geracoredeps_eprint(call_entry->file);
        geracoredeps_eprint(":");
        size_t line_str_len = geracoredeps_display_uint_length(call_entry->line);
        char line_str[line_str_len + 1];
        geracoredeps_display_uint(call_entry->line, line_str);
        line_str[line_str_len] = '\0';
        geracoredeps_eprint(line_str);
        geracoredeps_eprint(ERROR_NOTE_COLOR "\n");
        if(call_idx == 0) { break; }
        call_idx -= 1;
    }
    notrace:
    geracoredeps_eprint(ERROR_RESET_COLOR);
    geracoredeps_exit(1);
}

void gera___panic(const char* message) {
    gera___panic_pre();
    geracoredeps_eprint(message);
    gera___panic_post();
}

size_t gera___verify_index(gint index, size_t size, const char* file, size_t line) {
    size_t final_index;
    if(index < 0) { final_index = (size_t) (((gint) size) + index); }
    else { final_index = (size_t) index; }
    if(final_index < size) { return final_index; }
    size_t index_str_len = geracoredeps_display_sint_length(index);
    char index_str[index_str_len + 1];
    geracoredeps_display_sint(index, index_str);
    index_str[index_str_len] = '\0';
    size_t length_str_len = geracoredeps_display_uint_length(size);
    char length_str[length_str_len + 1];
    geracoredeps_display_uint(size, length_str);
    length_str[length_str_len] = '\0';
    gera___panic_pre_at(file, line);
    geracoredeps_eprint("the index ");
    geracoredeps_eprint(index_str);
    geracoredeps_eprint(" is out of bounds for an array of length ");
    geracoredeps_eprint(length_str);
    gera___panic_post();
    return -1;
}

void gera___verify_integer_divisor(gint d, const char* file, size_t line) {
    if(d != 0) { return; }
    gera___panic_pre_at(file, line);
    geracoredeps_eprint("integer division by zero");
    gera___panic_post();
}

GeraString gera___alloc_string(const char* data) {
    size_t length = 0;
    size_t length_bytes = 0;
    for(; data[length_bytes] != '\0'; length += 1) {
        length_bytes += gera___codepoint_size(data[length_bytes]);
    }
    GeraAllocation* allocation = gera___rc_alloc(
        length_bytes, &gera___free_nothing
    );
    for(size_t c = 0; c < length_bytes; c += 1) {
        allocation->data[c] = data[c];
    }
    return (GeraString) {
        .allocation = allocation,
        .data = allocation->data,
        .length_bytes = length_bytes,
        .length = length
    };
}

GeraString gera___wrap_static_string(const char* data) {
    size_t length = 0;
    size_t length_bytes = 0;
    for(; data[length_bytes] != '\0'; length += 1) {
        length_bytes += gera___codepoint_size(data[length_bytes]);
    }
    return (GeraString) {
        .allocation = NULL,
        .data = data,
        .length = length,
        .length_bytes = length_bytes
    };
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
    result.data = src.data + start_offset;
    return result;
}

GeraString gera___concat(GeraString a, GeraString b) {
    GeraAllocation* allocation = gera___rc_alloc(
        a.length_bytes + b.length_bytes, &gera___free_nothing
    );
    GeraString result;
    result.allocation = allocation;
    result.length = a.length + b.length;
    result.length_bytes = a.length_bytes + b.length_bytes;
    result.data = allocation->data;
    for(size_t i = 0; i < a.length_bytes; i += 1) {
        allocation->data[i] = a.data[i];
    }
    for(size_t i = 0; i < b.length_bytes; i += 1) {
        allocation->data[a.length_bytes + i] = b.data[i];
    }
    return result;
}

gint gera___hash(unsigned char* data, size_t data_len) {
    size_t hash = 0;
    for(size_t i = 0; i < data_len; i += 1) {
        hash = data[i] + (hash << 6) + (hash << 16) - hash;
    }
    return *((gint*) &hash);
}

GeraArray GERA_ARGS;
void gera___set_args(int argc, char** argv) {
    GERA_ARGS.allocation = gera___rc_alloc(
        sizeof(GeraString) * argc, &gera___free_nothing
    );
    GERA_ARGS.data = GERA_ARGS.allocation->data;
    GERA_ARGS.length = argc;
    for(size_t i = 0; i < argc; i += 1) {
        ((GeraString*) GERA_ARGS.data)[i] = gera___wrap_static_string(argv[i]);
    }
}
