
#include "gera.h"

gint gera_std_bitwise_and(gint a, gint b) {
    return a & b;
}

gint gera_std_bitwise_or(gint a, gint b) {
    return a | b;
}

gint gera_std_bitwise_xor(gint a, gint b) {
    return a ^ b;
}

gint gera_std_bitwise_not(gint x) {
    return ~x;
}

gint gera_std_bitwise_rshift(gint x, gint bits);

gint gera_std_bitwise_lshift(gint x, gint bits) {
    if(bits < 0) return gera_std_bitwise_rshift(x, -bits);
    return x << bits;
}

gint gera_std_bitwise_rshift(gint x, gint bits) {
    if(bits < 0) return gera_std_bitwise_lshift(x, -bits);
    return x >> bits;
}