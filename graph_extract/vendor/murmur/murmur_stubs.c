#include <stdint.h>

#include "murmur3.h"


#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

value call_murmur_hash(value inp) {
    CAMLparam1(inp);
    size_t k_size = caml_string_length(inp);
    char *k_buf = String_val(inp);

    uint64_t hash[2];
    uint64_t seed = 42;
    uint64_t hash_k;

    MurmurHash3_x64_128(k_buf, k_size, seed, hash);
    hash_k = hash[0] ^ hash[1];

    CAMLreturn(Val_long(hash_k));
}