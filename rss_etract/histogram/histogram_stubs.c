#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/stat.h>

#include "murmur3.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

typedef struct Histogram {

    uint64_t *keys;
    uint64_t *values;
    uint64_t len;
    uint64_t sum;

} Histogram;

char Histogram_load(char *fname, Histogram *res) {
    struct stat st;
    if (stat(fname, &st) != 0) return 1;

    FILE *inf = fopen(fname, "r");
    if (!inf) return 1;

    size_t fsize = st.st_size;
    void *buffer = malloc(fsize);
    if (buffer == 0) return 1;
    size_t read_size = fread(buffer, fsize, 1, inf);
    fclose(inf);

    size_t n_elements = read_size / sizeof(uint64_t) / 2;
    uint64_t *values = buffer + (read_size / 2);

    uint64_t sum = 0;
    size_t idx = n_elements;
    while(idx > 0) {
        sum += values[idx];
        idx--;
    }
    
    res->keys = buffer;
    res->values = values;
    res->len = n_elements;
    res->sum = sum;

    return 0;

}

void Histogram_free(Histogram h) {
    free(h.keys);
}

ssize_t Histogram_idx(Histogram h, uint64_t k) {
    size_t len = h.len;
    ssize_t right = len;
    ssize_t left = 0;

    while (right > left && left >= 0 && right <= len) {
        size_t mid = left + (right - left) / 2;
        if (h.keys[mid] == k) {
            return h.keys[mid];
        } else if (mid < k) {
            left = mid;
        } else {
            right = mid;
        }
    }

    return -1;
}

char Histogram_get(Histogram h, uint64_t k, int64_t *res) {
    ssize_t idx = Histogram_idx(h, k);
    if (idx < 0) {
        return 1;
    } else {
        *res = h.values[idx];
        return 0;
    }
}

/* ocaml interface */

#define Histogram_val(v) (*((Histogram *) Data_custom_val(v)))

void histogram_finalize(value h) {
    Histogram h_val = Histogram_val(h);
    Histogram_free(h_val);
}

static struct custom_operations histogram_ops = {
  "fr.inria.caml.histograms",
  histogram_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value call_histogram_load(value fname) {
    char *fname_buf = String_val(fname);
    Histogram res;
    if (Histogram_load(fname_buf, &res) != 0) caml_failwith("failed to load file");
    value v = alloc_custom(&histogram_ops, sizeof(Histogram), 0, 1);
    Histogram_val(v) = res;
    return v;
}

value call_histogram_get(value h, value k) {
    CAMLparam2(h, k);
    Histogram h_val = Histogram_val(h);

    char *k_buf = String_val(k);
    size_t k_size = caml_string_length(k);
    uint64_t hash[2];
    uint64_t seed = 42;
    uint64_t hash_k;

    MurmurHash3_x64_128(k_buf, k_size, seed, hash);
    hash_k = hash[0] ^ hash[1];

    int64_t v;
    if (Histogram_get(h_val, hash_k, &v) != 0) {
        v = 0;
    }
    CAMLreturn(Val_long(v));
}

value call_histogram_sum(value h) {
    CAMLparam1(h);
    Histogram h_val = Histogram_val(h);
    uint64_t res = h_val.sum;
    CAMLreturn(Val_long(res));
}
