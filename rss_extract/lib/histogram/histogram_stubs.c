#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
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

/* file format

    uint64_t: n_dists
    uint64_t[n_dists]: dist sizes

    for each dist:
        uint64_t[dist_size / 2]: sorted hashes
        uint64_t[dist_size / 2]: counts


*/

#define ERR_STAT 1
#define ERR_MALLOC 2
#define ERR_FILESIZE 3
#define ERR_OPEN 4
#define ERR_READ 5

const char *error_messages[5] = {
    "stat failed",
    "malloc failed",
    "file size incorrect",
    "open failed",
    "read failed"
};

char Histogram_load(char *fname, Histogram **res_hists, size_t *res_n) {
    struct stat st;
    if (stat(fname, &st) != 0) return ERR_STAT;
    
    FILE *inf = fopen(fname, "r");
    if (!inf) return ERR_OPEN;

    size_t fsize = st.st_size;

    uint64_t n_dists;
    if (fread(&n_dists, sizeof(n_dists), 1, inf) < 1) return ERR_READ;

    uint64_t *dist_sizes = malloc(sizeof(uint64_t) * n_dists);
    if (!dist_sizes) return ERR_MALLOC;

    if (fread(dist_sizes, sizeof(uint64_t), n_dists, inf) < n_dists) return ERR_READ;

    uint64_t total_dist_size = 0;
    for (size_t i=0; i < n_dists; i++) {
        total_dist_size += dist_sizes[i] * sizeof(uint64_t);
    }

    size_t computed_size = (total_dist_size*2 + n_dists*sizeof(uint64_t) + sizeof(uint64_t));

    if (computed_size != fsize) {
        return ERR_FILESIZE;
    }

    Histogram *res = malloc(sizeof(Histogram) * n_dists);

    if (!res) return ERR_MALLOC;

    for (size_t i=0; i < n_dists; i++) {
        Histogram *hist = res + (sizeof(Histogram) * i);
        uint64_t dist_size = dist_sizes[i];
        uint64_t *buffer = malloc(dist_size * sizeof(uint64_t) * 2);

        if (!buffer) return ERR_MALLOC;

        if (fread(buffer, sizeof(uint64_t), dist_size*2, inf) < dist_size*2) return ERR_READ;

        uint64_t *hashes = buffer;
        uint64_t *values = &(buffer[dist_size]);
        uint64_t sum = 0;

        for (size_t j=0; j < dist_size; j++) {
            sum += values[j];
        }

        hist->keys = hashes;
        hist->values = values;
        hist->len = dist_size;
        hist->sum = sum;
    }

    *res_hists = res;
    *res_n = n_dists;

    return 0;

}

void Histogram_free(Histogram h) {
    free(h.keys);
}

ssize_t Histogram_idx(Histogram h, uint64_t k) {
    size_t len = h.len;
    ssize_t right = len-1;
    ssize_t left = 0;

    while (left <= right) {
        size_t mid = (left + right) / 2;
        uint64_t v = h.keys[mid];
        if (v == k) {
            return mid;
        } else if (v < k) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    return -1;
}

char Histogram_get(Histogram h, uint64_t k, int64_t *res) {
    ssize_t idx = Histogram_idx(h, k);
    if (idx < 0) {
        *res = 0;
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

value _wrap_histogram(Histogram *h) {
    value res = alloc_custom(&histogram_ops, sizeof(Histogram), 0, 1);
    Histogram_val(res) = *h;
    return res;
}

value call_histogram_load(value fname) {
    char *fname_buf = String_val(fname);
    Histogram *hists;
    uint64_t n_hists;
    char status = Histogram_load(fname_buf, &hists, &n_hists);
    if (status != 0) {
        caml_failwith(error_messages[status - 1]);
    }

    Histogram **hist_pointers = malloc(sizeof(Histogram *) * (n_hists + 1)); // end with null pointer to make alloc_array happy
    memset(hist_pointers, 0, sizeof(Histogram *) * (n_hists + 1));
    for (size_t i=0; i < n_hists; i++) {
        hist_pointers[i] = hists + (sizeof(Histogram) * i);
    }

    value res = caml_alloc_array(_wrap_histogram, hist_pointers);
    free(hist_pointers);
    free(hists);
    return res;
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
