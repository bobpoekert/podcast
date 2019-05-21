#include <stdint.h>
#include <string.h>
#include <stdio.h>

#include "art.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

#define art_tree_val(v) (*((art_tree **) Data_custom_val(v)))

void art_finalize(value t) {

    art_tree *t_val = art_tree_val(t);
    art_tree_destroy(t_val);
    free(t_val);

}

static struct custom_operations art_ops = {
    "cheap.hella.art",
    art_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

typedef uint64_t val_t;
#define encode_val(v) ((val_t) Long_val(v))
#define decode_val(v) Val_long((val_t) v)

value _wrap_tree(art_tree *v) {
    value res = alloc_custom(&art_ops, sizeof(art_tree), 0, 1);
    art_tree_val(res) = v;
    return res;
}

CAMLprim value call_art_create() {
    CAMLparam0();
    art_tree *tree = malloc(sizeof(art_tree));
    if (art_tree_init(tree)) {
        free(tree);
        caml_failwith("failed to init tree");
    }
    return _wrap_tree(tree);
}

void call_art_put(value tree_v, value k, value v) {
    CAMLparam3(tree_v, k, v);

    char *k_buf = String_val(k);
    size_t k_size = caml_string_length(k);

    art_tree *tree = art_tree_val(tree_v);

    val_t v_int = encode_val(v);

    art_insert(tree, ( unsigned char *) k_buf, k_size, (void *) v_int);

    CAMLreturn0;
}

CAMLprim value call_art_get(value tree_v, value k) {

    char *k_buf = String_val(k);
    size_t k_size = caml_string_length(k);

    art_tree *tree = art_tree_val(tree_v);

    void *res = art_search(tree, (unsigned char *) k_buf, k_size);

    if (res != 0) {
        return decode_val(res);
    } else {
        caml_raise_not_found();
    }
}

int iter_callback(void *data,  unsigned char *key, uint32_t key_len, void *v_v) {
    value callback = *((value *) data);
    CAMLparam1(callback);
    CAMLlocal2(k, v);
    v = decode_val(v_v);

    k = caml_alloc_initialized_string(key_len, (const char *) key);

    caml_callback2(callback, k, v);
    CAMLreturnT(int, 0);
}

CAMLprim void call_art_iter(value tree_v, value callback_v) {
    CAMLparam2(tree_v, callback_v);

    art_tree *tree = art_tree_val(tree_v);
    value *callback_ref = &callback_v;
    caml_register_global_root(callback_ref);
    art_iter(tree, iter_callback, (void *) callback_ref);
    caml_remove_global_root(callback_ref);

    CAMLreturn0;
}

void call_art_iter_prefix(value tree_v, value key_v, value callback_v) {
    CAMLparam3(tree_v, key_v, callback_v);

    art_tree *tree = art_tree_val(tree_v);

    char *k_buf = String_val(key_v);
    size_t k_size = caml_string_length(key_v);

    value *callback_ref = &callback_v;

    caml_register_global_root(callback_ref);
    art_iter_prefix(tree, ( unsigned char *) k_buf, k_size, iter_callback, (void *) callback_ref);
    caml_remove_global_root(callback_ref);

    CAMLreturn0;
}

CAMLprim value call_art_remove(value tree_v, value key_v) {

    art_tree *tree = art_tree_val(tree_v);
    
    char *k_buf = String_val(key_v);
    size_t k_size = caml_string_length(key_v);

    value res;

    if (art_delete(tree,( unsigned char*) k_buf, k_size)) {
        res = Val_true;
    } else {
        res = Val_false;
    }

    return res;

}

CAMLprim value call_art_length(value tree_v) {

    art_tree *tree = art_tree_val(tree_v);

    return Val_long(art_size(tree));
}

int _sum_callback(void *target, const unsigned char *key, uint32_t key_len, void *value) {
    *((int64_t *) target) += ((int64_t) value);
    return 0;
}

CAMLprim value call_art_sum(value tree_v) {
    art_tree *tree = art_tree_val(tree_v);
    uint64_t res = 0;
    art_iter(tree, _sum_callback, &res);
    return Val_long(res);
}