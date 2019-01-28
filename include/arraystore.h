// arraystore library public API

#ifndef ARRAYSTORE_H_INCLUDED
#define ARRAYSTORE_H_INCLUDED

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif


//  ======================================================================
//  == Library-wide classes
    
//  ----------------------------------------------------------------------
//  asenv_t class - database environment

typedef struct asenv_t asenv_t;

// Open a new DB environment for the file at the given path, which need not
// exist. Assumes a max DB size of 10GB.
asenv_t *
asenv_new (const char *path);

// as _new(), but sets the maximum database size to 'mapsize' bytes, which must
// be a multiple of 4096.
asenv_t *
asenv_new_mapsize (const char *path, size_t mapsize);

// Close a database environment. You MUST not reuse any stores, transactions
// or spans derived from it afterwards. Idempotent.
void
asenv_destroy (asenv_t **self_p);


//  ----------------------------------------------------------------------
//  astxn_t - database transaction
//    These should be kept open as short a time as possible, as long-lived
//    transactions can prevent entry cleanup leading to database size bloat

typedef struct astxn_t astxn_t;

// Open a new read-write transaction. Blocks waiting for any other to close.
astxn_t *
astxn_new_rdrw (asenv_t *env);

// Open a new read-only transaction. Obvoiusly don't use to call put/delete.
astxn_t *
astxn_new_rdonly (asenv_t *env);

// Close a transaction, calling abort() if it's currently open. Idempotent.
void
astxn_destroy (astxn_t **self_p);

// Check whether a given transaction is read-only
bool
astxn_is_rdonly (astxn_t *self);

// Check whether a given transaction is read-write
bool
astxn_is_rdrw (astxn_t *self);

// Abort the given transaction and roll back any changes. No more reading
// or writing is allowed using the txn.
void
astxn_abort (astxn_t *self);

// Commit any changes written through this transaction. No more reading or
// writing is allowed using the txn.
// Returns 0 iff success
int
astxn_commit (astxn_t *self);


//  ======================================================================
//  == Individual typed-array data stores

//  ----------------------------------------------------------------------
//  i32as_t - int32 array store class

// Opaque class handle
typedef struct i32as_t i32as_t;

// Store GET return value, a view into a contiguous array of memory
typedef struct i32span {
    const int32_t *data;
    size_t         size;
} i32span;

// Open a new named array store in the database
i32as_t *
i32as_new (asenv_t *env, const char *name);

// Close the given store after use is finished
void
i32as_destroy (i32as_t **self_p);

// PUT a key/array value in the store. Returns 0 iff success.
int
i32as_put (i32as_t *self, astxn_t *txn,
           uint64_t key,
           const int32_t *data, size_t size);

// GET a value from the store. Returns span to NULL if no entry with that key
// existed, or on error (you can check which with _exists())
i32span
i32as_get (i32as_t *self, astxn_t *txn,
           uint64_t key);

// DELETE a value from the store. Returns 0 iff success.
int
i32as_delete (i32as_t *self, astxn_t *txn, uint64_t key);

// Checks whether a value with the given key EXISTS in the store
bool
i32as_exists (i32as_t *self, astxn_t *txn, uint64_t key);


//  ----------------------------------------------------------------------
//  i64as_t - int64 array store class

// Opaque class handle
typedef struct i64as_t i64as_t;
    
// Store GET return value, a view into a contiguous array of memory
typedef struct i64span {
    const int64_t *data;
    size_t         size;
} i64span;

// Open a new named array store in the database
i64as_t *
i64as_new (asenv_t *env, const char *name);

// Close the given store after use is finished
void
i64as_destroy (i64as_t **self_p);

// PUT a key/array value in the store. Returns 0 iff success.
int
i64as_put (i64as_t *self, astxn_t *txn,
           uint64_t key,
           const int64_t *data, size_t size);

// GET a value from the store. Returns span to NULL if no entry with that key
// existed, or on error (you can check which with _exists())
i64span
i64as_get (i64as_t *self, astxn_t *txn,
           uint64_t key);

// DELETE a value from the store. Returns 0 iff success.
int
i64as_delete (i64as_t *self, astxn_t *txn, uint64_t key);

// Checks whether a value with the given key EXISTS in the store
bool
i64as_exists (i64as_t *self, astxn_t *txn, uint64_t key);


//  ----------------------------------------------------------------------
//  f32as_t - 32-bit float array store class

// Opaque class handle
typedef struct f32as_t f32as_t;

// Store GET return value, a view into a contiguous array of memory
typedef struct f32span {
    const float *data;
    size_t       size;
} f32span;

// Open a new named array store in the database
f32as_t *
f32as_new (asenv_t *env, const char *name);

// Close the given store after use is finished
void
f32as_destroy (f32as_t **self_p);

// PUT a key/array value in the store. Returns 0 iff success.
int
f32as_put (f32as_t *self, astxn_t *txn,
           uint64_t key,
           const float *data, size_t size);

// GET a value from the store. Returns span to NULL if no entry with that key
// existed, or on error (you can check which with _exists())
f32span
f32as_get (f32as_t *self, astxn_t *txn,
           uint64_t key);

// DELETE a value from the store. Returns 0 iff success.
int
f32as_delete (f32as_t *self, astxn_t *txn, uint64_t key);

// Checks whether a value with the given key EXISTS in the store
bool
f32as_exists (f32as_t *self, astxn_t *txn, uint64_t key);


//  ----------------------------------------------------------------------
//  f64as_t - 64-bit float array store class

// Opaque class handle
typedef struct f64as_t f64as_t;

// Store GET return value, a view into a contiguous array of memory
typedef struct f64span {
    const double *data;
    size_t        size;
} f64span;

// Open a new named array store in the database
f64as_t *
f64as_new (asenv_t *env, const char *name);

// Close the given store after use is finished
void
f64as_destroy (f64as_t **self_p);

// PUT a key/array value in the store. Returns 0 iff success.
int
f64as_put (f64as_t *self, astxn_t *txn,
           uint64_t key,
           const double *data, size_t size);

// GET a value from the store. Returns span to NULL if no entry with that key
// existed, or on error (you can check which with _exists())
f64span
f64as_get (f64as_t *self, astxn_t *txn,
           uint64_t key);

// DELETE a value from the store. Returns 0 iff success.
int
f64as_delete (f64as_t *self, astxn_t *txn, uint64_t key);

// Checks whether a value with the given key EXISTS in the store
bool
f64as_exists (f64as_t *self, astxn_t *txn, uint64_t key);


//  ----------------------------------------------------------------------
//  byteas_t - byte array store class

// Opaque class handle
typedef struct byteas_t byteas_t;

// Store GET return value, a view into a contiguous array of memory
typedef struct bytespan {
    const unsigned char *data;
    size_t               size;
} bytespan;

// Open a new named array store in the database
byteas_t *
byteas_new (asenv_t *env, const char *name);

// Close the given store after use is finished
void
byteas_destroy (byteas_t **self_p);

// PUT a key/array value in the store. Returns 0 iff success.
int
byteas_put (byteas_t *self, astxn_t *txn,
            uint64_t key,
            const unsigned char *data, size_t size);

// GET a value from the store. Returns span to NULL if no entry with that key
// existed, or on error (you can check which with _exists())
bytespan
byteas_get (byteas_t *self, astxn_t *txn,
            uint64_t key);

// DELETE a value from the store. Returns 0 iff success.
int
byteas_delete (byteas_t *self, astxn_t *txn, uint64_t key);

// Checks whether a value with the given key EXISTS in the store
bool
byteas_exists (byteas_t *self, astxn_t *txn, uint64_t key);


    
// -- Header ends

#ifdef __cplusplus
}  // extern "C"
#endif

#endif  // ARRAYSTORE_H_INCLUDED
