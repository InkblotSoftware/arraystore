// Copyright (C) 2019 Inkblot Software Limited

// arraystore library implementation
//
// Written in a very specific 'c with classes and templates' style to
// avoid use of the cpp standard library.
//
// Objects can be created on the heap with allocate<T>(), which zeroes out
// their members, and then destroyed with destroy<T>() which calls the dtr
// and frees. Basically this is our new/delete.

// TODO split class groups out into separate files

#include <arraystore.h>
#include <lmdb.h>

#include <cassert>
#include <cerrno>
#include <cstdlib>
#include <cstring>

#include <cstdio>

#include <type_traits>
#include <utility>

// We put helpers into here
namespace arraystore { namespace impl {} }
using namespace arraystore::impl;



/*
 *  ************************************************************
 *  **********  UTILS  *****************************************
 *  ************************************************************
 */

namespace arraystore { 
namespace impl {

// A key in an array store
using Key = uint64_t;

// Better calloc
template <typename T>
T* allocate () {
    T* ptr = (T*) calloc (1, sizeof(T));
    assert (ptr);
    return ptr;
}

// Call dtr and null out pointer. nop if already null.
template <typename T>
void destroy (T **self_p) {
    assert (self_p);
    if (*self_p) {
        T* self = *self_p;
        self->~T ();
        free (self);
        *self_p = nullptr;
    }
}

// Internal span type
template <typename T>
struct Span {
    const T     *data = nullptr;
    const size_t size = 0;

    explicit Span (const T *dd, size_t ss) :data{dd}, size{ss} {}
    explicit Span () =default;
};

// Does the given pointer have valid alignment to store T there?
template <typename T>
bool is_aligned (const void *ptr) {
    auto ip = reinterpret_cast<uintptr_t> (ptr);
    return ip % alignof(T) == 0;
}

// SFINAE helper
template <typename Base, typename Derived>
using enable_if_base_of_t = typename std::enable_if<
                                std::is_base_of< Base, Derived >::value
                            >::type;
    
}  // namespace arraystore
}  // namespace impl


/*
 *  ************************************************************
 *  **********  CROSS-LIBRARY CLASSES  *************************
 *  ************************************************************
 */

//  ======================================================================
//  == asenv_t
    
//  ----------------------------------------------------------------------
//  Class body

struct asenv_t {
    // -- Member vars
    MDB_env *handle;

    // -- DB open constants
    static const size_t max_dbs = 10;
    static const size_t mapsize_1GB  =  1UL * 1024UL * 1024UL * 1024UL;
    static const size_t mapsize_10GB = 10UL * 1024UL * 1024UL * 1024UL;
    
    static const mdb_mode_t env_mode = 0644;

    // -- Dtr
    ~asenv_t () {
        if (handle)
            mdb_env_close (handle);
    }

    // -- Open lmdb env
    int open (const char *path, size_t mapsize) {
        assert (path);
        int rc = -1;

        rc = mdb_env_create (&handle);
        if (rc) return rc;

        rc = mdb_env_set_maxdbs (handle, max_dbs);
        if (rc) return rc;
        
        rc = mdb_env_set_mapsize (handle, mapsize);
        if (rc) return rc;
        
        rc = mdb_env_open (handle, path, MDB_NOSUBDIR, env_mode);
        if (rc) return rc;

        return 0;
    }
};

//  ----------------------------------------------------------------------
//  Exported C functions

extern "C" {
    asenv_t * asenv_new_mapsize (const char *path, size_t mapsize) {
        asenv_t *self = allocate<asenv_t> ();
        int rc = self->open (path, mapsize);
        if (rc) {
            destroy (&self);
            errno = rc;
            return nullptr;
        } else {
            return self;
        }
    }
    asenv_t * asenv_new (const char *path) {
        return asenv_new_mapsize (path, asenv_t::mapsize_10GB);
    }
    void asenv_destroy (asenv_t **self_p) {
        destroy (self_p);
    }
}
    
    
//  ======================================================================
//  == astxn_t

//  ----------------------------------------------------------------------
//  Main class

struct astxn_t {
    // -- Member vars
    MDB_txn *handle;
    bool     rdonly;

    // -- Dtr
    ~astxn_t () {
        if (handle)
            abort ();
    }

    // -- Open lmdb txn
    int open (asenv_t *env, bool wantRdrw) {
        assert (env);
        int flags = wantRdrw ? 0 : MDB_RDONLY;
        int rc = mdb_txn_begin (env->handle, nullptr, flags, &handle);
        if (rc)
            return rc;
        if (!wantRdrw)
            rdonly = true;
        return rc;
    }

    // -- Abort/commit
    void abort () {
        assert (handle);
        mdb_txn_abort (handle);
        handle = nullptr;
    }
    int commit () {
        assert (handle);
        int rc = mdb_txn_commit (handle);
        handle = nullptr;
        return rc;
    }
};
            
//  ----------------------------------------------------------------------
//  Exported C functions

extern "C" {
    astxn_t * astxn_new_rdonly (asenv_t *env) {
        astxn_t *self = allocate<astxn_t> ();
        int rc = self->open (env, false);
        if (rc) {
            destroy (&self);
            errno = rc;
            return nullptr;
        } else {
            return self;
        }
    }
    astxn_t * astxn_new_rdrw (asenv_t *env) {
        astxn_t *self = allocate<astxn_t> ();
        int rc = self->open (env, true);
        if (rc) {
            destroy (&self);
            errno = rc;
            return nullptr;
        } else {
            return self;
        }
    }
    void astxn_destroy (astxn_t **self_p) {
        destroy (self_p);
    }
    void astxn_abort (astxn_t *self) {
        assert (self);
        self->abort ();
    }
    int astxn_commit (astxn_t *self) {
        assert (self);
        return self->commit ();
    }
    bool astxn_is_rdonly (astxn_t *self) {
        assert (self);
        return self->rdonly;
    }
    bool astxn_is_rdrw (astxn_t *self) {
        assert (self);
        return ! astxn_is_rdonly (self);
    }
}



/*
 *  ************************************************************
 *  **********  ARRAY STORES  **********************************
 *  ************************************************************
 */

//  ======================================================================
//  == Generic typed store of arrays, used as a mixin for user-visible types
//  ==   Since all keys are 8 bytes, vals requiring 2 and 4 byte alignment
//  ==   don't need padding at the end of arrays to retain it
//  ==   (lmdb stores k1, v1, k2, v2 etc sequentially in file/mmap)

namespace arraystore {
namespace impl {

template <typename T>
struct TypedStore {
    // Member vars
    MDB_dbi _handle;
    bool _opened;

    using value_type = T;

    // Only use this once, just after object init
    int open (asenv_t *env, const char *name) {
        assert (env);
        assert (name);
        assert (!_opened);
        int rc = -1;

        // We need a txn to create the db if necessary
        astxn_t *txn = astxn_new_rdrw (env);
        if (!txn) goto die;

        rc = mdb_dbi_open (txn->handle, name, MDB_CREATE | MDB_INTEGERKEY,
                           &_handle);
        if (rc) goto die;

        rc = astxn_commit (txn);
        if (rc) goto die;

        astxn_destroy (&txn);
        _opened = true;
        return 0;

    die:
        astxn_destroy (&txn);
        return rc;
    }

    // -- Dtr - nop
    ~TypedStore () {}

    // -- GET
    Span<T> get (astxn_t *txn, Key key) {
        assert (txn);
        
        MDB_val mkey = {sizeof (key), &key};
        MDB_val mval = {0, nullptr};

        int rc = mdb_get (txn->handle, _handle, &mkey, &mval);
        if (rc) goto die;
        if (mval.mv_size % sizeof(value_type) != 0) goto die;

        // TODO consider whether to assert or return sentinel here
        assert (is_aligned<T> (mval.mv_data));

        return Span<T> {static_cast<const T *>(mval.mv_data),
                        mval.mv_size / sizeof(T)};

    die:
        return Span<T> {};
    }

    // -- PUT
    int put (astxn_t *txn, Key key, Span<T> data) {
        assert (txn);
        assert (data.data || (data.size == 0));
        
        MDB_val mkey = {sizeof (key), &key};
        MDB_val mval = {data.size * sizeof(T), (void*)data.data};
        
        int rc = mdb_put (txn->handle, _handle, &mkey, &mval, 0);
        return rc;
    }

    // -- EXISTS
    bool exists (astxn_t *txn, Key key) {
        assert (txn);
        
        MDB_val mkey = {sizeof (key), &key};
        MDB_val mval = {0, nullptr};

        int rc = mdb_get (txn->handle, _handle, &mkey, &mval);
        assert (rc == 0 || rc == MDB_NOTFOUND);

        return rc == 0;
    }

    // -- DELETE
    int del (astxn_t *txn, Key key) {
        assert (txn);

        MDB_val mkey = {sizeof (key), &key};
        MDB_val mval = {0, nullptr};

        int rc = mdb_del (txn->handle, _handle, &mkey, &mval);
        return rc;
    }
};

// For creating i32as's etc, which wrap a TypedStore
template <typename Container, typename Elem,
          class = enable_if_base_of_t< TypedStore<Elem>,
                                       Container >>
Container * makeTypedStoreContainer (asenv_t *env, const char *name) {
    Container *res = allocate<Container> ();
    int rc = res->open (env, name);
    if (rc) goto die;

    return res;
 die:
    destroy (&res);
    return nullptr;
}

}  // namespace arraystore
}  // namespace impl


//  ======================================================================
//  == Store of int32's

struct i32as_t : TypedStore<int32_t> {};

extern "C" {
    i32as_t * i32as_new (asenv_t *env, const char *name) {
        return makeTypedStoreContainer <i32as_t, int32_t> (env, name);
    }
    void i32as_destroy (i32as_t **self_p) {
        destroy (self_p);
    }
    i32span i32as_get (i32as_t *self, astxn_t *txn, Key key) {
        assert (self);
        auto sp = self->get (txn, key);
        return i32span {sp.data, sp.size};
    }
    int i32as_put (i32as_t *self, astxn_t *txn, Key key,
                   const int32_t *data, size_t size) {
        assert (self);
        return self->put (txn, key, Span<int32_t>{data,size});
    }
    bool i32as_exists (i32as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->exists (txn, key);
    }
    int i32as_delete (i32as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->del (txn, key);
    }
}


//  ======================================================================
//  == Store of int64's

struct i64as_t : TypedStore<int64_t> {};

extern "C" {
    i64as_t * i64as_new (asenv_t *env, const char *name) {
        return makeTypedStoreContainer <i64as_t, int64_t> (env, name);
    }
    void i64as_destroy (i64as_t **self_p) {
        destroy (self_p);
    }
    i64span i64as_get (i64as_t *self, astxn_t *txn, Key key) {
        assert (self);
        auto sp = self->get (txn, key);
        return i64span {sp.data, sp.size};
    }
    int i64as_put (i64as_t *self, astxn_t *txn, Key key,
                   const int64_t *data, size_t size) {
        assert (self);
        return self->put (txn, key, Span<int64_t>{data,size});
    }
    bool i64as_exists (i64as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->exists (txn, key);
    }
    int i64as_delete (i64as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->del (txn, key);
    }
}


//  ======================================================================
//  == Store of f32's

struct f32as_t : TypedStore<float> {};
static_assert (sizeof (float) == sizeof (int32_t), "");

extern "C" {
    f32as_t * f32as_new (asenv_t *env, const char *name) {
        return makeTypedStoreContainer <f32as_t, float> (env, name);
    }
    void f32as_destroy (f32as_t **self_p) {
        destroy (self_p);
    }
    f32span f32as_get (f32as_t *self, astxn_t *txn, Key key) {
        assert (self);
        auto sp = self->get (txn, key);
        return f32span {sp.data, sp.size};
    }
    int f32as_put (f32as_t *self, astxn_t *txn, Key key,
                   const float *data, size_t size) {
        assert (self);
        return self->put (txn, key, Span<float>{data,size});
    }
    bool f32as_exists (f32as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->exists (txn, key);
    }
    int f32as_delete (f32as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->del (txn, key);
    }
}


//  ======================================================================
//  == Store of f64's

struct f64as_t : TypedStore<double> {};
static_assert (sizeof (double) == sizeof (int64_t), "");

extern "C" {
    f64as_t * f64as_new (asenv_t *env, const char *name) {
        return makeTypedStoreContainer <f64as_t, double> (env, name);
    }
    void f64as_destroy (f64as_t **self_p) {
        destroy (self_p);
    }
    f64span f64as_get (f64as_t *self, astxn_t *txn, Key key) {
        assert (self);
        auto sp = self->get (txn, key);
        return f64span {sp.data, sp.size};
    }
    int f64as_put (f64as_t *self, astxn_t *txn, Key key,
                   const double *data, size_t size) {
        assert (self);
        return self->put (txn, key, Span<double>{data,size});
    }
    bool f64as_exists (f64as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->exists (txn, key);
    }
    int f64as_delete (f64as_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->del (txn, key);
    }
}


//  ======================================================================
//  == Store of bytes

struct byteas_t : TypedStore<unsigned char> {};

extern "C" {
    byteas_t * byteas_new (asenv_t *env, const char *name) {
        return makeTypedStoreContainer <byteas_t, unsigned char> (env, name);
    }
    void byteas_destroy (byteas_t **self_p) {
        destroy (self_p);
    }
    bytespan byteas_get (byteas_t *self, astxn_t *txn, Key key) {
        assert (self);
        auto sp = self->get (txn, key);
        return bytespan {sp.data, sp.size};
    }
    int byteas_put (byteas_t *self, astxn_t *txn, Key key,
                    const unsigned char *data, size_t size) {
        assert (self);
        return self->put (txn, key, Span<unsigned char>{data,size});
    }
    bool byteas_exists (byteas_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->exists (txn, key);
    }
    int byteas_delete (byteas_t *self, astxn_t *txn, Key key) {
        assert (self);
        return self->del (txn, key);
    }
}



/*
 *  ************************************************************
 *  **********  ITERATORS  *************************************
 *  ************************************************************
 */

//  ======================================================================
//  == Generic iterator class for array stores
//  ==    You MUST create these via calloc or similar, zeroing out memory

namespace arraystore {
namespace impl {

template <typename Store>
struct TypedStoreIter {
    static_assert (std::is_base_of< TypedStore<typename Store::value_type>,
                                    Store >::value,
                   "Can only wrap TypedStore's, of same element type.");

    using store_type = Store;
    using value_type = typename Store::value_type;
    
    // Member vars
    MDB_cursor *_handle;
    Store      *_store;
    MDB_val     _mkey;
    MDB_val     _mval;  // points to null iff iterator invalid

    // -- Set up the iter to look at the given store. Call once before iter use.
    int open (Store *store, astxn_t *txn) {
        assert (! _handle);
        assert (store);
        assert (txn);

        int rc = mdb_cursor_open (txn->handle, store->_handle, &_handle);
        return rc;
    }
    
    // -- Start the iterator's traversal, from or above the provided key
    bool upfrom (Key fromKey) {
        _mkey.mv_data = &fromKey;
        _mkey.mv_size = sizeof (fromKey);
        int rc = mdb_cursor_get (_handle, &_mkey, &_mval, MDB_SET_RANGE);

        assert (rc == 0 || rc == MDB_NOTFOUND);

        // Mark self as invalid if we haven't got data
        if (rc) setInvalid ();

        return (rc == 0);
    }
        
    // -- Dtr
    ~TypedStoreIter () {
        if (_handle) {
            mdb_cursor_close (_handle);
            _handle = nullptr;
        }
    }

    bool valid () { return _mval.mv_data != nullptr; }

    // -- Get key and value out of well-positioned iterator

    Key key () {
        assert (valid ());
        
        // Check key size is right
        // TODO consider whether we should assert here or return sentinel
        assert (_mkey.mv_size == sizeof(Key));
        
        // Our keys are not guaranteed 8-byte aligned in the db
        Key res = 0;
        memcpy (&res, _mkey.mv_data, sizeof(Key));
        return res;
    }
    
    Span<value_type> array () {
        assert (valid ());
        
        // Check array byte size is valid for given value type
        // TODO consider whether we should assert here or return sentinel
        assert (_mval.mv_size % sizeof(value_type) == 0);

        return Span<value_type> {
            reinterpret_cast<const value_type *> (_mval.mv_data),
            _mval.mv_size / sizeof(value_type)
        };
    }

    // -- Moving the cursor; returns true iff found entry
    
    bool next () {
        int rc = mdb_cursor_get (_handle, &_mkey, &_mval, MDB_NEXT);
        assert (rc == 0 || rc == MDB_NOTFOUND);
        return (rc == 0);
    }
    bool prev () {
        int rc = mdb_cursor_get (_handle, &_mkey, &_mval, MDB_PREV);
        assert (rc == 0 || rc == MDB_NOTFOUND);
        return (rc == 0);
    }

private:
    void setInvalid () {
        _mval.mv_data = nullptr;
        _mval.mv_size = 0;
    }
};

// For creating i32asiter's etc, which wrap a TypedStoreIter
template <typename Container,
          class Store = typename Container::store_type,
          class Value = typename Container::value_type,
          class = enable_if_base_of_t< TypedStoreIter<TypedStore<Value>>,
                                       Container >>
Container * makeIterContainer (Store *store, astxn_t *txn) {
    Container *res = allocate<Container> ();
    int rc = res->open (store, txn);
    if (rc) goto die;

    return res;
 die:
    destroy (&res);
    errno = rc;
    return nullptr;
}

}  // namespace arraystore
}  // namespace impl


//  ======================================================================
//  == Specific iterator class implementations

//  ----------------------------------------------------------------------
//  i32asiter_t

struct i32asiter_t : TypedStoreIter<TypedStore<int32_t>> {};

extern "C" {
    i32asiter_t * i32asiter_new (i32as_t *store, astxn_t *txn) {
        return makeIterContainer <i32asiter_t> (store, txn);
    }
    void i32asiter_destroy (i32asiter_t **self_p) {
        destroy (self_p);
    }
    bool i32asiter_upfrom (i32asiter_t *self, uint64_t key) {
        assert (self);
        return self->upfrom (key);
    }
    bool i32asiter_valid (i32asiter_t *self) {
        assert (self);
        return self->valid ();
    }
    uint64_t i32asiter_key (i32asiter_t *self) {
        assert (self);
        return self->key ();
    }
    i32span i32asiter_array (i32asiter_t *self) {
        assert (self);
        Span<int32_t> dats = self->array ();
        return i32span {dats.data, dats.size};
    }
    bool i32asiter_next (i32asiter_t *self) {
        assert (self);
        return self->next ();
    }
    bool i32asiter_prev (i32asiter_t *self) {
        assert (self);
        return self->prev ();
    }
}

//  ----------------------------------------------------------------------
//  i64asiter_t

struct i64asiter_t : TypedStoreIter<TypedStore<int64_t>> {};

extern "C" {
    i64asiter_t * i64asiter_new (i64as_t *store, astxn_t  *txn) {
        return makeIterContainer <i64asiter_t> (store, txn);
    }
    void i64asiter_destroy (i64asiter_t **self_p) {
        destroy (self_p);
    }
    bool i64asiter_upfrom (i64asiter_t *self, uint64_t key) {
        assert (self);
        return self->upfrom (key);
    }
    bool i64asiter_valid (i64asiter_t *self) {
        assert (self);
        return self->valid ();
    }
    uint64_t i64asiter_key (i64asiter_t *self) {
        assert (self);
        return self->key ();
    }
    i64span i64asiter_array (i64asiter_t *self) {
        assert (self);
        Span<int64_t> dats = self->array ();
        return i64span {dats.data, dats.size};
    }
    bool i64asiter_next (i64asiter_t *self) {
        assert (self);
        return self->next ();
    }
    bool i64asiter_prev (i64asiter_t *self) {
        assert (self);
        return self->prev ();
    }
}

//  ----------------------------------------------------------------------
//  i32asiter_t

struct f32asiter_t : TypedStoreIter<TypedStore<float>> {};
static_assert (sizeof (float) == sizeof (int32_t), "");

extern "C" {
    f32asiter_t * f32asiter_new (f32as_t *store, astxn_t *txn) {
        return makeIterContainer <f32asiter_t> (store, txn);
    }
    void f32asiter_destroy (f32asiter_t **self_p) {
        destroy (self_p);
    }
    bool f32asiter_upfrom (f32asiter_t *self, uint64_t key) {
        assert (self);
        return self->upfrom (key);
    }
    bool f32asiter_valid (f32asiter_t *self) {
        assert (self);
        return self->valid ();
    }
    uint64_t f32asiter_key (f32asiter_t *self) {
        assert (self);
        return self->key ();
    }
    f32span f32asiter_array (f32asiter_t *self) {
        assert (self);
        Span<float> dats = self->array ();
        return f32span {dats.data, dats.size};
    }
    bool f32asiter_next (f32asiter_t *self) {
        assert (self);
        return self->next ();
    }
    bool f32asiter_prev (f32asiter_t *self) {
        assert (self);
        return self->prev ();
    }
}

//  ----------------------------------------------------------------------
//  i64asiter_t

struct f64asiter_t : TypedStoreIter<TypedStore<double>> {};
static_assert (sizeof (double) == sizeof (int64_t), "");

extern "C" {
    f64asiter_t * f64asiter_new (f64as_t *store, astxn_t *txn) {
        return makeIterContainer <f64asiter_t> (store, txn);
    }
    void f64asiter_destroy (f64asiter_t **self_p) {
        destroy (self_p);
    }
    bool f64asiter_upfrom (f64asiter_t *self, uint64_t key) {
        assert (self);
        return self->upfrom (key);
    }
    bool f64asiter_valid (f64asiter_t *self) {
        assert (self);
        return self->valid ();
    }
    uint64_t f64asiter_key (f64asiter_t *self) {
        assert (self);
        return self->key ();
    }
    f64span f64asiter_array (f64asiter_t *self) {
        assert (self);
        Span<double> dats = self->array ();
        return f64span {dats.data, dats.size};
    }
    bool f64asiter_next (f64asiter_t *self) {
        assert (self);
        return self->next ();
    }
    bool f64asiter_prev (f64asiter_t *self) {
        assert (self);
        return self->prev ();
    }
}

//  ----------------------------------------------------------------------
//  byteasiter_t

struct byteasiter_t : TypedStoreIter<TypedStore<unsigned char>> {};

extern "C" {
    byteasiter_t * byteasiter_new (byteas_t *store, astxn_t *txn) {
        return makeIterContainer <byteasiter_t> (store, txn);
    }
    void byteasiter_destroy (byteasiter_t **self_p) {
        destroy (self_p);
    }
    bool byteasiter_upfrom (byteasiter_t *self, uint64_t key) {
        assert (self);
        return self->upfrom (key);
    }
    bool byteasiter_valid (byteasiter_t *self) {
        assert (self);
        return self->valid ();
    }
    uint64_t byteasiter_key (byteasiter_t *self) {
        assert (self);
        return self->key ();
    }
    bytespan byteasiter_array (byteasiter_t *self) {
        assert (self);
        Span<unsigned char> dats = self->array ();
        return bytespan {dats.data, dats.size};
    }
    bool byteasiter_next (byteasiter_t *self) {
        assert (self);
        return self->next ();
    }
    bool byteasiter_prev (byteasiter_t *self) {
        assert (self);
        return self->prev ();
    }
}
