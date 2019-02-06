// arraystore library implementation
//
// Written in a very specific 'c with classes and templates' style to
// avoid use of the cpp standard library.
//
// Objects can be created on the heap with allocate<T>(), which zeroes out
// their members, and then destroyed with destroy<T>() which calls the dtr
// and frees. Basically this is our new/delete.

#include <arraystore.h>
#include <lmdb.h>

#include <cassert>
#include <cerrno>
#include <cstdlib>

#include <cstdio>
#include <type_traits>
#include <utility>

// We put helpers into here
namespace arraystore {}
using namespace arraystore;


//  ======================================================================
//  == Utils

namespace arraystore {

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

}  // namespace arraystore
    

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
}

    
//  ======================================================================
//  == Internal span type

namespace arraystore {

template <typename T>
struct Span {
    const T     *data = nullptr;
    const size_t size = 0;

    Span (const T *dd, size_t ss) :data{dd}, size{ss} {}
    Span () =default;
};

}
        
        
//  ======================================================================
//  == Generic typed store of arrays, used as a mixin for user-visible types
//  ==   Since all keys are 8 bytes, vals requiring 2 and 4 byte alignment
//  ==   don't need padding at the end of arrays to retain it
//  ==   (lmdb stores k1, v1, k2, v2 etc sequentially in file/mmap)

namespace arraystore {

template <typename T>
struct TypedStore {
    // Member vars
    MDB_dbi _handle = 0;
    bool _opened = false;

    using value_type = T;

    // For use in mixins
    TypedStore *asTypedStore () { return this; }

    // Only use this once, just after object init
    int open (asenv_t *env, const char *name) {
        assert (env);
        assert (name);
        assert (!_opened);
        int rc = -1;

        // We need a txn to create the db if necessary
        astxn_t *txn = astxn_new_rdrw (env);
        if (!txn) goto die;

        rc = mdb_dbi_open (txn->handle, name, MDB_CREATE, &_handle);
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
template <typename Container, typename Elem>
Container * makeTypedStoreContainer (asenv_t *env, const char *name) {
    static_assert (std::is_same<
                       decltype(std::declval<Container>().asTypedStore()),
                       typename std::add_pointer<TypedStore<Elem>>::type
                   >::value,
                   "Can only construct TypedStore-holding types here");
    Container *res = allocate<Container> ();
    int rc = res->asTypedStore()->open (env, name);
    if (rc) goto die;

    return res;
 die:
    destroy (&res);
    return nullptr;
}

}  // namespace arraystore


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


