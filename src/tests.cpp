#include <arraystore.h>
#include <functional>
#include <vector>
#include <lmdb.h>


//  ----------------------------------------------------------------------
//  Assertion macro that ignores NDEBUG, and prints out errno + mdb string

#define enforce(cond)    \
    if (!(cond)) {       \
        fprintf (stderr, "\n\n## ENFORCEMENT FAILED:\n"); \
        fprintf (stderr, "%s:%d: \n",                     \
                 __FILE__, __LINE__);                     \
        fprintf (stderr, "    errno: %d, mdb of: %s\n",   \
                 errno, mdb_strerror (errno));            \
        abort ();        \
    }


//  ----------------------------------------------------------------------
//  Holder for all the main funcs in an array store

template <typename Store, typename Elem, typename Span>
struct StoreFuns {
    
    std::function <Store* (asenv_t*, const char*)> ctr;

    std::function <void (Store**)> dtr;

    std::function <int (Store*, astxn_t*, uint64_t, const Elem*, size_t)> put;

    std::function <Span (Store*, astxn_t*, uint64_t)> get;

    std::function <int (Store*, astxn_t*, uint64_t)> del;

    std::function <bool (Store*, astxn_t*, uint64_t)> exists;

    using store_type = Store;
    using value_type = Elem;
    using span_type  = Span;
};


//  ----------------------------------------------------------------------
//  All the array stores we have

auto storeFuns_i32 = StoreFuns <i32as_t, int32_t, i32span> {
    i32as_new,
    i32as_destroy,
    i32as_put,
    i32as_get,
    i32as_delete,
    i32as_exists
};

auto storeFuns_i64 = StoreFuns <i64as_t, int64_t, i64span> {
    i64as_new,
    i64as_destroy,
    i64as_put,
    i64as_get,
    i64as_delete,
    i64as_exists
};

auto storeFuns_f32 = StoreFuns <f32as_t, float, f32span> {
    f32as_new,
    f32as_destroy,
    f32as_put,
    f32as_get,
    f32as_delete,
    f32as_exists
};

auto storeFuns_f64 = StoreFuns <f64as_t, double, f64span> {
    f64as_new,
    f64as_destroy,
    f64as_put,
    f64as_get,
    f64as_delete,
    f64as_exists
};

auto storeFuns_byte = StoreFuns <byteas_t, unsigned char, bytespan> {
    byteas_new,
    byteas_destroy,
    byteas_put,
    byteas_get,
    byteas_delete,
    byteas_exists
};


//  ----------------------------------------------------------------------
//  Running tests on one array store

// TODO constrain SF
template <typename SF>
void testArrayStore (asenv_t *env,
                     SF storeFuns,
                     const char *name,
                     std::vector<typename SF::value_type> data) {
    using Store = typename SF::store_type;
    using Span  = typename SF::span_type;

    printf (" * Testing store %s...", name);
    fflush (stdout);
    
    Store *store = storeFuns.ctr (env, name);
    enforce (store);

    // Write some data
    {
        astxn_t *txn = astxn_new_rdrw (env);
        enforce (txn);

        enforce (! storeFuns.exists (store, txn, 111));

        // Main data
        int rc = storeFuns.put (store, txn, 111, &data[0], data.size());
        enforce (!rc);
        enforce (storeFuns.exists (store, txn, 111));

        // Same as main data, but we delete it
        rc = storeFuns.put (store, txn, 222, &data[0], data.size());
        enforce (!rc);
        enforce (storeFuns.exists (store, txn, 222));
        rc = storeFuns.del (store, txn, 222);
        enforce (!rc);
        enforce (! storeFuns.exists (store, txn, 222));

        // And an empty array
        rc = storeFuns.put (store, txn, 333, nullptr, 0);
        enforce (!rc);
        enforce (storeFuns.exists (store, txn, 333));

        // And write the main data here, then overwrite with an empty array
        rc = storeFuns.put (store, txn, 444, &data[0], data.size());
        enforce (!rc);
        rc = storeFuns.put (store, txn, 444, nullptr, 0);
        enforce (!rc);

        rc = astxn_commit (txn);
        enforce (!rc);
        
        astxn_destroy (&txn);
    }

    // Read it back in another txn
    {
        astxn_t *txn = astxn_new_rdonly (env);
        enforce (txn);

        // Main data
        Span sp = storeFuns.get (store, txn, 111);
        enforce (sp.data);
        enforce (sp.size == data.size());
        for (size_t i=0; i < data.size(); ++i)
            enforce (sp.data[i] == data.at(i));

        // Deleted entry
        enforce (! storeFuns.exists (store, txn, 222));
        sp = storeFuns.get (store, txn, 222);
        enforce (sp.data == nullptr);

        // Empty array
        enforce (storeFuns.exists (store, txn, 333));
        sp = storeFuns.get (store, txn, 333);
        enforce (sp.data);
        enforce (sp.size == 0);

        // Data overwritten to empty array
        sp = storeFuns.get (store, txn, 444);
        enforce (sp.data);
        enforce (sp.size == 0);

        astxn_destroy (&txn);
    }

    storeFuns.dtr (&store);

    printf (" OK\n");
}



int main (int argc, char **argv) {
    enforce (argc == 2);
    char *dbpath = argv [1];

    (void) dbpath;

    // valgrind prefers smaller mapsizes
    asenv_t *env = asenv_new_mapsize (dbpath, 10 * 1024 * 1024);
    enforce (env);

    puts ("");
    
    testArrayStore (env, storeFuns_i32,  "test_i32",  {10, 11, 12, 13});
    testArrayStore (env, storeFuns_i64,  "test_i64",  {10, 11, 12, 13});
    testArrayStore (env, storeFuns_f32,  "test_f32",  {10.1, 11.1, 12.1, 13.1});
    testArrayStore (env, storeFuns_f64,  "test_f64",  {10.1, 11.1, 12.1, 13.1});
    testArrayStore (env, storeFuns_byte, "test_byte", {'a', 'b', 'c'});

    asenv_destroy (&env);

    puts ("");
    puts ("-- All tests passed");
}
