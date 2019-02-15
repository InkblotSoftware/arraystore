// Copyright (C) 2019 Inkblot Software Limited

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
//  Similar holder for per-iterator functions

template <typename Iter, typename Store, typename Elem, typename Span>
struct IterFuns {

    std::function <Iter* (Store *store, astxn_t *txn)> ctr;

    std::function <void (Iter**)> dtr;

    std::function <bool (Iter*, uint64_t key)> upfrom;
    
    std::function <bool (Iter*)> valid;

    std::function <uint64_t (Iter*)> key;

    std::function <Span (Iter*)> array;

    std::function <bool (Iter*)> next;

    std::function <bool (Iter*)> prev;

    using iter_type = Iter;
    using store_type = Store;
};
    

//  ----------------------------------------------------------------------
//  Funs for all the iterator classes we have

auto iterFuns_i32 = IterFuns <i32asiter_t, i32as_t, int32_t, i32span> {
    i32asiter_new,
    i32asiter_destroy,
    i32asiter_upfrom,
    i32asiter_valid,
    i32asiter_key,
    i32asiter_array,
    i32asiter_next,
    i32asiter_prev
};

auto iterFuns_i64 = IterFuns <i64asiter_t, i64as_t, int64_t, i64span> {
    i64asiter_new,
    i64asiter_destroy,
    i64asiter_upfrom,
    i64asiter_valid,
    i64asiter_key,
    i64asiter_array,
    i64asiter_next,
    i64asiter_prev
};

auto iterFuns_f32 = IterFuns <f32asiter_t, f32as_t, float, f32span> {
    f32asiter_new,
    f32asiter_destroy,
    f32asiter_upfrom,
    f32asiter_valid,
    f32asiter_key,
    f32asiter_array,
    f32asiter_next,
    f32asiter_prev
};

auto iterFuns_f64 = IterFuns <f64asiter_t, f64as_t, double, f64span> {
    f64asiter_new,
    f64asiter_destroy,
    f64asiter_upfrom,
    f64asiter_valid,
    f64asiter_key,
    f64asiter_array,
    f64asiter_next,
    f64asiter_prev
};

auto iterFuns_byte = IterFuns <byteasiter_t, byteas_t, unsigned char, bytespan> {
    byteasiter_new,
    byteasiter_destroy,
    byteasiter_upfrom,
    byteasiter_valid,
    byteasiter_key,
    byteasiter_array,
    byteasiter_next,
    byteasiter_prev
};



//  ----------------------------------------------------------------------
//  Running tests on one array store

// TODO constrain SF and IF
template <typename SF, typename IF>
void testArrayStore (asenv_t *env,
                     SF storeFuns,
                     IF iterFuns,
                     const char *name,
                     std::vector<typename SF::value_type> data) {
    static_assert (std::is_same<typename SF::store_type,
                                typename IF::store_type
                   >::value,
                   "StoreFuns and IterFuns must refer to samae store type");
    
    using Store = typename SF::store_type;
    using Span  = typename SF::span_type;
    using Iter  = typename IF::iter_type;
    
    printf (" * Testing store/iter %s...", name);
    fflush (stdout);
    
    Store *store = storeFuns.ctr (env, name);
    enforce (store);

    
    //  ------------------------------------------------------------
    //  Write some data
    
    {
        astxn_t *txn = astxn_new_rdrw (env);
        enforce (txn);
        enforce (astxn_is_rdrw (txn));
        enforce (! astxn_is_rdonly (txn));

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


    //  ------------------------------------------------------------
    //  Read it back in another txn
    
    {
        astxn_t *txn = astxn_new_rdonly (env);
        enforce (txn);
        enforce (astxn_is_rdonly (txn));
        enforce (! astxn_is_rdrw (txn));

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

    
    //  ------------------------------------------------------------
    //  Traverse the store entries via an iterator
    
    {
        astxn_t *txn = astxn_new_rdonly (env);
        enforce (txn);

        // First a quick test of an iter that starts past all the entries
        {
            Iter *iter = iterFuns.ctr (store, txn);
            enforce (iter);

            bool ok = iterFuns.upfrom (iter, 99999);
            enforce (! ok);
            enforce (! iterFuns.valid (iter));

            // Nothing more to find by going forwards
            ok = iterFuns.next (iter);
            enforce (! ok);
            enforce (! iterFuns.valid (iter));

            // Going back we find the last elem
            ok = iterFuns.prev (iter);
            enforce (ok);
            enforce (iterFuns.valid (iter));
            enforce (iterFuns.key (iter) == 444);
            enforce (iterFuns.array(iter).size == 0);
            
            iterFuns.dtr (&iter);
        }

        // And check the standard while loop form we use works
        {
            Iter *iter = iterFuns.ctr (store, txn);
            enforce (iter);

            int count = 0;
            // Cover all but the first entry
            bool some = iterFuns.upfrom (iter, 150);
            while (some) {
                ++count;
                some = iterFuns.next (iter);
            }
            
            enforce (count == 2);
            iterFuns.dtr (&iter);
        }

        // Now the main tests proper
        
        Iter *iter = iterFuns.ctr (store, txn);
        enforce (iter);

        // Start at first entry in the store (111)

        bool ok = iterFuns.upfrom (iter, 0);
        enforce (ok);
        enforce (iterFuns.valid (iter));

        enforce (iterFuns.key (iter) == 111);
        enforce (iterFuns.array(iter).size == data.size());
        enforce (iterFuns.array(iter).data[0] == data.at(0));

        // Then move to next entry (333)

        ok = iterFuns.next (iter);
        enforce (ok);
        enforce (iterFuns.valid (iter));

        enforce (iterFuns.key (iter) == 333);
        enforce (iterFuns.array(iter).size == 0);

        // Then move to the last

        ok = iterFuns.next (iter);
        enforce (ok);
        enforce (iterFuns.valid (iter));
        enforce (iterFuns.key (iter) == 444);
        enforce (iterFuns.array(iter).size == 0);

        // Try to run off the end, but doesn't move us

        ok = iterFuns.next (iter);
        enforce (! ok);
        enforce (iterFuns.valid (iter));  // we haven't moved

        // Going further when we're at the end shouldn't change anything

        ok = iterFuns.next (iter);
        enforce (! ok);
        enforce (iterFuns.valid (iter));  // still haven't moved

        // Check we're still at the last entry

        enforce (iterFuns.key (iter) == 444);
        enforce (iterFuns.array (iter) .size == 0);

        // Now back up along the others

        ok = iterFuns.prev (iter);
        enforce (ok);
        enforce (iterFuns.valid (iter));
        enforce (iterFuns.key (iter) == 333);
        enforce (iterFuns.array (iter) .size == 0);

        ok = iterFuns.prev (iter);
        enforce (ok);
        enforce (iterFuns.valid (iter));
        enforce (iterFuns.key (iter) == 111);
        enforce (iterFuns.array (iter) .size == data.size());
        enforce (iterFuns.array (iter) .data [0] == data.at(0));

        // Try to run off the beginning

        ok = iterFuns.prev (iter);
        enforce (! ok);
        enforce (iterFuns.valid (iter));  // haven't moved

        // Going back again now shouldn't change anything

        ok = iterFuns.prev (iter);
        enforce (! ok);
        enforce (iterFuns.valid (iter));  // still haven't moved

        // And check we're still on the same item

        enforce (iterFuns.key (iter) == 111);
        enforce (iterFuns.array (iter) .size == data.size());

        // Clean up

        iterFuns.dtr (&iter);
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
    
    testArrayStore (env, storeFuns_i32, iterFuns_i32,
                    "test_i32", {10, 11, 12, 13});
    
    testArrayStore (env, storeFuns_i64, iterFuns_i64,
                    "test_i64", {10, 11, 12, 13});
    
    testArrayStore (env, storeFuns_f32, iterFuns_f32,
                    "test_f32", {10.1, 11.1, 12.1, 13.1});

    testArrayStore (env, storeFuns_f64, iterFuns_f64,
                    "test_f64", {10.1, 11.1, 12.1, 13.1});

    testArrayStore (env, storeFuns_byte, iterFuns_byte,
                    "test_byte", {'a', 'b', 'c'});

    asenv_destroy (&env);

    puts ("");
    puts ("-- All tests passed");
}
