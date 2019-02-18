#include <cstdio>
#include <cerrno>
#include <cstdlib>

#include <lmdb.h>  // for mdb_strerror
#include <arraystore.h>
#include <arraystore.hpp>


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
//  main()

int main (int argc, char **argv) {
    enforce (argc == 2);
    char *dbpath = argv [1];

    // valgrind prefers smaller mapsizes
    auto env = arraystore::ASEnv {dbpath, 10 * 1024 * 1024};
    auto store = arraystore::I32Store {env, "cpp_i32_store"};

    {
        auto txn = arraystore::ASTxn::makeRdrw (env);
        uint64_t someKey = 555;
        
        enforce (! store.exists (txn, someKey));

        int dats[] = {1, 2, 3, 4, 5};
        store.put (txn, someKey, dats, 5);
        enforce (store.exists (txn, someKey));

        auto got = store.get (txn, someKey);
        assert (got.size() == 5);
        assert (got.at(0) == 1);
        assert (got.at(4) == 5);

        // txn rolls back here
    }

    puts ("-- All tests passed");
}
