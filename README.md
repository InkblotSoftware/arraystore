arraystore
==========

Mutable memory-mapped array storage engine

High performance embedded data store for array-based structures. Particularly
designed as a storage engine for higher level special purpose databases
in OLAP-style applications, often written in higher-level languages.

Internally uses [LMDB](http://www.lmdb.tech/doc/) as a lower-level storage
engine, so inherits its zero-copy data access and cross-process ACID MVCC
semantics.


Data design and model
---------------------

arraystore assumes that all data in the world is made out of sets of arrays
of primitive types, each array carrying a uint64 key, and each set identified
with a string name. These sets themselves are stored within a database file.

The objective is to allow users to mimic on-disk whatever elaborate variation
of the 'struct of arrays' design they're using in RAM to make their queries
run fast. Because everything is memory mapped it's possible to use very similar
tricks, and often to get similar performance even though the overall set size
far exceeds working RAM capacity.

More formally:

- Each database contains one or more named array stores, with per-store
  element type T taken from {i32, i64, f32, f64, byte}
- Each database is a set of <uint64 (key), T[] (value)> pairs
- Pairs are ordered with key ascending, for the purposes of sequential
  traversal
- Pairs can be modified using traditional ACID transactions


Iteration and iterators
-----------------------

Many use cases can be handled with the store APIs directly, but if you don't
know what a store contains if or you want maximum traversal performance you'll
want to use an iterator. These take advantage of LMDB's B+ tree based design
to reduce most sequential store element accesses to one CPU cache miss, plus
a page fault if the data is cold and not in the OS cache.

Each store type has a corresponding iterator type, which can be created from
the corresponding store, and lets you move up and down the ascending-key-order
set of contained elements. Iterators, unsurprisingly, must not outlive the
transaction used to create them.

After constructing an iterator call _upfrom(key) to start start iterating
from that key, using _next() and _prev() to move around. Each function returns
a bool telling you whether it successfully moved to new data, or that there
was no new element to visit so it stayed at the same place. Use _upfrom(0) to
traverse the whole store.

An iterator's current element can be accessed using _key() and _array(). Note
that until it's successfully visited at least one element - which won't happen
if the store is empty, or you start too far up and never call _prev() - the
iterator remains in an 'invalid' state, and you MUST NOT call _key() or _array().
If you think you might misuse the return upfrom/next/prev return values and get
this wrong, you can call _valid() in an assert to make sure.


Building and installation
-------------------------

arraystore is built with CMake, so you can include in your own projects in
the normal way.

If you want to do a one-off build, and optionally a system-wide installation,
the following will succeed:

```sh
git clone https://github.com/InkblotSoftware/arraystore
cd arraystore
mkdir build && cd build
## Add "-DBUILD_SHARED_LIBS=ON" to build a .so rather than an .a
cmake .. -DCMAKE_BUILD_TYPE=Release
make
make install
```

Assuming you're on a Linux or similar system, you can run the project tests
using `make check` and `make memcheck` (the latter adds valgrind).


Project classes
---------------

Reference-based classes (based on standard C types):

- **asenv_t** - Database environment - start by opening a DB via one of these
- **astxn_t** - Database transaction - all interaction happens via one of these
- **byteas_t** - Array store of byte arrays
- **i32as_t** - Of int32 arrays
- **i64as_t** - Of int64 arrays
- **f32as_t** - Of 32-bit float arrays
- **f64as_t** - Of 64-bit float arrays
- **byteasiter_t** - Iterator across byte array store
- **i32asiter_t** - Across int32 array store
- **i64asiter_t** - Across int64 array store
- **f32asiter_t** - Across 32-bit float array store
- **f64asiter_t** - Across 64-bit float array store

Value types (based on standard C types):

- **bytespan** - Span of contiguous immutable bytes in memory
- **i32span** - Of int32s
- **i64span** - Of int64s
- **f32span** - Of 32-bit floats 
- **f64span** - Of 64-bit floats


Code usage example
------------------

```c
// #include <arraystore.h>

// Open database environment
asenv_t *env = asenv_new ("my_database.db");
assert (env);   // NULL on error

// Open an 'array store' of int32 arrays, called 'some_ints'
i32as_t *store = i32as_new (env, "some_ints"); 
assert (store);   // NULL on error

// Array store keys are all uint64s
uint64_t my_key = 98989;

// Write some data
{
    astxn_t *txn = astxn_new_rdrw (env);
    assert (txn);   // NULL on error
    
    int32_t data[] = {1, 2, 3};
    int rc = i32as_put (store, txn, my_key, data, 3);
    assert (!rc);   // 0 on success

    rc = astxn_commit (txn);
    assert (!rc);   // 0 on success
    
    astxn_destroy (&txn);   // dtrs are idempotent
}

// Read it back
{
    astxn_t *txn = astxn_new_rdonly (env);
    assert (txn);
    
    i32span sp = i32as_get (store, txn, my_key);
    assert (sp.data);   // .data is NULL on GET failure
    assert (sp.size == 3);
    assert (sp.data[2] == 3);
    
    astxn_destroy (&txn);
}

// Traverse the store via an iterator
{
    astxn_t *txn = astxn_new_rdonly (env);
    assert (txn);

    // NB iterator must not outlive transaction
    i32asiter_t *iter = i32asiter_new (store, txn);
    assert (iter);
    
    // Traverse all entries in the store
    bool some = i32asiter_upfrom (iter, 0);
    while (some) {
        assert (i32asiter_key (iter) == my_key);
        assert (i32asiter_array(iter).size == 3);
        assert (i32asiter_array(iter).data[2] == 3);
        some = i32asiter_next (iter);  // try to move to next entry
    }

    i32asiter_destroy (&iter);
    astxn_destroy (&txn);
}

// Clean up
i32as_destroy (&store);
asenv_destroy (&env);
```

This program simply needs linking with -larraystore, and including its header
directory. CMake can do this automatically for you, as the project is already
set up for inclusion.


Usage example case studies
--------------------------

### Time-series data

As an initial, simple example, suppose you have a set of IoT devices broadcasting
time-series data, composed of a 32 bit timestamp, a one-byte device ID, and a
64 bit float activity level. You want to store these as a chunked struct of
arrays, with chunk members sorted to have all broadcasts with ID together, in
ascending ID order, and with per-ID broadcasts in time ascending order; this will
allow you to write much faster query code.

We would create three array stores to model this data, and would separate out
broadcasts into arrays with - perhaps - one array for each hour of time:

- **int32_t** - "message_timestamps"
- **byteas_t** - "message_ids"
- **f64as_t** - "message_activities"

To get the value of a given message identified by a known
<device_id, timestamp>, the caller must calculate its chunk ID from the
timestamp, and then traverse the messages in the chunk until the correct one is
found.

In practice, most performance-orientated systems would create a third arraystore
containing the positions of each device ID span within each chunk, perhaps as
a pair of int32s (offset, length).


### Quadtrees and sparse data

As a more complicated example, suppose we have a set of sparse two-dimensional
points with activity levels, which we want to store in an efficient and
performant manner. A natural choice is a
[quadtree](https://en.wikipedia.org/wiki/Quadtree), which divides up space into
groups of four cells each of which can own points, and which are subdevided
whenever too many points are stored within one cell. Points can be found by within
a region by descending the tree inside it, examining the contents only of the leaf
cells (which contain the data) there.

We'll use a fairly straightforward implementation first, and use arbitrary uint64
keys for the cells without any particular meaning. This leads to the following
stores:

- **ui64as_t**  -"cell_children" - present if a cell is subdivided. Key is
  cell id, contents is a 4-array of cell ids, being the clockwise children
  of the cell
- **f32as_t** - "cell_points_xs" - present for any leaf cell. SOA member for
  x values of all points within that cell
- **f32as_t** - "cell_points_ys" - as above, but y values
- **f64as_t** - "cell_points_activities" - as above, but activity level

Each subdivided cell as has exactly one entry in the cell_children store, and
each leaf cell has exacly one entry in each points store. The three fields making
up each point are stored at the same offset in the same keyed array across the
three point stores.

Subdividing over-full cells is fairly straightforward, consisting of allocating
up to four new cell keys, moving old-cell leaf data into array sets for the new
cells, and adding a children entry for the old (now-split) cell.

More performant quadtree implementations can be created by using semantics-bearing
keys and exploting the database key-ascending ordering to create locality among
physically-colocated cells, allowing tree descending by mostly-sequential access.


Concurrency and caveats
-----------------------

The rules and limitations of this project are the same as LMDB.

Databases may be opened by multiple processes at the same time, but you MUST
not open the same database twice in the same process (i.e. don't construct two
`asenv_t` objects for the same DB file). Further, once you've created an `asenv_t`
object, only call `asenv_xxx()` functions on it from the thread that created it.

Transactions (`astxn_t`) can be created on any thread, but afterwards may only be
used by the thread that created them. In practice this covers all the `astxn_xxx()`
functions, all the xxxiter_yyy() iterator functions, and any function taking a
transaction as a parameter.

Data spans returned on a `xxx_get()` call remain valid for as long as the
transaction used to perform the get is not closed. After this time the span's
(old) data MUST NOT be accessed.

Since (most) data cannot be cleared from the database while any transaction is
open, leading to size bloat, it is important to keep transactions as short-lived
as possibe. In practice this is usually not difficult, but note the above
requirement that spans' data must not be accessed after the parent transaction
is closed.


Ownership and license
---------------------

arraystore is Copyright (C) 2019 Inkblot Software Limited. It's released under
the Mozilla Public License v2.0. This is BSD-like if you just want to use the
library.

arraystore also includes [LMDB](http://www.lmdb.tech/doc/), which is licensed
under the OpenLDAP Public License 2.8. This license is again BSD-like.


Full API
--------

```c
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
```
