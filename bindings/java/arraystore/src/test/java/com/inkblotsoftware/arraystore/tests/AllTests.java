// Copyright (C) 2019 Inkblot Software Limited

// TODO write more tests, especially of the hand-written classes and
// CLibrary, and also do full-ish coverage of at least one
// template-generated store class

package com.inkblotsoftware.arraystore.tests;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import org.junit.Test;

import com.inkblotsoftware.arraystore.*;
import java.nio.IntBuffer;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.Files;

public class AllTests {
    static final Path dbPath = Paths.get
        ("ARRAYSTORE_MVN_TEST_SCRATCH_DB.db");
    static final Path dbLockPath = Paths.get
        ("ARRAYSTORE_MVN_TEST_SCRATCH_DB.db-lock");

    static void tryDeleteScratchDB () throws Exception {
        if (Files.exists (dbPath)) {
            System.out.println ("--- Deleting scratch db");
            Files.delete (dbPath);
            Files.delete (dbLockPath);
        }
    }        
    
    @Test
    public void allTests () throws Exception {
        try {
            tryDeleteScratchDB ();
        
            long someKey = 555;

            // Valgrind prefers smaller mapsizes
            try (ASEnv    env   = new ASEnv (dbPath.toString(), 100*1024*1024);
                 I32Store store = new I32Store (env, "my_i32_store");
                 ASTxn    txn   = ASTxn.newRdrw (env)) {
                assertFalse (store.exists (txn, someKey));

                store.put (txn, someKey, new int[] {1,2,3,4,5});
                assertTrue (store.exists (txn, someKey));

                IntBuffer vals = store.get (txn, someKey);
                assertTrue (vals.limit() == 5);
                assertTrue (vals.get (0) == 1);
                assertTrue (vals.get (4) == 5);

                // txn rolls back here
            }
        } finally {
            tryDeleteScratchDB ();
        }
    }
}
