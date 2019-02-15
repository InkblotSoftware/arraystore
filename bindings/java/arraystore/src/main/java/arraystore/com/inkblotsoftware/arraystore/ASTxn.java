package com.inkblotsoftware.arraystore;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;


public class ASTxn implements AutoCloseable {
    //  ------------------------------------------------------------
    //  Private
    
    Pointer _handle;

    private ASTxn (ASEnv env, boolean rdrw) {
        if (rdrw) {
            _handle = CLibrary.INSTANCE.astxn_new_rdrw (env._handle);
        } else {
            _handle = CLibrary.INSTANCE.astxn_new_rdonly (env._handle);
        }
        if (_handle == Pointer.NULL)
            throw new RuntimeException ("ASTxn open failed"); // TODO better
    }

    public void close () {
        if (_handle == Pointer.NULL)
            return;
        CLibrary.INSTANCE.astxn_destroy (new PointerByReference (_handle));
        _handle = Pointer.NULL;
    }

    
    //  ------------------------------------------------------------
    //  Factory funs

    public static ASTxn newRdrw (ASEnv env) {
        return new ASTxn (env, true);
    }
    public static ASTxn newRdonly (ASEnv env) {
        return new ASTxn (env, false);
    }


    //  ------------------------------------------------------------
    //  Accessors

    public boolean isRdrw () {
        return CLibrary.INSTANCE.astxn_is_rdrw (_handle);
    }
    public boolean isRdonly () {
        return CLibrary.INSTANCE.astxn_is_rdonly (_handle);
    }


    //  ------------------------------------------------------------
    //  commit/abort

    public void abort () {
        CLibrary.INSTANCE.astxn_abort (_handle);
    }
    public void commit () {
        int rc = CLibrary.INSTANCE.astxn_commit (_handle);
        if (rc != 0)
            throw new RuntimeException ("ASTxn commit failed"); // TODO better
    }
}
                
