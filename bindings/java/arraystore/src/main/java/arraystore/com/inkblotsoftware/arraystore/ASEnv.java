// Copyright (C) 2019 Inkblot Software Limited

package com.inkblotsoftware.arraystore;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;

public class ASEnv implements AutoCloseable {
    Pointer _handle;

    public ASEnv (String path) {
        _handle = CLibrary.INSTANCE.asenv_new (path);
        // TODO better exception
        if (_handle == Pointer.NULL)
            throw new RuntimeException ("ASEnv open failed");
    }

    public ASEnv (String path, long mapsize) {
        _handle = CLibrary.INSTANCE.asenv_new_mapsize (path, Size_t.of (mapsize));
        // TODO better exception
        if (_handle == Pointer.NULL)
            throw new RuntimeException ("ASEnv mapsize open failed");
    }

    public void close () {
        if (_handle == Pointer.NULL)
            return;
        CLibrary.INSTANCE.asenv_destroy (new PointerByReference (_handle));
        _handle = Pointer.NULL;
    }
}

    
