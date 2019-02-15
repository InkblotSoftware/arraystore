// Copyright (C) 2019 Inkblot Software Limited

package com.inkblotsoftware.arraystore;
import com.sun.jna.IntegerType;
import com.sun.jna.Native;

public class UInt64 extends IntegerType {
    public UInt64 () {
        this (0);
    }

    public UInt64 (long value) {
        super (4, value, true);
    }

    static UInt64 of (long value) {
        return new UInt64 (value);
    }
}
    

