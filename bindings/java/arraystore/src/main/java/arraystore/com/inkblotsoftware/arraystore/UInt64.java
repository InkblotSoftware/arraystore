package com.inkblotsoftware.arraystore;
import com.sun.jna.IntegerType;
import com.sun.jna.Native;

public class UInt64 extends IntegerType {
    public UInt64 () {
        this (0);
    }

    public UInt64 (long value) {
        // // TODO better exception
        // if (value < 0)
        //     throw new RuntimeException ("UInt can't be negative");
        super (4, value, true);
    }

    static UInt64 of (long value) {
        return new UInt64 (value);
    }
}
    

