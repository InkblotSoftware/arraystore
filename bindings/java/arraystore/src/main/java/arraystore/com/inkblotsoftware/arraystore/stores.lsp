;;; Copyright (C) 2019 Inkblot Software Limited

;;; Array stores support/generation code
;;;
;;; The basic idea is that each store is defined by a prim type (i32 etc)

(load "cogen/cogen.lsp")
(load "cogen/java.lsp")
(load "banners.lsp")

(context 'Stores)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  What are the store classes called? 

(define (java-classname store-type)
  (title-case (string store-type "Store")))

(define (c-classname store-type)
  (string store-type "as"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TEMPLATE: higher level java wrapper class

(setq template-java-store-class
      (Cogen:compile-template (context)
[text]<% Banners:autogenned %>
<% Banners:copyright %>

package <%(M "package")%>;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.FloatBuffer;
import java.nio.DoubleBuffer;

.(letn ((st  (M "store-type"))
.       (cn  (java-classname st))
.       (ccn (c-classname st))
.       (pt  (Java:prim-type st))
.       (tpt (title-case pt)))
.
public class <% cn %> implements AutoCloseable {
    Pointer _handle;
    
    public <% cn%> (ASEnv env, String name) {
        _handle = CLibrary.INSTANCE.<% ccn %>_new (env._handle, name);
        // TODO better exception
        if (_handle == Pointer.NULL) {
            throw new RuntimeException ("Open failed");
        }
    }

    @Override public void close () {
        if (_handle == Pointer.NULL)
           return;
        CLibrary.INSTANCE.<% ccn %>_destroy (new PointerByReference (_handle));
        _handle = Pointer.NULL;
    }

    public <%tpt%>Buffer get (ASTxn txn, long key) {
        CLibrary.<%st%>span.ByValue sp =
            CLibrary.INSTANCE.<% ccn %>_get (_handle, txn._handle, UInt64.of(key));
        assert (sp != null);

        // TODO better exception
        if (sp.data == null)
            throw new RuntimeException ("Key not found");

        long numBytes = sp.size.longValue() * <%(Java:prim-type-size st)%>;
        ByteBuffer bb = sp.data.getPointer ()
                            .getByteBuffer (0, numBytes);
.(if (= st "byte")
        return bb;
        return bb.as<% tpt %>Buffer ();
.)
    }

    public void put (ASTxn txn, long key, <%pt%>[] data) {
        put (txn, key, <%tpt%>Buffer.wrap (data));
    }
    public void put (ASTxn txn, long key, <%tpt%>Buffer data) {
        int rc = CLibrary.INSTANCE.<%ccn%>_put (_handle, txn._handle,
                                               UInt64.of (key),
                                               data, Size_t.of (data.limit()));
        // TODO better exception
        if (rc != 0)
            throw new RuntimeException ("put() failed");
    }

    public boolean exists (ASTxn txn, long key) {
        int res = CLibrary.INSTANCE.<%ccn%>_exists (_handle, txn._handle,
                                                    UInt64.of (key));
        return res == 1;
    }
}
.)  ; letn
[/text]))
