;;; Copyright (C) 2019 Inkblot Software Limited

;;; JNA c library wrapper generation code (project specific)

(load "../../../../../../../../../cogen/cogen.lsp")
(load "../../../../../../../../../cogen/java.lsp")
(load "../../../../../../../../../cogen/banners.lsp")

(context 'Clib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TEMPLATE: main class java code

(setq template-java-clib-class
      (Cogen:compile-template (context)
[text]<% Banners:autogenned %>
<% Banners:copyright %>

package <%(M "package")%>;
import com.sun.jna.Native;
import com.sun.jna.Structure;
import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;
.(dolist (st (M "store-types"))
.  (let ((tpt (title-case (Java:prim-type st))))
import java.nio.<%tpt%>Buffer;
import com.sun.jna.ptr.<%tpt%>ByReference;
.))


public interface CLibrary extends Library {
    static final CLibrary INSTANCE = (CLibrary) Native.load
                                         ("arraystore", CLibrary.class);


    //  ------------------------------------------------------------
    //  asenv_t

    Pointer asenv_new         (String path);
    Pointer asenv_new_mapsize (String path, Size_t mapsize);
    void    asenv_destroy     (PointerByReference self_p);


    //  ------------------------------------------------------------
    //  astxn_t

    Pointer astxn_new_rdrw   (Pointer env);
    Pointer astxn_new_rdonly (Pointer env);
    void    astxn_destroy    (PointerByReference self_p);
    boolean astxn_is_rdonly  (Pointer self);
    boolean astxn_is_rdrw    (Pointer self);
    void    astxn_abort      (Pointer self);
    int     astxn_commit     (Pointer self);


    //  ------------------------------------------------------------
    //  Store classes + spans
.
.(dolist (st (M "store-types"))
.  (let ((sn  (Stores:c-classname st))
.        (pt  (Java:prim-type st))
.        (tpt (title-case (Java:prim-type st))))

    // -- <%sn%>_t
    
    @Structure.FieldOrder({"data", "size"})
    public static class <%st%>span extends Structure {
        public static class ByValue
            extends <%st%>span implements Structure.ByValue {}
        public <%tpt%>ByReference data;
        public Size_t size;
    }
    
    Pointer <%sn%>_new     (Pointer env, String name);
    void    <%sn%>_destroy (PointerByReference self_p);
    int     <%sn%>_put     (Pointer self, Pointer txn,
                            UInt64 key,
                            <%tpt%>Buffer data, Size_t size);
    int    <%sn%>_delete   (Pointer self, Pointer txn, UInt64 key);
    int    <%sn%>_exists   (Pointer self, Pointer txn, UInt64 key);
    <%st%>span.ByValue
           <%sn%>_get (Pointer self, Pointer txn, UInt64 key);
.))  ; dolist
}
[/text]))

