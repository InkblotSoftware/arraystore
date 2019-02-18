(load "../../cogen/cogen.lsp")
(load "../../cogen/cc.lsp")
(load "../../cogen/banners.lsp")


(define (c-store-class primtype)
  (string primtype "as"))

(define (c-span-class primtype)
  (string primtype "span"))

(define (cpp-store-class primtype)
  (string (title-case primtype) "Store"))

(define (cpp-span-class primtype)
  (string (title-case primtype) "Span"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TEMPLATE: Span class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pass in a model with "elem-type" set to a primtype
(setq template-cpp-span-class
      (Cogen:compile-template (context)
[text]
.(letn ((et   (M "elem-type"))
.       (cet  (Cc:prim-type et))
.       (csp  (c-span-class et))
.       (cpsp (cpp-span-class et)))
class <% cpsp %> {
    <% csp %> _sp {nullptr, 0};
public:
    const <% cet %> * begin () const { return _sp.data; }
    const <% cet %> * end   () const { return _sp.data + _sp.size; }
    size_t          size  () const { return _sp.size; }

    <% cet %> operator[] (size_t n) { assert (_sp.data); return _sp.data [n]; }
    <% cet %>         at (size_t n) {
        if (n >= size()) throw std::length_error {"Outside span"};
        return operator[] (n);  }

    <% cpsp %> (<% csp %> sp) { _sp = sp; }
};
.)  ; letn
[/text]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TEMPLATE: store class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pass in a model with "elem-type" set to a primtype
(setq template-cpp-store-class
      (Cogen:compile-template (context)
[text]
.(letn ((et   (M "elem-type"))
.       (cet  (Cc:prim-type et))
.       (csp  (c-span-class et))
.       (cpsp (cpp-span-class et))
.       (cst  (c-store-class et))
.       (cpst (cpp-store-class et)))
.
class <% cpst %> {
    <% cst %>_t *_handle = nullptr;
public:

    <% cpst %>(const <% cpst %>&) =delete;
    <% cpst %>(<% cpst %> &&other) {
        _handle = other._handle;
        other._handle = nullptr;
    }

    <% cpst %> (ASEnv &env, const char *name) {
        _handle = <% cst %>_new (env._handle, name);
        // TODO better exception
        if (! _handle)
            throw std::runtime_error {"Open failed"};
    }
    ~<% cpst %> () {
        <% cst %>_destroy (&_handle);
    }

    <% cpsp %> get (ASTxn &txn, Key key) {
        <% csp %> sp = <% cst %>_get (_handle, txn._handle, key);
        // TODO better exception
        if (sp.data == nullptr)
            throw std::runtime_error {"GET failed"};
        return <% cpsp %> {sp};
    }

    void put (ASTxn &txn, Key key, const <%cet%> *data, size_t size) {
        int rc = <% cst %>_put (_handle, txn._handle, key, data, size);
        // TODO better exception
        if (rc)
            throw std::runtime_error {"PUT failed"};
    }

    bool exists (ASTxn &txn, Key key) {
        return <% cst %>_exists (_handle, txn._handle, key);
    }
};
.)  ; letn
[/text]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TEMPLATE: entire file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq template-entire-file
      (Cogen:compile-template (context)
[text]<% Banners:copyright %>
<% Banners:autogenned %>

#pragma once

#include <arraystore.h>
#include <cassert>
#include <stdexcept>

namespace arraystore {

using Key = int64_t;


//  ----------------------------------------------------------------------
//  Publicly-visible classes

class ASEnv;
class ASTxn;
.(dolist (st (M "store-types"))
class <%(cpp-store-class st)%>;
.)  ; dolist


//  ----------------------------------------------------------------------
//  asenv_t wrapper

class ASEnv {
    asenv_t *_handle = nullptr;
public:
    friend class ASTxn;
.(dolist (st (M "store-types"))
    friend class <%(cpp-store-class st)%>;
.)  ; dolist

    ASEnv(const ASEnv&) =delete;

    ASEnv (ASEnv &&other) {
        _handle = other._handle;
        other._handle = nullptr;
    }

    ASEnv (const char *dbpath) {
        _handle = asenv_new (dbpath);
        // TODO better exception
        if (! _handle)
            throw std::runtime_error {"Open failed"};
    }
    ASEnv (const char *dbpath, size_t mapsize) {
        _handle = asenv_new_mapsize (dbpath, mapsize);
        // TODO better exception
        if (! _handle)
            throw std::runtime_error {"Open failed"};
    }
    ~ASEnv () {
        asenv_destroy (&_handle);        
    }
};


//  ----------------------------------------------------------------------
//  astxn_t wrapper

class ASTxn {
    astxn_t *_handle = nullptr;
    ASTxn (astxn_t *han) { _handle = han; }
public:
.(dolist (st (M "store-types"))
    friend class <%(cpp-store-class st)%>;
.)  ; dolist

    ASTxn(const ASTxn&) =delete;
    ~ASTxn () {
        astxn_destroy (&_handle);
    }
    ASTxn (ASTxn &&other) {
        _handle = other._handle;
        other._handle = nullptr;
    }

    static ASTxn makeRdrw (ASEnv &env) {
        astxn_t *han = astxn_new_rdrw (env._handle);
        // TODO better exception
        if (! han)
            throw std::runtime_error {"Txn open failed"};
        return ASTxn {han};
    }
    static ASTxn makeRdonly (ASEnv &env) {
        astxn_t *han = astxn_new_rdonly (env._handle);
        // TODO better exception
        if (! han)
            throw std::runtime_error {"Txn open failed"};
        return ASTxn {han};
    }
};


.(dolist (st (M "store-types"))
//  ----------------------------------------------------------------------
//  "<% (Cc:prim-type st) %>" span
<% (template-cpp-span-class  (expand '(("elem-type" st)) 'st)) %>
//  ----------------------------------------------------------------------
//  "<% (Cc:prim-type st) %>" array store
<% (template-cpp-store-class (expand '(("elem-type" st)) 'st)) %>
.)  ; dolist
}  // namespace arraystore[/text]))
                              


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Write out the file

(setq model '(("store-types" ("i32" "i64" "f32" "f64" "byte"))))

(let ((filename "arraystore.hpp")
      (filedata (template-entire-file model)))
  (println "-- Writing file " filename)
  (write-file filename filedata))



(exit)
