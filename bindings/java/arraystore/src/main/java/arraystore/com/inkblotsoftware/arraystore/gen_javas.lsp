;;; Copyright (C) 2019 Inkblot Software Limited

;;; Entrypoint for writing template-based java source files

(load "stores.lsp")
(load "clib.lsp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Project constants

(setq all-store-types
      '("i32" "i64" "f32" "f64" "byte"))

(setq package "com.inkblotsoftware.arraystore");


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Write the JNA wrapper class

(letn ((model (list (list "package" package)
                    (list "store-types" all-store-types)))
       (filename "CLibrary.java")
       (filedata (Clib:template-java-clib-class model)))
  (println "-- Writing clib file: " filename)
  (write-file filename filedata))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Write the arraystore java files

(dolist (st all-store-types)
  (letn ((filename (string (Stores:java-classname st)
                          ".java"))
         (model (list (list "package" package)
                      (list "store-type" st)))
         (filedata (Stores:template-java-store-class model)))
    (println "-- Writing store file: " filename)
    (write-file filename filedata)))



(exit)
