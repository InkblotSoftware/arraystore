;;; Copyright (C) 2019 Inkblot Software Limited

;;; Java-specific helpers for cogen

(context 'Java)


;;; Translating language agnostic primitive types to Java type names
(define (prim-type ty)
  (case ty
    ("i32" "int")
    ("i64" "long")
    ("f32" "float")
    ("f64" "double")
    ("byte" "byte")
    (true (throw-error "Unknown prim type"))))

;;; How big are the prim types
;;; TODO move this into Cogen proper, it's not java specific
(define (prim-type-size ty)
  (case ty
    ("i32" 4)
    ("i64" 8)
    ("f32" 4)
    ("f64" 8)
    ("byte" 1)))
