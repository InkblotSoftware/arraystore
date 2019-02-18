;;; Copyright (C) 2019 Inkblot Software Limited

;;; C language specific code generation

(context 'Cc)

(define (prim-type primtype)
  (case primtype
    ("i32" "int32_t")
    ("i64" "int64_t")
    ("f32" "float")
    ("f64" "double")
    ("byte" "unsigned char")
    (true (throw-error (string "Unknown prim type: " ty)))))
