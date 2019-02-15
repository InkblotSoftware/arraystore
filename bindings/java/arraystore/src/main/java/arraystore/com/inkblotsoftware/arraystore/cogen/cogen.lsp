;;; Programmable source code generator
;;;
;;; The basic idea is that you compile templates to lambdas, then call
;;; these with models (which are all alists).
;;; Proper docs to come when this is in its own repo.

(context 'Cogen)


;;; Is a given line a pure-code-to-be-executed line?
(define (is-code-line str)
  (and (> (length str) 0)
       (= (str 0) ".")))

;;; Translate a non '.' template line into a newlisp-code-string line
(define (tmpl-line-to-fn-line line)
  (let ((cl (copy line)))
    (replace "<%(.*?)%>"
             cl
             (string "[/text])" "(emit " $1 ")" "(emit [text]")
             0)
    (string "(begin "
            "(emit [text]"
            cl
            "[/text])"
            ")")))

;;; Turn a template line into a newlisp-like code line (but these still
;;; have to be merged together before read-expr'ing
(define (mangle-line str)
  (if (is-code-line str)
      (rest str)
      (tmpl-line-to-fn-line str)))

;;; Generate the string that will be eval-string'd do a lambda
;;; Note that you can use the M function for lookup into the model provided
;;; to the template at user-eval time.
(define (gen-template-lambda-str str)
  (letn ((lines (map (fn (s) (string s "\n"))
                     (parse str "\n")))
         (mang-lines (map mangle-line lines)))
    (string "(lambda (model)\n"
            "  (let ((RES [text][/text])\n"
            "        (emit (lambda (s) (push (string s) RES -1)))\n"
            "        (M (lambda (key) (or (lookup key model)\n"
            "                             (throw-error 'MISSING-M-KEY)))))\n"
            "    (begin\n"
            (join mang-lines "\n")
            "\nRES)))")))

;;; Client entrypoint: given a template string and the context you'd like
;;; to do user-provided symbol resolution in, compile it to a lambda and return
(define (compile-template ctx str)
  (eval-string (gen-template-lambda-str str)
               ctx))
