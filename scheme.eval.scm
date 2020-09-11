(module scheme.eval (eval environment)
  (import (rename scheme (eval %eval)))
  (import chicken.base chicken.type)
  (import r7rs-library)

;;;
;;; 6.12. Environments and evaluation
;;;

  (: eval (* (struct environment) -> *))

  (define (eval expr env) (%eval expr env))

  (: environment (#!rest list -> (struct environment)))

  (define (environment . specs)
    (let ((name (gensym "environment-module-")))
      (define (delmod)
	(and-let* ((modp (assq name ##sys#module-table)))
	  (set! ##sys#module-table (delq modp ##sys#module-table))))
      (define (delq x lst)
        (let loop ([lst lst])
          (cond ((null? lst) lst)
	        ((eq? x (##sys#slot lst 0)) (##sys#slot lst 1))
	        (else (cons (##sys#slot lst 0) (loop (##sys#slot lst 1)))) ) ) )
      (dynamic-wind
       void
       (lambda ()
	 ;; create module...
	 (%eval `(module ,name ()
		   (import r7rs) ; for `import`
		   ,@(map (lambda (spec)
			    `(import ,(fixup-import/export-spec spec 'environment)))
			  specs)))
	 (let ((mod (##sys#find-module name)))
	   (##sys#make-structure 'environment
	    (cons 'import specs)
	    (let ((env (##sys#slot mod 13)))
	      (append (car env) (cdr env))) ; combine env and syntax bindings
	    #t)))
       ;; ...and remove it right away
       delmod)))

)
