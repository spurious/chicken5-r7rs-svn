;;
;; This is a slightly modified copy of core syntax-rules, enhanced
;; with underscore "wildcard" patterns and the ellipsis (... ...)
;; "quoting" mechanism from R7RS.
;;
;; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; The syntax-rules macro (new in R5RS)

;;; [Hacked slightly by Taylor R. Campbell to make it work in his
;;; macro expander `riaxpander'.]

;; [Hacked even more by Felix L. Winkelmann to make it work in his
;; Hi-Lo expander]

; Example:
;
; (define-syntax or
;   (syntax-rules ()
;     ((or)          #f)
;     ((or e)        e)
;     ((or e1 e ...) (let ((temp e1))
;		       (if temp temp (or e ...))))))


(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (exp r c)
     (##sys#check-syntax 'syntax-rules exp '#(_ 2))
     (let ((subkeywords (cadr exp))
           (rules (cddr exp))
           (ellipsis '...))
       (when (symbol? subkeywords)
         (##sys#check-syntax 'syntax-rules exp '(_ _ list . #(_ 0)))
         (set! ellipsis subkeywords)
         (set! subkeywords (car rules))
         (set! rules (cdr rules)))
       (process-syntax-rules ellipsis rules subkeywords r c)))))

(begin-for-syntax
 (define (process-syntax-rules ellipsis rules subkeywords r c)

   (define %append '##sys#append)
   (define %apply '##sys#apply)
   (define %and (r 'and))
   (define %car '##sys#car)
   (define %cdr '##sys#cdr)
   (define %length '##sys#length)
   (define %vector? '##sys#vector?)
   (define %vector->list '##sys#vector->list)
   (define %list->vector '##sys#list->vector)
   (define %>= '##sys#>=)
   (define %= '##sys#=)
   (define %+ '##sys#+)
   (define %compare (r 'compare))
   (define %cond (r 'cond))
   (define %cons '##sys#cons)
   (define %else (r 'else))
   (define %eq? '##sys#eq?)
   (define %equal? '##sys#equal?)
   (define %input (r 'input))
   (define %l (r 'l))
   (define %len (r 'len))
   (define %lambda (r 'lambda))
   (define %let (r 'let))
   (define %let* (r 'let*))
   (define %list? '##sys#list?)
   (define %loop (r 'loop))
   (define %map1 '##sys#map)
   (define %map '##sys#map-n)
   (define %pair? '##sys#pair?)
   (define %quote (r 'quote))
   (define %rename (r 'rename))
   (define %tail (r 'tail))
   (define %temp (r 'temp))
   (define %syntax-error '##sys#syntax-error-hook)
   (define %ellipsis (r ellipsis))
   (define %take-right (r 'chicken.internal.syntax-rules#take-right))
   (define %drop-right (r 'chicken.internal.syntax-rules#drop-right))
   (define %syntax-rules-mismatch
     (r 'chicken.internal.syntax-rules#syntax-rules-mismatch))

   (define (ellipsis? x)
     (c x %ellipsis))

   ;; R7RS support: underscore matches anything
   (define (underscore? x)
     (c x (r '_)))

   (define (make-transformer rules)
     `(##sys#er-transformer
       (,%lambda (,%input ,%rename ,%compare)
                 (,%let ((,%tail (,%cdr ,%input)))
                        (,%cond ,@(map process-rule rules)
                                (,%else
                                 (,%syntax-error "no rule matches form" ,%input)))))))

   (define (process-rule rule)
     (if (and (pair? rule)
              (pair? (cdr rule))
              (null? (cddr rule)))
         (let ((pattern (cdar rule))
               (template (cadr rule)))
           `((,%and ,@(process-match %tail pattern #f ellipsis?))
             (,%let* ,(process-pattern pattern
                                       %tail
                                       (lambda (x) x) #f ellipsis?)
                     ,(process-template template
                                        0
                                        ellipsis?
                                        (meta-variables pattern 0 ellipsis? '() #f)))))
         (##sys#syntax-error-hook "ill-formed syntax rule" rule)))

   ;; Generate code to test whether input expression matches pattern

   (define (process-match input pattern seen-segment? el?)
     (cond ((symbol? pattern)
            (if (memq pattern subkeywords)
                `((,%compare ,input (,%rename (##core#syntax ,pattern))))
                `()))
           ((segment-pattern? pattern seen-segment? el?)
            (process-segment-match input pattern el?))
           ((pair? pattern)
            `((,%let ((,%temp ,input))
                     (,%and (,%pair? ,%temp)
                            ,@(process-match `(,%car ,%temp) (car pattern) #f el?)
                            ,@(process-match `(,%cdr ,%temp) (cdr pattern) #f el?)))))
           ((vector? pattern)
            `((,%let ((,%temp ,input))
                     (,%and (,%vector? ,%temp)
                            ,@(process-match `(,%vector->list ,%temp)
                                             (vector->list pattern) #f el?)))))
           ((or (null? pattern) (boolean? pattern) (char? pattern))
            `((,%eq? ,input ',pattern)))
           (else
            `((,%equal? ,input ',pattern)))))

   (define (process-segment-match input pattern el?)
     (let ((conjuncts (process-match `(,%car ,%l) (car pattern) #f el?)))
       `((,%and (,%list? ,input) ; Can't ask for its length if not a proper list
                (,%let ((,%len (,%length ,input)))
                       (,%and (,%>= ,%len ,(length (cddr pattern)))
                              (,%let ,%loop ((,%l ,input)
                                             (,%len ,%len))
                                     (,%cond
                                      ((,%= ,%len ,(length (cddr pattern)))
                                       ,@(process-match %l (cddr pattern) #t el?))
                                      (,%else
                                       (,%and ,@conjuncts
                                              (,%loop (,%cdr ,%l) (,%+ ,%len -1))))))))))))

   ;; Generate code to take apart the input expression
   ;; This is pretty bad, but it seems to work (can't say why).

   (define (process-pattern pattern path mapit seen-segment? el?)
     (cond ((symbol? pattern)
            (if (or (memq pattern subkeywords) (underscore? pattern))
                '()
                (list (list pattern (mapit path)))))
           ((segment-pattern? pattern seen-segment? el?)
            (let* ((tail-length (length (cddr pattern)))
                   (%match (if (zero? tail-length) ; Simple segment?
                               path ; No list traversing overhead at runtime!
                               `(,%drop-right ,path ,tail-length))))
              (append
               (process-pattern (car pattern)
                                %temp
                                (lambda (x) ;temp is free in x
                                  (mapit
                                   (if (eq? %temp x)
                                       %match ; Optimization: no map+lambda
                                       `(,%map1 (,%lambda (,%temp) ,x) ,%match))))
                                #f el?)
               (process-pattern (cddr pattern)
                                `(,%take-right ,path ,tail-length)
                                mapit #t el?))))
           ((pair? pattern)
            (append (process-pattern (car pattern) `(,%car ,path) mapit #f el?)
                    (process-pattern (cdr pattern) `(,%cdr ,path) mapit #f el?)))
           ((vector? pattern)
            (process-pattern (vector->list pattern)
                             `(,%vector->list ,path) mapit #f el?))
           (else '())))

   ;; Generate code to compose the output expression according to template

   (define (process-template template dim el? env)
     (cond ((symbol? template)
            (let ((probe (assq template env)))
              (if probe
                  (if (<= (cdr probe) dim)
                      template
                      (##sys#syntax-error-hook
                       "template dimension error (too few ellipses?)"
                       template))
                  `(,%rename (##core#syntax ,template)))))
           ((ellipsis-escaped-pattern? template el?)
            (if (or (not (pair? (cdr template))) (pair? (cddr template)))
                (##sys#syntax-error-hook "Invalid escaped ellipsis template" template)
                (process-template (cadr template) dim (lambda _ #f) env)))
           ((segment-template? template el?)
            (let* ((depth (segment-depth template el?))
                   (seg-dim (+ dim depth))
                   (vars
                    (free-meta-variables (car template) seg-dim el? env '())))
              (if (null? vars)
                  (##sys#syntax-error-hook "too many ellipses" template)
                  (let* ((x (process-template (car template) seg-dim el? env))
                         (gen (if (and (pair? vars)
                                       (null? (cdr vars))
                                       (symbol? x)
                                       (eq? x (car vars)))
                                  x	;+++
                                  `(,%map (,%lambda ,vars ,x)
                                          ,@vars)))
                         (gen (do ((d depth (- d 1))
                                   (gen gen `(,%apply ,%append ,gen)))
                                  ((= d 1)
                                   gen)))
                         (tail (segment-tail template el?)))
                    (if (null? tail)
                        gen		;+++
                        `(,%append ,gen ,(process-template tail dim el? env)))))))
           ((pair? template)
            `(,%cons ,(process-template (car template) dim el? env)
                     ,(process-template (cdr template) dim el? env)))
           ((vector? template)
            `(,%list->vector
              ,(process-template (vector->list template) dim el? env)))
           (else
            `(,%quote ,template))))

   ;; Return an association list of (var . dim)

   (define (meta-variables pattern dim el? vars seen-segment?)
     (cond ((symbol? pattern)
            (if (or (memq pattern subkeywords) (underscore? pattern))
                vars
                (cons (cons pattern dim) vars)))
           ((segment-pattern? pattern seen-segment? el?)
            (meta-variables (car pattern) (+ dim 1) el?
                            (meta-variables (cddr pattern) dim el? vars #t) #f))
           ((pair? pattern)
            (meta-variables (car pattern) dim el?
                            (meta-variables (cdr pattern) dim el? vars #f) #f))
           ((vector? pattern)
            (meta-variables (vector->list pattern) dim el? vars #f))
           (else vars)))

   ;; Return a list of meta-variables of given higher dim

   (define (free-meta-variables template dim el? env free)
     (cond ((symbol? template)
            (if (and (not (memq template free))
                     (let ((probe (assq template env)))
                       (and probe (>= (cdr probe) dim))))
                (cons template free)
                free))
           ((segment-template? template el?)
            (free-meta-variables (car template)
                                 dim el? env
                                 (free-meta-variables (cddr template)
                                                      dim el? env free)))
           ((pair? template)
            (free-meta-variables (car template)
                                 dim el? env
                                 (free-meta-variables (cdr template)
                                                      dim el? env free)))
           ((vector? template)
            (free-meta-variables (vector->list template) dim el? env free))
           (else free)))

   (define (ellipsis-escaped-pattern? pattern el?)
     (and (pair? pattern) (el? (car pattern))))
  
   (define (segment-pattern? p seen-segment? el?)
     (and (segment-template? p el?)
          (cond
           (seen-segment?
            (##sys#syntax-error-hook "Only one segment per level is allowed" p))
           ((not (list? p))             ; Improper list
            (##sys#syntax-error-hook "Cannot combine dotted tail and ellipsis" p))
           (else #t))))

   (define (segment-template? pattern el?)
     (and (pair? pattern)
          (pair? (cdr pattern))
          (el? (cadr pattern))))

   ;; Count the number of `...'s in PATTERN.

   (define (segment-depth pattern el?)
     (if (segment-template? pattern el?)
         (+ 1 (segment-depth (cdr pattern) el?))
         0))

   ;; Get whatever is after the `...'s in PATTERN.

   (define (segment-tail pattern el?)
     (let loop ((pattern (cdr pattern)))
       (if (and (pair? pattern)
                (el? (car pattern)))
           (loop (cdr pattern))
           pattern)))

   (make-transformer rules)))
