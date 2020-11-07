;;;; srfi-45.scm
;;;; Kon Lovett, May '09

;; Issues
;;
;; - All operations inlined & primitive due to high-performance nature.
;;
;; - This has been heavily modified from the original in order to extend
;; rather than supplant the R5RS 'delay' and to allow multiple value return.
;;
;; - See `+lazy-strict+' & r5rs usage for potentially dangerous code.

;;; Module srfi-45

(module srfi-45

  (;export
    ;SRFI 45
    (lazy *make-lazy-promise)
    (eager *make-eager-promise)
    delay
    promise?
    force
    ;Extras
    lazy-promise?
    eager-promise?
    recursive-promise?
    lazy-strict)

  (import
    (rename scheme (force r5rs:force) (delay r5rs:delay))
    (rename chicken (promise? r5rs:promise?))
    (only extras fprintf))

  (require-extension type-errors)

  (include "chicken-primitive-object-inlines")

;; Utilities

(define-inline (%length=1 ls) (and (not (%null? ls)) (%null? (%cdr ls))))

;; Use SRFI 45 strict semantics for lazy promise

(define +lazy-strict?+ #t)

;; Optional promise state constraint checking

(define-syntax paranoid
  (syntax-rules ()
    ((paranoid ?expr0 ...)
      (cond-expand
        (srfi-45-paranoia (begin ?expr0 ...))
        (else             (begin)))) ) )

;; Recursive promise

(define-inline (%make-promise-box tag val) (cons tag val))
(define-inline (%maybe-promise-box? obj) (%pair? obj))
(define-inline (%promise-box-tag prmbox) (%car prmbox))
(define-inline (%promise-box-tag-set! prmbox tag) (%set-car!/mutate prmbox tag))
(define-inline (%promise-box-value prmbox) (%cdr prmbox))
(define-inline (%promise-box-value-set! prmbox val) (%set-cdr! prmbox val))

(define-inline (%eager-promise-box? obj)
  (and (%maybe-promise-box? obj)
       (%eq? 'eager (%promise-box-tag obj))) )
(define-inline (%lazy-promise-box? obj)
  (and (%maybe-promise-box? obj)
       (%eq? 'lazy (%promise-box-tag obj))) )
(define-inline (%r5rs-promise-box? obj)
  (and (%maybe-promise-box? obj)
       (%eq? 'r5rs (%promise-box-tag obj))) )

(define-inline (%promise-box? obj)
  (and (%maybe-promise-box? obj)
       (memq (%promise-box-tag obj) '(r5rs eager lazy))) )

(define-inline (%make-recursive-promise tag val)
  (%make-structure 'recursive-promise (%make-promise-box tag val)) )
(define-inline (%recursive-promise? obj) (%structure-instance? obj 'recursive-promise))
(define-inline (%promise-content prm) (%structure-ref prm 1))
(define-inline (%promise-content-set! prm prmbox) (%structure-set! prm 1 prmbox))

(define-inline (%make-eager-promise val) (%make-recursive-promise 'eager val))
(define-inline (%eager-promise? obj)
  (and (%recursive-promise? obj)
       (%eager-promise-box? (%promise-content obj))) )

(define-inline (%make-lazy-promise val) (%make-recursive-promise 'lazy val))
(define-inline (%lazy-promise? obj)
  (and (%recursive-promise? obj)
       (%lazy-promise-box? (%promise-content obj))))

(define-inline (%coerce-eager-promise-box promise-box results)
  #;(assert (%lazy-promise-box? promise-box))
  #;(assert (list? results))
  (%promise-box-tag-set! promise-box 'eager)
  (%promise-box-value-set! promise-box results) )

(define-inline (%coerce-r5rs-promise-box promise-box promise)
  #;(assert (%lazy-promise-box? promise-box))
  #;(assert (r5rs:promise? promise))
  (%promise-box-tag-set! promise-box 'r5rs)
  (%promise-box-value-set! promise-box promise) )

;;

(define (*make-lazy-promise thunk) (%make-lazy-promise thunk))
(define (*make-eager-promise thunk) (%make-eager-promise (call-with-values thunk list)))

;; Constructors

(define-syntax lazy
  (syntax-rules ()
    ((_ ?expr) (*make-lazy-promise (lambda () ?expr)))))

(define-syntax eager
  (syntax-rules ()
    ((_ ?expr) (*make-eager-promise (lambda () ?expr)))))

(define-syntax delay
  (syntax-rules ()
    ((_ ?expr) (lazy (eager ?expr)))))

;; Predicates

(define (lazy-promise? obj) (%lazy-promise? obj))
(define (eager-promise? obj) (%eager-promise? obj))
(define (recursive-promise? obj) (%recursive-promise? obj))

(define (promise? obj) (or (r5rs:promise? obj) (%recursive-promise? obj)))

;; Use SRFI 45 strict semantics for lazy promise

(define (lazy-strict . args)
  (if (null? args) +lazy-strict?+
    (set! +lazy-strict?+ (%->boolean (car args))) ) )

;; What kinda promise

(define-record-printer (recursive-promise obj out)
  (display "#<" out)
  (let ((content (%promise-content obj)))
    (cond
      ((%eager-promise-box? content)  (display "eager promise" out))
      ((%lazy-promise-box? content)   (display "lazy promise" out))
      ;This shouldn't be visible
      ((%r5rs-promise-box? content)   (display "r5rs promise" out))
      (else
        (fprintf out "unknown promise ~s" content)) ) )
  (display ">" out) )

;; Force

(define (force promise)
  ;What kind of promise?
  (cond
    ;New fashion promise?
    ((%recursive-promise? promise)
      ;Unbox
      (let ((content (%promise-content promise)))
        (paranoid
          (unless (%maybe-promise-box? content)
            (signal-type-error 'force
              "[1] not a promise-box" content) ) )
        ;Process by kind
        (let ((value (%promise-box-value content)))
          (case (%promise-box-tag content)
            ;Hack to allow lazy to have an R5RS promise
            ((r5rs)
              (paranoid
                (unless (r5rs:promise? value)
                  (signal-type-error 'force
                    "[2] not a R5RS promise" value) ) )
              (r5rs:force value) )
            ;Eager has value ready
            ((eager)
              (paranoid
                (unless (%pair? value)
                  (signal-type-error 'force
                    "[3] not an eager promise value" value) ) )
              (apply values value) )
            ;Force a lazy promise's value
            ((lazy)
              (paranoid
                ;Must be an un-forced value (i.e. still a thunk)
                (unless (%procedure? value)
                  (signal-type-error 'force
                    "[4] not a lazy promise thunk" value) ) )
              ;Force the promise by invoking the thunk
              (let ((results (call-with-values value list)))
                (paranoid
                  ;Still a valid state?
                  (unless (%recursive-promise? promise)
                    (signal-type-error 'force
                      "[5] not a promise" promise) ) )
                ;Re-fetch the top promise in case it was "forced"
                (let ((content (%promise-content promise)))
                  (paranoid
                    ;Still a valid state?
                    (unless (%promise-box? content)
                      (signal-type-error 'force
                        "[6] not a promise-box" content) ) )
                  ;Check for proper use
                  (if (not (%length=1 results))
                    ;then `lazy' used improperly
                    (if +lazy-strict?+
                      (error 'force "improper use of `lazy'" results)
                      ;Ignore misuse - What was lazy is now eager
                      (%coerce-eager-promise-box content results) )
                    ;else should be promise
                    (let ((promise* (%car results)))
                      (cond
                        ;Per SRFI 45 only valid state
                        ((%recursive-promise? promise*)
                          (if (%lazy-promise-box? content)
                            ;then copy the promise to the top
                            (let ((content* (%promise-content promise*)))
                              (paranoid
                                (unless (%promise-box? content*)
                                  (signal-type-error 'force
                                    "[7] not a promise-box" content*) ) )
                              (%promise-box-tag-set! content (%promise-box-tag content*))
                              (%promise-box-value-set! content (%promise-box-value content*))
                              (%promise-content-set! promise* content) )
                            (paranoid
                              (unless (%eager-promise-box? content)
                                (signal-type-error 'force
                                  "[8] not an eager promise" promise))
                              (unless (%eager-promise-box? (%promise-content promise*))
                                (signal-type-error 'force
                                  "[9] not an eager promise" promise*)) ) ) )
                        ;This is a hack & 1/2
                        ((r5rs:promise? promise*)
                          (if (%lazy-promise-box? content)
                            (%coerce-r5rs-promise-box content promise*)
                            (paranoid
                              ;FIXME Only possiblility is lazy
                              (unless (%eager-promise-box? content)
                                (signal-type-error 'force
                                  "[10] not an eager promise" promise)) ) ) )
                        ;So `lazy' used improperly
                        (else
                          (if +lazy-strict?+
                            (error 'force "improper use of `lazy'" promise*)
                            ;Ignore misuse - What was lazy is now eager
                            (%coerce-eager-promise-box content results) ) ) ) ) ) ) )
              ;Recursive forcing - remember
              (force promise) )
            ;Not a proper recursive promise
            (else
              (signal-type-error 'force
                "unknown recursive promise content" content) ) ) ) ) )
    ;Old fashion promise?
    ((r5rs:promise? promise)
      (r5rs:force promise) )
    ;Not a promise at all
    ;Return object per the Chicken manual
    (else
      promise ) ) )

#; ;Essentially the reference implmenentation
(define (force promise)
  (cond
    ((%recursive-promise? promise)
      (let ((content (%promise-content promise)))
        (unless (%maybe-promise-box? content)
          (signal-type-error 'force "not a promise-box" content) )
        (let ((value (%promise-box-value content)))
          (case (%promise-box-tag content)
            ((eager)
              (apply values value))
            ((lazy)
              ;Assumes lazy can only be applied to an expression yielding
              ;another promise
              (let* ((promise* (value))
                     ;Re-fetch the top promise in case it was "forced"
                     (content  (%promise-content promise)))
                (unless (%eager-promise-box? content)
                  (let ((content* (%promise-content promise*)))
                    (%promise-box-tag-set! content (%promise-box-tag content*))
                    (%promise-box-value-set! content (%promise-box-value content*)) )
                  (%promise-content-set! promise* content) ) )
              (force promise) ) ) ) ) )
    ;Old fashion promise?
    ((r5rs:promise? promise)
      (r5rs:force promise) )
    ;Not a promise at all
    ;Return object per the Chicken manual
    (else
      promise ) ) )

;;;

(register-feature! 'srfi-45)

) ;module srfi-45

#|
Copyright (C) André van Tonder (2003). All Rights Reserved.


Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:


The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.


THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#
