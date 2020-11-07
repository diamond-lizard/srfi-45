;;;; srfi-45 test

(module test ()

(import
  (rename scheme (force r5rs:force) (delay r5rs:delay))
  (rename chicken (promise? r5rs:promise?)))

(require-extension srfi-45)

;; Perform, or not, a bounded space test.
;; The infinite tests are not performed by default.

(define-syntax +bounded-space
  (syntax-rules (force)
    ((_ (force ?expr))
      (begin
        (print "+++ Bounded Space Test: (force " '?expr ") +++")
        (force ?expr) ) ) ) )

(define-syntax -bounded-space
  (syntax-rules (force)
    ((_ (force ?expr))
      (print "+++ Skipping Bounded Space Test: (force " '?expr ") +++") ) ) )

;=========================================================================
; TESTS AND BENCHMARKS:
;=========================================================================

;=========================================================================
; R5RS & SRFI-45 test 1:

(print "+++ Should print 'hi 1 +++")

(define r (r5rs:delay (begin (display 'hi) (display #\space) 1)))
(define s (lazy r))
(define t (lazy s))
(print (force t))

;=========================================================================
; Multiple values test 1:

(print "+++ Should print '(1 2 3) +++")

(define r (delay (values 1 2 3)))
(define s (lazy r))
(define t (lazy s))
(print (receive (force t)))

;=========================================================================
; Memoization test 1:

(print "+++ Should print 'hello once +++")

(define s (delay (begin (print 'hello) 1)))

(force s)
(force s)

;=========================================================================
; Memoization test 2:

(print "+++ Should print 'bonjour once +++")

(let ((s (delay (begin (print 'bonjour) 2))))
  (+ (force s) (force s)))

;=========================================================================
; Memoization test 3: (pointed out by Alejandro Forero Cuervo)

(print "+++ Should print 'hi once +++")

(define r (delay (begin (print 'hi) 1)))
(define s (lazy r))
(define t (lazy s))

(force t)
(force r)

;=========================================================================
; Memoization test 4: Stream memoization

(print "+++ Should print 'ho five times +++")

(define (stream-drop s index)
  (lazy (if (zero? index) s
            (stream-drop (cdr (force s)) (- index 1)))))

(define (ones)
  (delay (begin
           (print 'ho)
           (cons 1 (ones)))))

(define s (ones))

(car (force (stream-drop s 4)))
(car (force (stream-drop s 4)))

;=========================================================================
; Reentrancy test 1: from R5RS

(print "+++ Should print 6 twice +++")

(define count 0)
(define p
  (delay (begin
           (set! count (+ count 1))
           (if (> count x) count
               (force p)))))
(define x 5)
(print (force p))
(set! x 10)
(print (force p))


;=========================================================================
; Reentrancy test 2: from SRFI 40

(print "+++ Should print 'second once +++")

(define f
  (let ((first? #t))
    (delay (if (not first?) 'second
               (begin
                 (set! first? #f)
                 (force f))))))

(print (force f))

;=========================================================================
; Reentrancy test 3: due to John Shutt

(print "+++ Should print 5 0 10 +++")

(define q
  (let ((count 5))
    (define (get-count) count)
    (define p (delay (if (<= count 0) count
                         (begin
                           (set! count (- count 1))
                           (force p)
                           (set! count (+ count 2))
                           count))))
    (list get-count p)))

(define get-count (car q))
(define p (cadr q))

(print (get-count))
(print (force p))
(print (get-count))

;=========================================================================
; Test leaks:  All the leak tests should run in bounded space.

;=========================================================================
; Leak test 1: Infinite loop in bounded space.

(define (loop) (lazy (loop)))

(-bounded-space (force (loop)))

;=========================================================================
; Leak test 2: Pending memos should not accumulate
;              in shared structures.

(define s (loop))

(-bounded-space (force s))

;=========================================================================
; Leak test 3: Safely traversing infinite stream.

(define (from n)
  (delay (cons n (from (+ n 1)))))

(define (traverse s)
  (lazy (traverse (cdr (force s)))))

(-bounded-space (force (traverse (from 0))))

;=========================================================================
; Leak test 4: Safely traversing infinite stream
;              while pointer to head of result exists.

(define s (traverse (from 0)))

(-bounded-space (force s))

;=========================================================================
; Convenient list deconstructor used below.

(define-syntax test:match
  (syntax-rules ()
    ((test:match exp
       (()      exp1)
       ((h . t) exp2))
     (let ((lst exp))
       (cond ((null? lst)
               exp1)
             ((pair? lst)
              (let ((h (car lst))
                    (t (cdr lst)))
                exp2))
             (else
              'test:match-error))))))

;========================================================================
; Leak test 5: Naive stream-filter should run in bounded space.
;              Simplest case.

(define (stream-filter p? s)
  (lazy (test:match (force s)
          (()
           (delay '()))
          ((h . t)
           (if (p? h) (delay (cons h (stream-filter p? t)))
               (stream-filter p? t))))))

(+bounded-space (force (stream-filter (lambda (n) (= n 100000 #;10000000000)) (from 0))))

;========================================================================
; Leak test 6: Another long traversal should run in bounded space.

; The stream-ref procedure below does not strictly need to be lazy.
; It is defined lazy for the purpose of testing safe compostion of
; lazy procedures in the times3 benchmark below (previous
; candidate solutions had failed this).

(define (stream-ref s index)
  (lazy (test:match (force s)
          (()
           'error)
          ((h . t)
           (if (zero? index) (delay h)
               (stream-ref t (- index 1)))))))

; Check that evenness is correctly implemented - should terminate:

(print "+++ Should print 0 +++")

(print (force (stream-ref (stream-filter zero? (from 0)) 0)))

(define s (stream-ref (from 0) 10000 #;100000000))

(+bounded-space (force s))

;======================================================================
; Leak test 7: Infamous example from SRFI 40.

(define (times3 n)
  (stream-ref (stream-filter (lambda (x) (zero? (modulo x n))) (from 0)) 3))

(print "+++ Should print 21 +++")

(print (force (times3 7)))

(+bounded-space (force (times3 10000 #;100000000)))

)
