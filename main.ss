(define-syntax λ
  (syntax-rules ()
    [(_ param body ...)
     (lambda param body ...)]))

(define-syntax case-λ
  (syntax-rules ()
    [(_ (param body) ...)
     (case-lambda (param body) ...)]))

(define-syntax defvar
  (syntax-rules ()
    [(_ name value)
     (define name value)]))

(define-syntax defun
  (syntax-rules ()
    [(_ name param body ...)
     (define name (λ param body ...))]))

(define-syntax progn
  (syntax-rules ()
    [(_ body ...)
     (begin body ...)]))

;; (define (F g)             ; g is some func like exp.
;; (λ (x n)
;;   (cond [(= n 0) 1]
;;         [else (* x (g x (sub1 n)))])))

;; (define Y            ; (Y F) = (F (Y F))
;;   (λ (f)
;;     ((λ (x) (f (x x)))
;;      (λ (x) (f (x x))))))


(define true #t)
(define false #f)
(define nil '())
(define null '())
(define empty '())

(define (false? val) (not val))
(define (true? val) (not (false? val)))

(define (displayln datum)
  (display datum)
  (newline))

(define (/= . args) (not (apply = args)))

(define return (λ (x) x))


(define-syntax while
  (syntax-rules ()
    [(_ pred body ...)
     (let loop () (when pred body ... (loop)))]))

;; (define-syntax for
;;   (syntax-rules ()
;;     [(_ (i from to) body ...)
;;      (let loop ([i from])
;;        (when (< i to)
;;          body ...
;;          (loop (add1 i))))]))


;; (define-syntax nil!
;;   (syntax-rules ()
;;     [(_ x)
;;      (set! x '())]))

(define sqr (λ (x) (* x x)))
(define average (λ (x y) (/ (+ x y) 2)))

(define (lat? l)
  (cond
   [(null? l) #t]
   [(atom? (car l)) (lat? (cdr l))]
   [else #f]))

;; ;; set cycle.
;; (define (cycle c)
;;   (set-cdr! (last-pair c) c)
;;   c)

;; (define (cycle? c)
;;   (let loop ([m (cdr c)])
;;     (cond
;;      [(null? m) #f]
;;      [(eq? c m) #t]
;;      [else (loop (cdr m))])))


;;;; set IO

;;read file
(define read-file
  (λ (file-name)
    (let ([p (open-input-file file-name)])
      (let loop ([ls1 '()] [c (read-char p)])
        (cond
         [(eof-object? c)
          (close-input-port p)
          (list->string (reverse ls1))]
         [else
          (loop (cons c ls1) (read-char p))])))))

;; (define read-file
;;   (λ (file-name)
;;     (call-with-input-file file-name
;;       (λ (p)
;;         ;; p is the port used to input.
;;         (let loop ([ls1 '()] [c (read-char p)])
;;           ;; c is the read char from p.
;;           (cond
;;            [(eof-object? c)
;;             (close-input-port p)
;;             (list->string (reverse ls1))]
;;            [else
;;             (loop (cons c ls1) (read-char p))]))))))

;; (define read-file
;;   (λ (file-name)
;;     (with-input-from-file file-name
;;       (λ ()
;;         (let loop ([ls1 '()] [c (read-char)])
;;           (if (eof-object? c)
;;               (list->string (reverse ls1))
;;               (loop (cons c ls1) (read-char))))))))


;;;; set stream.

(define memo-proc
  (λ (proc)
    (let ([already-run? #f] [result #f])
      (λ ()
        (if (not already-run?)
            (begin (set! result (proc))
                   (set! already-run? #t)
                   result)
            result)))))

(define-syntax stream-cons
  (syntax-rules ()
    [(stream-cons x y) (cons x (delay y))]))

(define (stream-first stream) (car stream))
(define (stream-rest stream) (force (cdr stream)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-caar stream) (stream-car (stream-car stream)))
(define (stream-cadr stream) (stream-car (stream-cdr stream)))
(define (stream-cdar stream) (stream-cdr (stream-car stream)))
(define (stream-cddr stream) (stream-cdr (stream-cdr stream)))

(define (stream-caaar stream) (stream-car (stream-caar stream)))
(define (stream-caadr stream) (stream-car (stream-cadr stream)))
(define (stream-cadar stream) (stream-car (stream-cdar stream)))
(define (stream-caddr stream) (stream-car (stream-cddr stream)))
(define (stream-cdaar stream) (stream-cdr (stream-caar stream)))
(define (stream-cdadr stream) (stream-cdr (stream-cadr stream)))
(define (stream-cddar stream) (stream-cdr (stream-cdar stream)))
(define (stream-cdddr stream) (stream-cdr (stream-cddr stream)))

(define (stream-caaaar stream) (stream-car (stream-caaar stream)))
(define (stream-caaadr stream) (stream-car (stream-caadr stream)))
(define (stream-caadar stream) (stream-car (stream-cadar stream)))
(define (stream-caaddr stream) (stream-car (stream-caddr stream)))
(define (stream-cadaar stream) (stream-car (stream-cdaar stream)))
(define (stream-cadadr stream) (stream-car (stream-cdadr stream)))
(define (stream-caddar stream) (stream-car (stream-cddar stream)))
(define (stream-cadddr stream) (stream-car (stream-cdddr stream)))
(define (stream-cdaaar stream) (stream-cdr (stream-caaar stream)))
(define (stream-cdaadr stream) (stream-cdr (stream-caadr stream)))
(define (stream-cdadar stream) (stream-cdr (stream-cadar stream)))
(define (stream-cdaddr stream) (stream-cdr (stream-caddr stream)))
(define (stream-cddaar stream) (stream-cdr (stream-cdaar stream)))
(define (stream-cddadr stream) (stream-cdr (stream-cdadr stream)))
(define (stream-cdddar stream) (stream-cdr (stream-cddar stream)))
(define (stream-cddddr stream) (stream-cdr (stream-cdddr stream)))



(define empty-stream '())
(define stream-empty? null?)

(define stream-append
  (λ (s1 s2)
    (if (stream-empty? s1)
        s2
        (stream-cons (stream-car s1)
                     (stream-append (stream-cdr s1) s2)))))

(define stream-ref
  (λ (s n)
    ;; s is a stream, get its nth cell.
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (sub1 n)))))

(define print-stream
  (λ (s)
    (cond [(stream-empty? s)
           (newline)
           "done"]
          [else (display (stream-car s))
                (display " ")
                (print-stream (stream-cdr s))])))


(define stream-map
  (λ (proc s)
    ;; s is a stream, and proc is called by all cells of s.
    (if (stream-empty? s)
        empty-stream
        (stream-cons
         (proc (stream-car s))
         (stream-map proc (stream-cdr s))))))

(define stream-filter
  (λ (pred s)
    ;; s is a stream, and pred is used to judge and choose the cells of s.
    (cond
     [(stream-empty? s) empty-stream]
     [(pred (stream-car s))
      (stream-cons (stream-car s)
                   (filter pred (stream-cdr s)))]
     [else (filter pred (stream-cdr s))])))

(define stream-accumulate
  (λ (combiner init-val s)
    ;; s is a stream. deal with all cells in the s.
    ;; combiner is the function about how to deal with s.
    (if (stream-empty? s)
        init-val
        (combiner (stream-car s)
                  (stream-accumulate combiner
                                     init-val
                                     (stream-cdr s))))))

(define enumerate-tree
  (λ (tree)
    (if (leaf-node? tree)
        (stream-cons tree
                     empty-stream)
        (stream-append
         (enumerate-tree
          (left-branch tree))
         (enumerate-tree
          (right-branch tree)))))
  )

(define enumerate-interval
  (λ (low high)
    (if (> low high)
        empty-stream
        (stream-cons
         low
         (enumerate-interval (add1 low) high)))))


(define stream-scale
  (λ (c s)
    ;; s is a stream. c is a constant.
    (stream-map (λ (x) (* x c)) s)))

(define stream-add
  (λ (s1 s2)
    (cond [(stream-empty? s1) s2]
          [(stream-empty? s2) s1]
          [else
           (stream-cons
            (+ (stream-car s1) (stream-car s2))
            (stream-add (stream-cdr s1) (tail s2)))])))

(define stream-sub
  (λ (s1 s2)
    (cond [(stream-empty? s1) s2]
          [(stream-empty? s2) s1]
          [else
           (stream-cons
            (- (stream-car s1) (stream-car s2))
            (stream-sub (stream-cdr s1) (tail s2)))])))

;;;; AMB
;;; fail is called to backtrack when a condition fails.  At the top
;;; level, however, there is no more to backtrack, so we signal an
;;; error with SRFI 23.
(define fail
  (λ ()
    (error 'amb "Amb tree exhausted!")))

(define-syntax amb
  (syntax-rules ()
    [(_) (fail)]                     ; Two shortcuts.
    [(_ expression) expression]
    [(_ expression ...)
     (let ([fail-save fail])
       ((call/cc                     ; Capture a continuation to
         (λ (k-success)              ;   which we return possibles.
           (call/cc
            (λ (k-failure)           ; k-failure will try the next
              (set! fail             ;   possible expression.
                (λ () (k-failure #f)))
              (k-success             ; Note that the expression is
               (λ ()                 ;   evaluated in stream-cdr position
                 expression))))      ;   with respect to AMB.
           ...
           (set! fail fail-save)     ; Finally, if this is reached,
           fail-save))))]))          ;   we restore the saved fail.
