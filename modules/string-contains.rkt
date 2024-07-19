#lang racket/base

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

https://gist.github.com/bennn/0772b7d59bfdb421c048

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

;; Testing 3 implementations of string-contains?
;; 1. Using a simple regular expression
;; 2. Using a for loop
;; 3. Using a let loop

;; To run the freq-test, you'll want to download the file "common-words.rktd"
;; from this gist: https://gist.github.com/bennn/7efd60b4809a475af297

;; My results (average of 30 runs)
;; 1. 8852.90 ms
;; 2. 921.77 ms
;; 3. 851.00 ms

(require
  (only-in racket/file file->value)
  (only-in racket/format ~r)
)

(provide string-contains?)

#|
(define (string-contains1 str sub)
  (regexp-match? (regexp sub) str))

(define (string-contains2 str sub)
  (define L1 (string-length str))
  (define L2 (string-length sub))
  (for/or ([start (in-range (add1 (- L1 L2)))])
    (for/and ([i (in-range L2)])
      (char=? (string-ref str (+ i start))
              (string-ref sub i)))))
|#

(define (string-contains? str sub)
  (define L1 (string-length str))
  (define L2 (string-length sub))
  (define d (- L1 L2))
  (or (zero? L2)
    (let loop ([start 0])
      (and (<= start d)
           (or (let loop2 ([offset 0])
                 (or (= offset L2)
                     (and (char=? (string-ref str (+ start offset))
                                  (string-ref sub offset))
                          (loop2 (add1 offset)))))
               (loop (add1 start)))))))

;; =============================================================================

#|

(define (simple-test)
  (define test-counter (box 0))
  (define-syntax-rule (incr x) (set-box! x (add1 (unbox x))))
  (define (check-true x)
    (unless x (error (format "test ~a failed" (unbox test-counter))))
    (incr test-counter))
  (define (check-false x)
    (when x (error (format "test ~a failed" (unbox test-counter))))
    (incr test-counter))

  (for ([string-contains?
         (in-list (list string-contains1 string-contains2 string-contains3))])
    (check-true  (string-contains? "racket" "racket"))
    (check-true  (string-contains? "racket" "cket"))
    (check-true  (string-contains? "racket" "acke"))
    (check-true  (string-contains? "racket" "t"))
    (check-true  (string-contains? "racket" ""))
    (check-false (string-contains? "racket" "b"))
    (check-false (string-contains? "racket" "R"))
    (check-false (string-contains? "racket" "kc"))
    (check-false (string-contains? "racket" "racketr")))

  (printf "~a tests passed\n" (unbox test-counter))
)

(define (mean val*)
  (define-values (s l)
    (for/fold ([s 0] [l 0]) ([v (in-list val*)]) (values (+ s v) (add1 l))))
  (/ s l))

(define-syntax-rule (benchmark expr)
  (begin
    (let* ([num-runs 30]
           [times (for/list ([i (in-range num-runs)])
                    (let-values ([(res cpu-time real-time gc-time)
                                  (time-apply (lambda () expr) '())])
                      cpu-time))]
           [avg (~r (mean times) #:precision 2)])
    (printf "Average of ~a runs: ~a ms\n" num-runs avg))))

(define (freq-test)
  (define fname "common-words.rktd")
  (define w* (file->value fname))
  (define-syntax-rule (run-test f)
    (begin
      (printf "Running `~a` on all pairs of words in '~a'\n"
              (object-name f) fname)
      (benchmark (for* ([w1 (in-list w*)] [w2 (in-list w*)])
        (f w1 w2)))))
  ;; -- for-loop impl.
  (for ([f (in-list (list string-contains2
                          string-contains3
                          ))])
    (run-test f)))

;(simple-test)
;(freq-test)

|#