#lang racket

(require xml)

(define (xexpr->sexpr xexpr)
  (define (rec xexpr return)
    (let ((head (car xexpr))
          (tail (cdr xexpr)))
      (cond ((and (null? tail)
                  (string? head)
                  (not (string-contains? head "\r\n")))
             (append return head))
            ((null? tail)
             return)
            ((or (null? head)
                 (and (string? head) (string-contains? head "\r\n")))
             (rec tail return))
            ((and (list? head) (null? (cadr head)))             
             (rec tail (append return
                               (list (list (car head) (rec (cdr head) '()))))))
            (else
             (rec tail (append return (list head)))))))
  (rec xexpr '()))

(define xml
  (xexpr->sexpr
   (xml->xexpr
    (document-element
     (read-xml
      (open-input-file "./routes/hardware.xml"))))))

xml