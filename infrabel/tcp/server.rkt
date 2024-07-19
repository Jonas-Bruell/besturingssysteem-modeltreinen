#lang racket

(require racket/tcp)
(provide start-server)

(define (handle in out)
  ;(displayln (read-line in 'return-linefeed))
  (write-string "Hello World!" out)
  (flush-output out))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (displayln in)
  (displayln out)
  (thread
   (λ ()
     (handle in out)
     (close-input-port in)
     (close-output-port out))))

(define clients '())

(define (update)
  (displayln "update")
  (car clients))

(define (start-server host port)
  (displayln "starting server")
  (define listener (tcp-listen port 5 #t host))

  
  (define (reading-loop)
    (displayln "serving")
    (accept-and-handle listener)
    (reading-loop))

  
  (define t (thread reading-loop))

  #|
  (λ ()
    (displayln "stopping server")
    (kill-thread t)
    (tcp-close listener))
|#

  (let loop ()
    (λ (input . msg)
      (displayln input)
      (case input
        ('stop (displayln "stopping server")
               (kill-thread t)
               (tcp-close listener))
        ('restart (displayln "restarting server")
                  (set! listener (tcp-listen port 5 #t "localhost"))
                  (set! t (thread reading-loop)))
        ('update (update))
        ;('send (displayln "sending")
        ;       (write-string (car msg) out)
        ;       (flush-output out)
        ;       (loop))
        (else (displayln "message does not exist")
              (loop))
        )))


  )

