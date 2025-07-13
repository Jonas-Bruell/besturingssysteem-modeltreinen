#lang racket

(provide infrabel-server%)

;; SERVER (listens for request)
(define infrabel-server%
  (class object%
    (super-new)
    (init-field host port)
    
    (define infrabel #f)
    (define the-listener (tcp-listen (string->number port) 4 #t host))

    (define/public (init! infrab)
      (set! infrabel infrab))

    (define/public (stop)
      (displayln "stop"))

    (thread
     (lambda ()
    (let loop ()
      (define-values (in out) (tcp-accept the-listener))
      ; new thread per client
      (thread 
       (lambda ()

         (define (print)
           (displayln (read in)))
     
         (displayln "new client connected")
         (write "welcome" out)
         (flush-output out)

         (let/ec return
           (let loop ()
             (define msg (read in))
             (displayln msg)
             (cond ((string=? msg "Ping")
                    (write "Pong" out) (flush-output out))
                   ((string=? msg "Hello?")
                    (write "World!" out) (flush-output out))
                   ((string=? msg "Test")
                    (write "Performing test :)" out) (flush-output out)
                    (send infrabel set-crossing-position! 'C-1 'closed))
                   ((string=? msg "close")
                    (write "closed" out) (flush-output out) (return))
                   (else
                    (displayln "no equal msg")))
             (loop)))

         (displayln "client disconnected")
         (close-input-port in)
         (close-output-port out)))
      (loop))))))