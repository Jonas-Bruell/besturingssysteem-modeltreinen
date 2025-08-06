;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                   >>> provider/client.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide provider-client%)

;; Client: sends request
(define provider-client%
  (class object%
    (init-field host port add-to-log add-to-update)
    (super-new)

    (define-values (in out) (tcp-connect "localhost" 2020))
    (define connected? #t)

    (define/public (stop)
      (displayln "stop"))

    (define/public (ping)
      (write "Ping" out)
      (flush-output out))

    (define/public (hello?)
      (write "Hello?" out)
      (flush-output out))

    (define/public (test)
      (write "Test" out)
      (flush-output out))

    (define/public (close)
      (set! connected? #f)
      (write "close" out)
      (flush-output out)
      (close-input-port in)
      (close-output-port out))

    (thread
     (lambda ()
       (let loop ()
         (when connected?
           (displayln (read in))
           (loop)))
       (displayln "disconnected")))))