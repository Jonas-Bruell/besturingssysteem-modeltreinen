;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             >>> infrabel.rkt <<<                           ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;
;; Description: ADT infrabel
;; Arguments: none
;; Output: none
;; Messages:
;; - message: arguments:
;;            output:
;; Extra info:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require "adt-udp-communication.rkt")

(define (make-infrabel)
  (let ()
    
    (display "")
    
    (λ (message . arguments)
      (case message
        ;(<msg> (<function>))
        ;(<msg> (apply <function> arguments))
        (else (error " " message))))))