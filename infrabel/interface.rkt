;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                 >>> infrabel/interface.rkt <<<                                 ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "../railway/interface.rkt"
         "logic/conductor.rkt")
(provide infrabel%)

(define infrabel%
  (class railway% ; INHERITED FUNCTIONS!
    (inherit-field add-to-log add-to-update)
    (init-field server)
    (super-new)

    (define conductors '())

    (define/public (add-conductor-to-train train-object provider-name conductor-name)
      (define conductor-object (new conductor% (train-object train-object)
                             (provider-name provider-name) (add-to-update add-to-update)))
      (set! conductors (append conductors (cons conductor-name conductor-object))))

    (define (get-conductor conductor-name)
      (cdr (assoc conductors conductor-name)))

    (define/public (instruct-conductor-follow-route conductor-name route)
      (send (get-conductor conductor-name) follow-route route))

    
    #| </infrabel%> |#))