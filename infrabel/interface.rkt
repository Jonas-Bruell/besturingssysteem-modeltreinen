;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       >>> infrabel/interface.rkt <<<                       ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 5 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../railway/interface.rkt"
         "logic/conductor.rkt")
(provide infrabel%)

(define infrabel%
  (class railway% ; INHERITED FUNCTIONS!
    (inherit-field add-to-log add-to-update)
    (init-field server)
    (super-new)

    (define/public (add-conductor-to-train train-object)
      (void))

    
    #| </infrabel%> |#))