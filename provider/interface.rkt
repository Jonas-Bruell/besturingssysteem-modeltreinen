;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                 >>> provider/interface.rkt <<<                                 ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "../railway/interface.rkt")
(provide provider%)

(define provider%
  (class railway% ; INHERITED FUNCTIONS!
    (inherit-field connection add-to-log add-to-update)
    (init-field stop-provider)
    (super-new)

    (define/override (stop)
      (super stop)
      (stop-provider))

    (define/public (send-infrabel x)
      (send connection send-infrabel x))

    #|provider%|#))