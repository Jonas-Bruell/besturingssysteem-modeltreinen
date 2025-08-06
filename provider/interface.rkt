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
  (class railway% ; INHERITED FUNCTION!
    (inherit-field add-to-log add-to-update)
    (init-field stop-provider client)
    (super-new)

    (define/override (stop)
      (super stop)
      (stop-provider))

    (define/public (send-client x)
      (send client x))

    #|provider%|#))