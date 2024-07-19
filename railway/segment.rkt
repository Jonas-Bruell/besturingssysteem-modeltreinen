;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> segment.rkt <<<                            ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide segment%) ; (object symbol symbol -> class)

(define segment%
  (class object%
    (super-new)
    (init-field connection id state)
    (define/public (get-state)
      state)
    (define/public (reserve)
      (set! state 'reserved))
    (define/public (free)
      (set! state 'free))
    ))