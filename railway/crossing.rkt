;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> crossing.rkt <<<                            ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide crossing%) ; (object symbol symbol -> class)

(define crossing%
  (class object%
    (super-new)
    (init-field connection id state)
    (define/public (get-state) state)
    (define/public (set-state! new-state)
      (set! state new-state)
      (case new-state
        ('open (send connection open-crossing! id))
        ('close (send connection close-crossing! id))
        (else (error "crossing%: wrong message sent: " new-state))))
    ))