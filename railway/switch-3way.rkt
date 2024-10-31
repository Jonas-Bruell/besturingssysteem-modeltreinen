;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> switch.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide switch-3way%) ; (object symbol symbol -> class)

(define switch-3way%
  (class object%
    (super-new)
    (init-field connection id position)
    (define/public (get-position) position)
    (define/public (set-position! new-position)
      (set! position new-position)
      (if (member new-position '(1 2))
        (send connection set-switch-position! id new-position)
        (error "switch%: wrong message sent: " new-position)))
    ))