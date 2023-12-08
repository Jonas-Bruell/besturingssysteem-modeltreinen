;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> switch.rkt <<<                             ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide switch%) ; (object symbol symbol -> class)

(define switch%
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