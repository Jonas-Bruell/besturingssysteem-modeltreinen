;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> train.rkt <<<                             ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide train%) ; (object symbol symbol symbol -> class)

(define train%
  (class object%
    (super-new)
    (init-field connection id previous current)
    (send connection add-loco id previous current)
    (define/public (set-train-speed! new-speed)
      (send connection set-loco-speed! id new-speed))
    ))