;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              >>> light.rkt <<<                             ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide light%) ; (object symbol symbol -> class)

(define light%
  (class object%
    (super-new)
    (init-field connection id signal)
    (define/public (get-signal) signal)
    (define/public (set-signal! new-signal)
      (set! signal new-signal)
      (if (member new-signal
                  '(Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))
          (send connection set-sign-code! id new-signal)
          (error "light%: wrong message sent: " new-signal)))
    ))