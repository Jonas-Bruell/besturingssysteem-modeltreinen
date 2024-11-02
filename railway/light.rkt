;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              >>> light.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide light%)

(define light%
  (class object%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad light
    ;
    ; @param id :: the name of the railroad light
    ; @param connection :: lower-level implementation of railroad light
    ; @param segment :: segment over which the light goes
    ;
    (init-field id connection segment)
    (field (signal (send connection get-signal)))

    ;
    ; Possible railway light signals
    ;
    (define Hp0          'Hp0)
    (define Hp1          'Hp1)
    (define Hp0+Sh0      'Hp0+Sh0)
    (define Ks1+Zs3      'Ks1+Zs3)
    (define Ks2          'Ks2)
    (define Ks2+Zs3      'Ks2+Zs3)
    (define Sh1          'Sh1)
    (define Ks1+Zs3+Zs3v 'Ks1+Zs3+Zs3v)

    ;
    ; get-id :: get the id of the railway light
    ;
    ; @returns symbol :: id-field of light
    ;
    (define/public (get-id) id)

    ;
    ; get-signal :: get the signal of the railway light
    ;
    ; @returns symbol :: signal-field of light
    ;
    (define/public (get-signal) signal)

    ;
    ; get-segment :: get the id of the segment on which the light is
    ;
    ; @returns symbol :: id of segment
    ;
    (define/public (get-segment) segment)

    ;
    ; set-signal! :: change the signal of the railway light only when it is
    ; not yet on the required signal
    ;
    ; @param new-signal :: the new signal of the light
    ;
    (define/public (set-signal! new-signal)
      (cond ((eq? new-signal signal)
             (void))
            ((member new-signal
                  (list Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))
             (send connection set-signal! new-signal)
             (set! signal (send connection get-signal)))
            (else (error "light%: wrong message sent: " new-signal))))
    ))
