;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                              >>> railway/components/light.rkt <<<                              ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide light%)

(define light%
  (class object%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad light
    ;
    ; @param add-to-log-callback :: function to add a string to the log callback
    ; @param id :: the name of the railroad light
    ; @param connection :: lower-level implementation of railroad light
    ; @param segment :: segment over which the light goes
    ;
    (init-field add-to-log id connection segment)
    (init-field (signal (send connection get-signal id)))
    (init-field (prev-signal (send connection get-signal id)))

    ; set connection signal to same signal as this signal
    (define startup-time 1)
    (thread (λ () (sleep startup-time) (send connection set-sign-code! id signal)))

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
    ; add-to-log :: add a message of type string to the log
    ;
    ; @param string :: the string to be added to the log
    ;
    (define log-event (add-to-log (string-append "Light '" (symbol->string id)))) ; curryied

    ;
    ; get-id :: get the id of the railway light
    ;
    ; @returns symbol :: id-field of light
    ;
    (define/public (get-id) id)

    ;
    ; get-signals :: get list of possible signals of the light
    ;
    ; @returns list :: list of possible signals
    ;
    (define/public (get-signals) (list Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))

    ;
    ; get-signal :: get the signal of the railway light
    ;
    ; @returns symbol :: signal-field of light
    ;
    (define/public (get-signal) signal)

    ;
    ; get-prev-signal :: get the previous signal of the railway light, gets updated by set-signal!
    (define/public (get-prev-signal) prev-signal)

    ;
    ; get-segment :: get the id of the segment on which the light is
    ;
    ; @returns symbol :: id of segment
    ;
    (define/public (get-segment) segment)

    ;
    ; set-signal! :: change the signal of the railway light only when it is
    ;                not yet on the required signal
    ;
    ; @param new-signal symbol :: the new signal of the light
    ;
    (define/public (set-signal! new-signal)
      (cond ((eq? new-signal signal)
             (void))
            ((member new-signal
                  (list Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))
             (send connection set-sign-code! id new-signal)
             (set! prev-signal signal)
             (set! signal new-signal)
             (log-event "Method set-signal! called"
                        (string-append "changed signal to '" (symbol->string new-signal))))
            (else (error "light%: wrong message sent: " new-signal))))
    ))
