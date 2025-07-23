;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                           >>> railway/components/switch-cross.rkt <<<                          ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "switch.rkt")

(provide switch-cross%)

(define switch-cross%
  (class switch%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad switch
    ;
    ; @param id :: the name of the railroad switch
    ; @param connection :: lower-level implementation of railroad switch
    ; @param in :: (in-left . in-right) railway element that goes into this
    ;              segment (clockwise)
    ; @param out :: (out-left . out-right), cons-cell of railway element that
    ;               exits out of this segment (clockwise)
    ;
    (inherit-field id connection in out)
    (inherit-field state position)

    ;
    ; Possible railway switch positions
    ;
    (define in-left   'in-left)
    (define in-right  'in-right)
    (define out-left  'out-left)
    (define out-right 'out-right)

    ;
    ; get-prev-left :: get the previous railway element, in the direction of the
    ;                  2-ended side, on the left.
    ;
    ; @returns symbol :: left in-field of switch
    ;
    (define/public (get-prev-left) (car in))
    
    ;
    ; get-prev-right :: get the previous railway element, in the direction of
    ;                   the 2-ended side, on the right.
    ;
    ; @returns symbol :: right in-field of switch
    ;
    (define/public (get-prev-right) (cdr in))

    ; set-position! :: change the position of the railway switch only when it is
    ;                  not yet in the required state
    ;
    ; @param new-position symbol :: the new position of the switch
    ;
    (define/override (set-position! new-position)
       (cond ((eq? new-position position)
             (void))
            ((member new-position (list in-left in-right out-left out-right))
             (send connection set-position! new-position)
             (set! position (send connection get-position)))
            (else (error "switch%: wrong message sent: " new-position))))
    ))