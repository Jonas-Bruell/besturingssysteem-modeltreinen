;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> switch.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "switch.rkt")
(provide switch-3way%)

(define switch-3way%
  (class switch%
    (super-new)
;
    ; This class represents an atomic abstraction of a railroad 3-way switch
    ;
    ; @param id :: the name of the railroad 3-way switch
    ; @param connection :: lower-level implementation of railroad 3-way switch
    ; @param in :: railway element that goes into this segment (clockwise)
    ; @param out :: (left middle right), list of railway element that exits out
    ;               of this segment (clockwise)
    ;
    (inherit-field id connection in out)
    (inherit-field state position)

    ;
    ; Possible railway switch positions
    ;
    (define left   'left)
    (define middle 'middle)
    (define right  'right)

    ;
    ; get-next-middle :: get the next railway element, in the direction of the
    ;                    2-ended side, in the middle.
    ;
    ; @returns symbol :: middle out-field of switch-3way
    ;
    (define/public (get-next-middle) (cadr out))
    
    ;
    ; get-next-right :: get the next railway element, in the direction of the
    ;                   2-ended side, on the right.
    ;
    ; @returns symbol :: right out-field of switch
    ;
    (define/override (get-next-right) (caddr out))

    ; set-position! :: change the position of the railway switch only when it is
    ;                  not yet in the required state
    ;
    ; @param new-position symbol :: the new position of the switch
    ;
    (define/override (set-position! new-position)
       (cond ((eq? new-position position)
             (void))
            ((member new-position (list left middle right))
             (send connection set-position! new-position)
             (set! position (send connection get-position)))
            (else (error "switch-3way%: wrong message sent: " new-position))))
    ))