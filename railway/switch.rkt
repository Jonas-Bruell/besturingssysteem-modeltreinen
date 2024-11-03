;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> switch.rkt <<<                             ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "segment.rkt")
(provide switch%)

(define switch%
  (class segment%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad switch
    ;
    ; @param id :: the name of the railroad switch
    ; @param connection :: lower-level implementation of railroad switch
    ; @param in :: railway element that goes into this segment (clockwise)
    ; @param out :: (left . right), cons-cell of railway element that exits out
    ;               of this segment (clockwise)
    ;
    (inherit-field id connection in out)
    (inherit-field state)
    (field (position (send connection get-position)))

    ;
    ; Possible railway switch positions
    ;
    (define left 'left)
    (define right 'right)

    ;
    ; get-next-left :: get the next railway element, in the direction of the
    ;                  2-ended side, on the left.
    ;
    ; @returns symbol :: left out-field of switch
    ;
    (define/public (get-next-left) (car out))
    
    ;
    ; get-next-right :: get the next railway element, in the direction of the
    ;                   2-ended side, on the right.
    ;
    ; @returns symbol :: right out-field of switch
    ;
    (define/public (get-next-right) (cdr out))

    ;
    ; get-position :: get the position of the switch
    ;                 represented by left or right
    ;
    ; @returns symbol :: position-field of switch
    ;
    (define/public (get-position) position)

    ; set-position! :: change the position of the railway switch only when it is
    ;                  not yet in the required state
    ;
    ; @param new-position symbol :: the new position of the switch
    ;
    (define/public (set-position! new-position)
       (cond ((eq? new-position position)
             (void))
            ((member new-position (list left right))
             (send connection set-position! new-position)
             (set! position (send connection get-position)))
            (else (error "switch%: wrong message sent: " new-position))))
    ))